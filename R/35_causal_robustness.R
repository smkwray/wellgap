# 35_causal_robustness.R
# More conservative wellbeing-focused causal robustness models.

if (!exists("project_root", inherits = FALSE)) {
  find_project_root <- function(start = getwd()) {
    cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
    repeat {
      if (file.exists(file.path(cur, "config", "config.yml"))) return(cur)
      parent <- dirname(cur)
      if (identical(parent, cur)) stop("Could not find project root containing config/config.yml", call. = FALSE)
      cur <- parent
    }
  }
  project_root <- find_project_root()
}

source(file.path(project_root, "R", "01_helpers.R"))

required_pkgs <- c("dplyr", "purrr", "tibble", "tidyr", "fixest", "broom")
invisible(lapply(required_pkgs, require, character.only = TRUE))

read_panel <- function() {
  panel_path <- path_project(cfg$paths$final_root, "state_year_panel.rds")
  if (!file.exists(panel_path)) stop("Missing final panel: ", panel_path, call. = FALSE)
  safe_read_rds(panel_path)
}

read_brfss_subgroups <- function() {
  path <- path_project(cfg$paths$intermediate_root, "brfss_state_year_subgroups.csv")
  if (!file.exists(path)) stop("Missing BRFSS subgroup file: ", path, call. = FALSE)
  safe_read_csv(path)
}

clean_control_set <- function(data, controls = analysis_primary_controls()) {
  controls <- controls[controls %in% names(data)]
  unique(controls)
}

minimal_control_candidates <- function() {
  cfg_vec(cfg$analysis$minimal_controls %||% c(
    "l1_log_real_mean_income",
    "l1_rpp_all_items",
    "l1_unemployment_rate",
    "l1_college_share",
    "l1_black_share",
    "l1_hispanic_share"
  ))
}

locked_control_ladder_specs <- function(data) {
  list(
    minimal = clean_control_set(data, minimal_control_candidates()),
    causal_core = clean_control_set(data, analysis_causal_core_controls()),
    rich = clean_control_set(data, analysis_rich_controls())
  )
}

wellbeing_treatments <- function(data) {
  candidates <- unique(c(
    analysis_wellbeing_primary_treatments(),
    analysis_wellbeing_secondary_treatments()
  ))
  candidates[candidates %in% names(data)]
}

wellbeing_outcomes <- function(data) {
  candidates <- cfg_vec(cfg$analysis$primary_wellbeing_outcomes)
  candidates[candidates %in% names(data)]
}

wellbeing_falsification_outcomes <- function(data) {
  candidates <- cfg_vec(cfg$analysis$falsification_outcomes %||% c(
    "frequent_physical_distress_rate",
    "mean_bad_physical_days"
  ))
  candidates[candidates %in% names(data)]
}

hard_falsification_outcomes <- function(data) {
  candidates <- c("suicide_age_adjusted_rate", "drug_poisoning_age_adjusted_rate")
  candidates[candidates %in% names(data)]
}

wellbeing_longdiff_horizons <- function() {
  c(2L, 3L)
}

wellbeing_longdiff_horizon <- function(outcome) {
  if (identical(outcome, "fair_poor_health_rate")) 3L else 2L
}

standardized_outcome_name <- function(outcome) {
  paste0(outcome, "_age_std")
}

precision_weight_name <- function(outcome) {
  paste0(outcome, "_precision_wt")
}

standardized_precision_weight_name <- function(outcome) {
  paste0(outcome, "_age_std_precision_wt")
}

prepare_causal_sample <- function(data, years = analysis_causal_main_years()) {
  data |>
    dplyr::filter(year %in% years) |>
    dplyr::left_join(state_division_lookup(), by = "state_fips") |>
    dplyr::mutate(
      state = as.factor(state),
      division_year = interaction(division, year, drop = TRUE, lex.order = TRUE)
    )
}

treatment_lag_name <- function(treatment, lag_n) {
  if (lag_n <= 1L) return(treatment)
  if (grepl("^l1_", treatment)) {
    return(sub("^l1_", paste0("l", lag_n, "_"), treatment))
  }
  paste0("l", lag_n, "_", treatment)
}

ensure_treatment_lags <- function(data, treatment, max_lag = 3L) {
  out <- data
  lag_vars <- treatment

  if (max_lag >= 2L) {
    for (lag_n in 2:max_lag) {
      lag_name <- treatment_lag_name(treatment, lag_n)
      out <- add_lag(out, treatment, n = lag_n - 1L, group = "state", order = "year", new_name = lag_name)
      lag_vars <- c(lag_vars, lag_name)
    }
  }

  list(data = out, lag_vars = unique(lag_vars))
}

build_dynamic_formula <- function(outcome, treatment, controls = NULL) {
  rhs <- c(treatment, "y_l1", controls)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  stats::as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(rhs, collapse = " + "),
      " | state + division_year"
    )
  )
}

build_trend_formula <- function(outcome, treatment, controls = NULL) {
  rhs <- c(treatment, controls)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  stats::as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(rhs, collapse = " + "),
      " | state + division_year + state[trend_year]"
    )
  )
}

prepare_dynamic_wellbeing_data <- function(data, outcome, treatment, controls = analysis_primary_controls(), weight_var = NULL) {
  controls <- clean_control_set(data, controls)
  tmp <- add_lag(data, outcome, n = 1, group = "state", order = "year", new_name = "y_l1")
  needed <- unique(c(outcome, treatment, "y_l1", controls, "state", "year", "division_year", weight_var))
  dat <- tmp |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na(dplyr::all_of(c(outcome, treatment, "y_l1")))

  if (!is.null(weight_var) && weight_var %in% names(dat)) {
    dat <- dat |>
      tidyr::drop_na(dplyr::all_of(weight_var))
  }

  dat
}

prepare_trend_benchmark_data <- function(data, outcome, treatment, controls = analysis_primary_controls()) {
  controls <- clean_control_set(data, controls)
  dat <- data |>
    dplyr::mutate(trend_year = year - min(year, na.rm = TRUE)) |>
    dplyr::select(dplyr::all_of(unique(c(
      outcome, treatment, controls, "state", "year", "division_year", "trend_year"
    )))) |>
    tidyr::drop_na(dplyr::all_of(c(outcome, treatment)))

  dat
}

run_dynamic_wellbeing_fe <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  cluster_var = cfg$analysis$cluster_var %||% "state",
  weight_var = NULL
) {
  controls <- clean_control_set(data, controls)
  dat <- prepare_dynamic_wellbeing_data(data, outcome, treatment, controls, weight_var)

  wt <- NULL
  if (!is.null(weight_var) && weight_var %in% names(dat)) {
    wt <- stats::as.formula(paste0("~", weight_var))
  }

  fixest::feols(
    fml = build_dynamic_formula(outcome, treatment, controls),
    data = dat,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    weights = wt,
    warn = FALSE
  )
}

run_trend_benchmark_wellbeing <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  cluster_var = cfg$analysis$cluster_var %||% "state"
) {
  controls <- clean_control_set(data, controls)
  dat <- prepare_trend_benchmark_data(data, outcome, treatment, controls)

  fixest::feols(
    fml = build_trend_formula(outcome, treatment, controls),
    data = dat,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    warn = FALSE,
    notes = FALSE
  )
}

add_cce_terms <- function(data, vars) {
  vars <- vars[vars %in% names(data)]
  if (length(vars) == 0) return(data)

  data |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(vars),
        ~ mean(.x, na.rm = TRUE),
        .names = "cce_{.col}"
      )
    ) |>
    dplyr::ungroup()
}

build_cce_formula <- function(outcome, treatment, controls = NULL, cce_terms = NULL) {
  rhs <- c(treatment, "y_l1", controls, cce_terms)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  stats::as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(rhs, collapse = " + "),
      " | state"
    )
  )
}

run_cce_dynamic_wellbeing_fe <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  cluster_var = cfg$analysis$cluster_var %||% "state"
) {
  controls <- clean_control_set(data, controls)
  tmp <- add_lag(data, outcome, n = 1, group = "state", order = "year", new_name = "y_l1")
  base_vars <- unique(c(outcome, treatment, "y_l1", controls))
  tmp <- add_cce_terms(tmp, base_vars)
  cce_terms <- paste0("cce_", base_vars)
  needed <- unique(c(outcome, treatment, "y_l1", controls, cce_terms, "state", "year"))
  dat <- tmp |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na(dplyr::all_of(c(outcome, treatment, "y_l1")))

  fixest::feols(
    fml = build_cce_formula(outcome, treatment, controls, cce_terms),
    data = dat,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    warn = FALSE
  )
}

build_longdiff_vars <- function(data, vars, diff_n = 2L) {
  out <- data
  for (v in vars) {
    lag_name <- paste0("l", diff_n, "_", v)
    diff_name <- paste0("d", diff_n, "_", v)
    out <- add_lag(out, v, n = diff_n, group = "state", order = "year", new_name = lag_name)
    out <- out |>
      dplyr::mutate(!!diff_name := dplyr::if_else(!is.na(.data[[lag_name]]), .data[[v]] - .data[[lag_name]], NA_real_))
  }
  out
}

prepare_long_difference_data <- function(data, outcome, treatment, controls = analysis_primary_controls(), diff_n = 2L) {
  controls <- clean_control_set(data, controls)
  vars <- unique(c(outcome, treatment, controls))
  tmp <- build_longdiff_vars(data, vars, diff_n = diff_n)
  outcome_var <- paste0("d", diff_n, "_", outcome)
  treatment_var <- paste0("d", diff_n, "_", treatment)
  control_vars <- paste0("d", diff_n, "_", controls)
  control_vars <- control_vars[control_vars %in% names(tmp)]
  rhs <- c(treatment_var, control_vars)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  needed <- unique(c(outcome_var, treatment_var, control_vars, "state", "year", "division", "division_year"))
  dat <- tmp |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na(dplyr::all_of(c(outcome_var, treatment_var)))

  list(
    data = dat,
    outcome_var = outcome_var,
    treatment_var = treatment_var,
    control_vars = control_vars,
    rhs = rhs
  )
}

run_long_difference_wellbeing <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  diff_n = 2L,
  cluster_var = cfg$analysis$cluster_var %||% "state"
) {
  prep <- prepare_long_difference_data(data, outcome, treatment, controls, diff_n = diff_n)
  dat <- prep$data
  outcome_var <- prep$outcome_var
  treatment_var <- prep$treatment_var
  rhs <- prep$rhs

  fml <- stats::as.formula(
    paste0(
      outcome_var,
      " ~ ",
      paste(rhs, collapse = " + "),
      " | division_year"
    )
  )

  fixest::feols(
    fml = fml,
    data = dat,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    warn = FALSE
  )
}

prepare_moving_average_dynamic_data <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  ma_window = 3L
) {
  controls <- clean_control_set(data, controls)
  lagged <- ensure_treatment_lags(data, treatment, max_lag = ma_window)
  tmp <- add_lag(lagged$data, outcome, n = 1, group = "state", order = "year", new_name = "y_l1")
  ma_var <- paste0("ma", ma_window, "_", treatment)
  lag_vars <- lagged$lag_vars[seq_len(min(ma_window, length(lagged$lag_vars)))]
  tmp[[ma_var]] <- rowMeans(as.matrix(dplyr::select(tmp, dplyr::all_of(lag_vars))))

  needed <- unique(c(outcome, ma_var, "y_l1", controls, "state", "year", "division_year"))
  dat <- tmp |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na(dplyr::all_of(c(outcome, ma_var, "y_l1")))

  list(
    data = dat,
    treatment_var = ma_var,
    lag_vars = lag_vars
  )
}

run_moving_average_dynamic_wellbeing_fe <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  ma_window = 3L,
  cluster_var = cfg$analysis$cluster_var %||% "state"
) {
  prep <- prepare_moving_average_dynamic_data(data, outcome, treatment, controls, ma_window = ma_window)
  dat <- prep$data

  fixest::feols(
    fml = build_dynamic_formula(outcome, prep$treatment_var, controls),
    data = dat,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    warn = FALSE,
    notes = FALSE
  )
}

prepare_distributed_lag_dynamic_data <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  max_lag = 3L
) {
  controls <- clean_control_set(data, controls)
  lagged <- ensure_treatment_lags(data, treatment, max_lag = max_lag)
  tmp <- add_lag(lagged$data, outcome, n = 1, group = "state", order = "year", new_name = "y_l1")
  lag_vars <- lagged$lag_vars[seq_len(min(max_lag, length(lagged$lag_vars)))]

  needed <- unique(c(outcome, lag_vars, "y_l1", controls, "state", "year", "division_year"))
  dat <- tmp |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na(dplyr::all_of(c(outcome, lag_vars, "y_l1")))

  list(
    data = dat,
    lag_vars = lag_vars
  )
}

build_distributed_lag_formula <- function(outcome, lag_vars, controls = NULL) {
  rhs <- c(lag_vars, "y_l1", controls)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  stats::as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(rhs, collapse = " + "),
      " | state + division_year"
    )
  )
}

run_distributed_lag_dynamic_wellbeing_fe <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  max_lag = 3L,
  cluster_var = cfg$analysis$cluster_var %||% "state"
) {
  prep <- prepare_distributed_lag_dynamic_data(data, outcome, treatment, controls, max_lag = max_lag)

  fixest::feols(
    fml = build_distributed_lag_formula(outcome, prep$lag_vars, controls),
    data = prep$data,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    warn = FALSE,
    notes = FALSE
  )
}

tidy_distributed_lag_terms <- function(model, lag_vars, model_type) {
  term_rows <- broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term %in% lag_vars) |>
    dplyr::mutate(
      model_type = model_type,
      n_obs = stats::nobs(model)
    )

  beta <- stats::coef(model)[lag_vars]
  vc <- stats::vcov(model)[lag_vars, lag_vars, drop = FALSE]
  cum_est <- sum(beta)
  cum_se <- sqrt(sum(vc))
  cum_t <- cum_est / cum_se
  cum_p <- 2 * stats::pnorm(abs(cum_t), lower.tail = FALSE)
  cum_ci_low <- cum_est - stats::qnorm(0.975) * cum_se
  cum_ci_high <- cum_est + stats::qnorm(0.975) * cum_se

  cumulative_row <- tibble::tibble(
    term = paste0("cumulative_", length(lag_vars), "lag_effect"),
    estimate = cum_est,
    std.error = cum_se,
    statistic = cum_t,
    p.value = cum_p,
    conf.low = cum_ci_low,
    conf.high = cum_ci_high,
    model_type = model_type,
    n_obs = stats::nobs(model)
  )

  dplyr::bind_rows(term_rows, cumulative_row)
}

extract_fixest_term <- function(model, term) {
  ct <- fixest::coeftable(model)
  idx <- match(term, rownames(ct))
  if (is.na(idx)) return(NULL)

  tibble::tibble(
    term = term,
    estimate = unname(ct[idx, 1]),
    std.error = unname(ct[idx, 2]),
    statistic = unname(ct[idx, 3]),
    p.value = unname(ct[idx, 4])
  )
}

run_wild_cluster_bootstrap_dynamic_fe <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  cluster_var = cfg$analysis$cluster_var %||% "state",
  boot_reps = 399L,
  seed = cfg$project$seed %||% 123
) {
  controls <- clean_control_set(data, controls)
  dat <- prepare_dynamic_wellbeing_data(data, outcome, treatment, controls)
  full_fml <- build_dynamic_formula(outcome, treatment, controls)
  null_fml <- build_dynamic_formula(outcome, NULL, controls)

  full_mod <- fixest::feols(
    fml = full_fml,
    data = dat,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    warn = FALSE,
    notes = FALSE
  )
  null_mod <- fixest::feols(
    fml = null_fml,
    data = dat,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    warn = FALSE,
    notes = FALSE
  )

  obs <- extract_fixest_term(full_mod, treatment)
  if (is.null(obs)) stop("Treatment term not found in fitted model: ", treatment, call. = FALSE)

  used_idx <- fixest::obs(null_mod)
  dat_used <- dat[used_idx, , drop = FALSE]
  cluster_ids <- as.character(dat_used[[cluster_var]])
  fitted_null <- stats::fitted(null_mod)
  resid_null <- stats::residuals(null_mod)
  unique_clusters <- unique(cluster_ids)

  set.seed(seed)
  boot_store <- replicate(boot_reps, {
    weights <- stats::setNames(sample(c(-1, 1), length(unique_clusters), replace = TRUE), unique_clusters)
    boot_dat <- dat_used
    boot_dat[[outcome]] <- fitted_null + resid_null * unname(weights[cluster_ids])
    boot_mod <- tryCatch(
      fixest::feols(
        fml = full_fml,
        data = boot_dat,
        cluster = stats::as.formula(paste0("~", cluster_var)),
        warn = FALSE,
        notes = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(boot_mod)) return(c(estimate = NA_real_, statistic = NA_real_))
    boot_term <- extract_fixest_term(boot_mod, treatment)
    if (is.null(boot_term)) return(c(estimate = NA_real_, statistic = NA_real_))

    c(
      estimate = boot_term$estimate[[1]],
      statistic = boot_term$statistic[[1]]
    )
  })

  boot_df <- tibble::as_tibble(t(boot_store))
  boot_df <- boot_df |>
    tidyr::drop_na()

  if (nrow(boot_df) == 0) {
    stop("Wild bootstrap failed for all replications.", call. = FALSE)
  }

  boot_centered <- boot_df$estimate - mean(boot_df$estimate, na.rm = TRUE)
  ci <- obs$estimate[[1]] + stats::quantile(boot_centered, probs = c(0.025, 0.975), na.rm = TRUE)
  p_boot <- mean(abs(boot_df$statistic) >= abs(obs$statistic[[1]]), na.rm = TRUE)

  tibble::tibble(
    term = treatment,
    estimate = obs$estimate[[1]],
    std.error = stats::sd(boot_df$estimate, na.rm = TRUE),
    statistic = obs$statistic[[1]],
    p.value = p_boot,
    conf.low = unname(ci[[1]]),
    conf.high = unname(ci[[2]]),
    model_type = "dynamic_fe_wild_cluster_bootstrap",
    n_obs = stats::nobs(full_mod),
    boot_reps = boot_reps,
    boot_success = nrow(boot_df),
    inference_standard = "wild_cluster_rademacher"
  )
}

run_package_wild_cluster_bootstrap_dynamic_fe <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  cluster_var = cfg$analysis$cluster_var %||% "state",
  boot_reps = 999L,
  seed = cfg$project$seed %||% 123,
  engine = "R-lean",
  sampling = "standard"
) {
  if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
    return(run_wild_cluster_bootstrap_dynamic_fe(
      data = data,
      outcome = outcome,
      treatment = treatment,
      controls = controls,
      cluster_var = cluster_var,
      boot_reps = boot_reps,
      seed = seed
    ))
  }

  controls <- clean_control_set(data, controls)
  mod <- run_dynamic_wellbeing_fe(data, outcome = outcome, treatment = treatment, controls = controls)

  set.seed(seed)
  if (requireNamespace("dqrng", quietly = TRUE)) {
    dqrng::dqset.seed(seed)
  }

  boot_obj <- tryCatch(
    fwildclusterboot::boottest(
      mod,
      B = boot_reps,
      param = treatment,
      clustid = cluster_var,
      engine = engine,
      sampling = sampling,
      conf_int = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(boot_obj)) {
    return(run_wild_cluster_bootstrap_dynamic_fe(
      data = data,
      outcome = outcome,
      treatment = treatment,
      controls = controls,
      cluster_var = cluster_var,
      boot_reps = boot_reps,
      seed = seed
    ))
  }

  boot_tidy <- generics::tidy(boot_obj)
  tibble::tibble(
    term = treatment,
    estimate = boot_tidy$estimate[[1]],
    std.error = NA_real_,
    statistic = boot_tidy$statistic[[1]],
    p.value = boot_tidy$p.value[[1]],
    conf.low = as.numeric(boot_tidy$conf.low[[1]] %||% NA_real_),
    conf.high = as.numeric(boot_tidy$conf.high[[1]] %||% NA_real_),
    model_type = "dynamic_fe_wild_cluster_bootstrap",
    n_obs = stats::nobs(mod),
    boot_reps = boot_reps,
    boot_success = boot_reps,
    inference_standard = "fwildclusterboot_rlean",
    boot_engine = engine
  )
}

build_dynamic_panel_gmm_formula <- function(outcome, treatment, controls = NULL) {
  build_dynamic_panel_gmm_formula_with_lags(outcome, treatment, controls = controls, lag_inst = "2:3")
}

build_dynamic_panel_gmm_formula_with_lags <- function(outcome, treatment, controls = NULL, lag_inst = "2:3") {
  rhs <- c(paste0("lag(", outcome, ", 1)"), treatment, controls)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  iv_terms <- c(treatment, controls)
  iv_terms <- iv_terms[!is.na(iv_terms) & nzchar(iv_terms)]

  stats::as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(rhs, collapse = " + "),
      " | lag(",
      outcome,
      ", ",
      lag_inst,
      ") | ",
      paste(iv_terms, collapse = " + ")
    )
  )
}

run_dynamic_panel_gmm <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  effect = "twoways",
  model = "twosteps",
  transformation = "d",
  collapse = FALSE,
  lag_inst = "2:3"
) {
  if (!requireNamespace("plm", quietly = TRUE)) {
    stop("plm is not installed.", call. = FALSE)
  }
  suppressPackageStartupMessages(library(plm))

  controls <- clean_control_set(data, controls)
  dat <- prepare_dynamic_wellbeing_data(data, outcome, treatment, controls)
  pdata <- plm::pdata.frame(dat, index = c("state", "year"))

  plm::pgmm(
    formula = build_dynamic_panel_gmm_formula_with_lags(outcome, treatment, controls, lag_inst = lag_inst),
    data = pdata,
    effect = effect,
    model = model,
    transformation = transformation,
    collapse = collapse
  )
}

tidy_dynamic_panel_gmm_term <- function(model, term, model_type = "difference_gmm_twoways") {
  summ <- summary(model, robust = TRUE)
  coef_df <- as.data.frame(summ$coefficients)
  coef_df$term <- rownames(coef_df)
  rownames(coef_df) <- NULL
  row <- coef_df[coef_df$term == term, , drop = FALSE]

  if (nrow(row) == 0) stop("Treatment term not found in GMM model: ", term, call. = FALSE)

  tibble::tibble(
    term = term,
    estimate = row$Estimate[[1]],
    std.error = row$`Std. Error`[[1]],
    statistic = row$`z-value`[[1]],
    p.value = row$`Pr(>|z|)`[[1]],
    conf.low = row$Estimate[[1]] - stats::qnorm(0.975) * row$`Std. Error`[[1]],
    conf.high = row$Estimate[[1]] + stats::qnorm(0.975) * row$`Std. Error`[[1]],
    model_type = model_type,
    n_obs = tryCatch(stats::nobs(model), error = function(e) NA_integer_),
    m1_p = summ$m1$p.value %||% NA_real_,
    m2_p = summ$m2$p.value %||% NA_real_,
    sargan_p = summ$sargan$p.value %||% NA_real_,
    inference_standard = "pgmm_robust"
  )
}

tidy_primary_term <- function(model, term, model_type) {
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term == !!term) |>
    dplyr::mutate(model_type = model_type, n_obs = stats::nobs(model))
}

run_primary_wellbeing_causal_models <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatments <- wellbeing_treatments(sample)
  outcomes <- wellbeing_outcomes(sample)
  specs <- analysis_model_specs(include_weighted = FALSE)

  dynamic_rows <- purrr::map_dfr(names(specs), function(spec_name) {
    spec <- specs[[spec_name]]
    control_count <- length(clean_control_set(sample, spec$controls))
    purrr::map_dfr(treatments, function(tr) {
      purrr::map_dfr(outcomes, function(y) {
        mod <- run_dynamic_wellbeing_fe(sample, outcome = y, treatment = tr, controls = spec$controls)
        tidy_primary_term(mod, tr, "dynamic_fe") |>
          dplyr::mutate(spec = spec_name, outcome = y, treatment = tr, control_count = control_count)
      })
    })
  })

  longdiff_rows <- purrr::map_dfr(names(specs), function(spec_name) {
    spec <- specs[[spec_name]]
    control_count <- length(clean_control_set(sample, spec$controls))
    purrr::map_dfr(treatments, function(tr) {
      purrr::map_dfr(outcomes, function(y) {
        purrr::map_dfr(wellbeing_longdiff_horizons(), function(diff_n) {
          mod <- run_long_difference_wellbeing(sample, outcome = y, treatment = tr, controls = spec$controls, diff_n = diff_n)
          tidy_primary_term(mod, paste0("d", diff_n, "_", tr), paste0("long_diff_", diff_n, "y")) |>
            dplyr::mutate(spec = spec_name, outcome = y, treatment = tr, control_count = control_count, diff_n = diff_n)
        })
      })
    })
  })

  safe_write_csv(dynamic_rows, path_project(cfg$paths$tables_root, "wellbeing_dynamic_fe.csv"))
  safe_write_csv(longdiff_rows, path_project(cfg$paths$tables_root, "wellbeing_longdiff_coefficients.csv"))
  invisible(list(dynamic = dynamic_rows, longdiff = longdiff_rows))
}

run_latent_factor_wellbeing_sensitivity <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatments <- wellbeing_treatments(sample)
  outcomes <- wellbeing_outcomes(sample)
  specs <- analysis_model_specs(include_weighted = FALSE)

  out <- purrr::map_dfr(names(specs), function(spec_name) {
    spec <- specs[[spec_name]]
    control_count <- length(clean_control_set(sample, spec$controls))
    purrr::map_dfr(treatments, function(tr) {
      purrr::map_dfr(outcomes, function(y) {
        mod <- run_cce_dynamic_wellbeing_fe(sample, outcome = y, treatment = tr, controls = spec$controls)
        tidy_primary_term(mod, tr, "cce_dynamic_fe") |>
          dplyr::mutate(spec = spec_name, outcome = y, treatment = tr, control_count = control_count)
      })
    })
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_cce_dynamic_fe.csv"))
  invisible(out)
}

run_measurement_wellbeing_sensitivity <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatments <- wellbeing_treatments(sample)
  outcomes <- wellbeing_outcomes(sample)
  specs <- analysis_model_specs(include_weighted = FALSE)

  rows <- list()

  for (spec_name in names(specs)) {
    spec <- specs[[spec_name]]
    control_count <- length(clean_control_set(sample, spec$controls))

    for (tr in treatments) {
      for (y in outcomes) {
        std_y <- standardized_outcome_name(y)
        raw_wt <- precision_weight_name(y)
        std_wt <- standardized_precision_weight_name(y)

        if (std_y %in% names(sample)) {
          mod_std <- run_dynamic_wellbeing_fe(sample, outcome = std_y, treatment = tr, controls = spec$controls)
          rows[[paste(spec_name, tr, std_y, "std", sep = "__")]] <- tidy_primary_term(mod_std, tr, "dynamic_fe_age_std") |>
            dplyr::mutate(
              spec = spec_name,
              outcome = y,
              outcome_variant = std_y,
              treatment = tr,
              control_count = control_count
            )
        }

        if (raw_wt %in% names(sample)) {
          mod_pw <- run_dynamic_wellbeing_fe(sample, outcome = y, treatment = tr, controls = spec$controls, weight_var = raw_wt)
          rows[[paste(spec_name, tr, y, "precision", sep = "__")]] <- tidy_primary_term(mod_pw, tr, "dynamic_fe_precision_weighted") |>
            dplyr::mutate(
              spec = spec_name,
              outcome = y,
              outcome_variant = y,
              treatment = tr,
              control_count = control_count,
              weight_var = raw_wt
            )
        }

        if (std_y %in% names(sample) && std_wt %in% names(sample)) {
          mod_std_pw <- run_dynamic_wellbeing_fe(sample, outcome = std_y, treatment = tr, controls = spec$controls, weight_var = std_wt)
          rows[[paste(spec_name, tr, std_y, "std_precision", sep = "__")]] <- tidy_primary_term(mod_std_pw, tr, "dynamic_fe_age_std_precision_weighted") |>
            dplyr::mutate(
              spec = spec_name,
              outcome = y,
              outcome_variant = std_y,
              treatment = tr,
              control_count = control_count,
              weight_var = std_wt
            )
        }
      }
    }
  }

  out <- dplyr::bind_rows(rows)
  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_measurement_sensitivity.csv"))
  invisible(out)
}

run_falsification_outcome_checks <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatments <- wellbeing_treatments(sample)
  outcomes <- wellbeing_falsification_outcomes(sample)
  specs <- analysis_model_specs(include_weighted = FALSE)

  out <- purrr::map_dfr(names(specs), function(spec_name) {
    spec <- specs[[spec_name]]
    control_count <- length(clean_control_set(sample, spec$controls))
    purrr::map_dfr(treatments, function(tr) {
      purrr::map_dfr(outcomes, function(y) {
        mod <- run_dynamic_wellbeing_fe(sample, outcome = y, treatment = tr, controls = spec$controls)
        tidy_primary_term(mod, tr, "dynamic_fe_falsification") |>
          dplyr::mutate(spec = spec_name, outcome = y, treatment = tr, control_count = control_count)
      })
    })
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_falsification_dynamic_fe.csv"))
  invisible(out)
}

run_hard_outcome_falsification_checks <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatments <- wellbeing_treatments(sample)
  outcomes <- hard_falsification_outcomes(sample)
  specs <- analysis_model_specs(include_weighted = FALSE)

  if (length(outcomes) == 0) {
    note("No hard-outcome falsification columns found in panel; skipping.")
    return(invisible(NULL))
  }

  out <- purrr::map_dfr(names(specs), function(spec_name) {
    spec <- specs[[spec_name]]
    control_count <- length(clean_control_set(sample, spec$controls))
    purrr::map_dfr(treatments, function(tr) {
      purrr::map_dfr(outcomes, function(y) {
        mod <- run_dynamic_wellbeing_fe(sample, outcome = y, treatment = tr, controls = spec$controls)
        tidy_primary_term(mod, tr, "dynamic_fe_hard_falsification") |>
          dplyr::mutate(spec = spec_name, outcome = y, treatment = tr, control_count = control_count)
      })
    })
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_hard_outcome_falsification.csv"))
  invisible(out)
}

run_locked_trends_benchmark <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatment <- analysis_primary_treatment()
  outcomes <- unique(c(analysis_primary_outcome(), analysis_secondary_outcome()))
  controls <- clean_control_set(sample, analysis_causal_core_controls())

  out <- purrr::map_dfr(outcomes, function(y) {
    mod <- run_trend_benchmark_wellbeing(sample, outcome = y, treatment = treatment, controls = controls)
    tidy_primary_term(mod, treatment, "dynamic_fe_state_trends") |>
      dplyr::mutate(
        outcome = y,
        treatment = treatment,
        spec = "baseline",
        control_set = "causal_core"
      )
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_trends_benchmark.csv"))
  invisible(out)
}

run_locked_slow_response_checks <- function(panel = read_panel(), ma_window = 3L) {
  sample <- prepare_causal_sample(panel)
  treatment <- analysis_primary_treatment()
  outcomes <- unique(c(analysis_primary_outcome(), analysis_secondary_outcome()))
  controls <- clean_control_set(sample, analysis_causal_core_controls())

  out <- purrr::map_dfr(outcomes, function(y) {
    prep <- prepare_moving_average_dynamic_data(sample, outcome = y, treatment = treatment, controls = controls, ma_window = ma_window)
    mod <- run_moving_average_dynamic_wellbeing_fe(sample, outcome = y, treatment = treatment, controls = controls, ma_window = ma_window)
    tidy_primary_term(mod, prep$treatment_var, paste0("dynamic_fe_ma", ma_window, "_treatment")) |>
      dplyr::mutate(
        outcome = y,
        treatment = treatment,
        treatment_variant = prep$treatment_var,
        lag_window = ma_window,
        spec = "baseline",
        control_set = "causal_core"
      )
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_slow_response.csv"))
  invisible(out)
}

run_locked_distributed_lag_checks <- function(panel = read_panel(), max_lag = 3L) {
  sample <- prepare_causal_sample(panel)
  treatment <- analysis_primary_treatment()
  outcomes <- unique(c(analysis_primary_outcome(), analysis_secondary_outcome()))
  controls <- clean_control_set(sample, analysis_causal_core_controls())

  out <- purrr::map_dfr(outcomes, function(y) {
    prep <- prepare_distributed_lag_dynamic_data(sample, outcome = y, treatment = treatment, controls = controls, max_lag = max_lag)
    mod <- run_distributed_lag_dynamic_wellbeing_fe(sample, outcome = y, treatment = treatment, controls = controls, max_lag = max_lag)
    tidy_distributed_lag_terms(mod, prep$lag_vars, paste0("dynamic_fe_distributed_lag_", max_lag)) |>
      dplyr::mutate(
        outcome = y,
        treatment = treatment,
        treatment_variant = dplyr::if_else(term == paste0("cumulative_", max_lag, "lag_effect"), term, term),
        lag_window = max_lag,
        spec = "baseline",
        control_set = "causal_core"
      )
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_distributed_lag.csv"))
  invisible(out)
}

run_locked_control_ladder_checks <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatment <- analysis_primary_treatment()
  outcomes <- unique(c(analysis_primary_outcome(), analysis_secondary_outcome()))
  ladders <- locked_control_ladder_specs(sample)

  out <- purrr::map_dfr(names(ladders), function(ladder_name) {
    controls <- ladders[[ladder_name]]
    purrr::map_dfr(outcomes, function(y) {
      mod <- run_dynamic_wellbeing_fe(sample, outcome = y, treatment = treatment, controls = controls)
      tidy_primary_term(mod, treatment, "dynamic_fe_control_ladder") |>
        dplyr::mutate(
          outcome = y,
          treatment = treatment,
          spec = ladder_name,
          control_set = ladder_name,
          control_count = length(controls)
        )
    })
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_control_ladder.csv"))
  invisible(out)
}

run_locked_wild_bootstrap_checks <- function(panel = read_panel(), boot_reps = 999L) {
  sample <- prepare_causal_sample(panel)
  treatment <- analysis_primary_treatment()
  outcomes <- unique(c(analysis_primary_outcome(), analysis_secondary_outcome()))
  controls <- clean_control_set(sample, analysis_causal_core_controls())

  out <- purrr::map_dfr(outcomes, function(y) {
    run_package_wild_cluster_bootstrap_dynamic_fe(
      sample,
      outcome = y,
      treatment = treatment,
      controls = controls,
      boot_reps = boot_reps
    ) |>
      dplyr::mutate(
        outcome = y,
        treatment = treatment,
        spec = "baseline",
        control_set = "causal_core"
      )
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_wild_bootstrap.csv"))
  invisible(out)
}

run_locked_dynamic_panel_checks <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatment <- analysis_primary_treatment()
  outcomes <- unique(c(analysis_primary_outcome(), analysis_secondary_outcome()))
  controls <- clean_control_set(sample, analysis_causal_core_controls())

  gmm_variants <- list(
    difference = list(effect = "twoways", model = "twosteps", transformation = "d", collapse = TRUE, lag_inst = "2:3", model_type = "difference_gmm_twoways"),
    system = list(effect = "twoways", model = "onestep", transformation = "ld", collapse = TRUE, lag_inst = "2:3", model_type = "system_gmm_twoways")
  )

  out <- purrr::map_dfr(names(gmm_variants), function(variant_name) {
    variant <- gmm_variants[[variant_name]]
    purrr::map_dfr(outcomes, function(y) {
      mod <- tryCatch(
        run_dynamic_panel_gmm(
          sample,
          outcome = y,
          treatment = treatment,
          controls = controls,
          effect = variant$effect,
          model = variant$model,
          transformation = variant$transformation,
          collapse = variant$collapse,
          lag_inst = variant$lag_inst
        ),
        error = function(e) NULL
      )
      if (is.null(mod)) return(tibble::tibble())

      tidy_dynamic_panel_gmm_term(mod, treatment, variant$model_type) |>
        dplyr::mutate(
          outcome = y,
          treatment = treatment,
          spec = "baseline",
          control_set = "causal_core",
          gmm_variant = variant_name,
          diagnostics_pass = !is.na(m2_p) & !is.na(sargan_p) & m2_p > 0.05 & sargan_p > 0.05,
          retained = variant_name == "difference" & !is.na(m2_p) & !is.na(sargan_p) & m2_p > 0.05 & sargan_p > 0.05
        )
    })
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_dynamic_panel_gmm.csv"))
  invisible(out)
}

run_subgroup_reverse_causality_checks <- function(panel = read_panel(), subgroup_panel = read_brfss_subgroups()) {
  panel_base <- prepare_causal_sample(panel)
  subgroup_panel <- subgroup_panel |>
    dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year))

  treatments <- wellbeing_treatments(panel_base)
  outcomes <- wellbeing_outcomes(panel_base)

  out <- purrr::map_dfr(unique(subgroup_panel$subgroup), function(subgroup_name) {
    dat <- panel_base |>
      dplyr::select(-dplyr::all_of(intersect(outcomes, names(panel_base)))) |>
      dplyr::left_join(
        subgroup_panel |>
          dplyr::filter(subgroup == subgroup_name) |>
          dplyr::select(state_fips, year, dplyr::all_of(outcomes)),
        by = c("state_fips", "year")
      )

    purrr::map_dfr(treatments, function(tr) {
      purrr::map_dfr(outcomes, function(y) {
        mod <- run_dynamic_wellbeing_fe(dat, outcome = y, treatment = tr, controls = analysis_primary_controls())
        tidy_primary_term(mod, tr, "dynamic_fe_subgroup") |>
          dplyr::mutate(subgroup = subgroup_name, outcome = y, treatment = tr, spec = "baseline")
      })
    })
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "wellbeing_subgroup_dynamic_fe.csv"))
  invisible(out)
}

if (sys.nframe() == 0) {
  panel <- read_panel()
  run_primary_wellbeing_causal_models(panel)
  run_latent_factor_wellbeing_sensitivity(panel)
  run_measurement_wellbeing_sensitivity(panel)
  run_falsification_outcome_checks(panel)
  run_hard_outcome_falsification_checks(panel)
  run_locked_control_ladder_checks(panel)
  run_locked_trends_benchmark(panel)
  run_locked_slow_response_checks(panel)
  run_locked_distributed_lag_checks(panel)
  run_locked_wild_bootstrap_checks(panel)
  run_locked_dynamic_panel_checks(panel)
  run_subgroup_reverse_causality_checks(panel)
}
