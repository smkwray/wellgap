# 34_sensitivity.R
# Locked-design sensitivity diagnostics for the causal core.

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
source(file.path(project_root, "R", "35_causal_robustness.R"))

required_pkgs <- c("dplyr", "purrr", "tibble", "tidyr")
invisible(lapply(required_pkgs, require, character.only = TRUE))

read_panel <- function() {
  panel_path <- path_project(cfg$paths$final_root, "state_year_panel.rds")
  if (!file.exists(panel_path)) stop("Missing final panel: ", panel_path, call. = FALSE)
  safe_read_rds(panel_path)
}

locked_sensitivity_specs <- function(data) {
  list(
    baseline = clean_control_set(data, analysis_causal_core_controls()),
    rich = clean_control_set(data, analysis_rich_controls())
  )
}

locked_sensitivity_outcomes <- function() {
  unique(c(analysis_primary_outcome(), analysis_secondary_outcome()))
}

locked_sensitivity_designs <- function() {
  c("dynamic_fe", paste0("long_diff_", wellbeing_longdiff_horizons(), "y"))
}

fit_locked_sensitivity_model <- function(data, outcome, treatment, controls, design) {
  if (identical(design, "dynamic_fe")) {
    mod <- run_dynamic_wellbeing_fe(data, outcome = outcome, treatment = treatment, controls = controls)
    return(list(model = mod, term = treatment, diff_n = NA_integer_))
  }

  diff_n <- as.integer(sub("^long_diff_([0-9]+)y$", "\\1", design))
  prep <- prepare_long_difference_data(data, outcome, treatment, controls, diff_n = diff_n)
  mod <- run_long_difference_wellbeing(data, outcome = outcome, treatment = treatment, controls = controls, diff_n = diff_n)
  list(model = mod, term = prep$treatment_var, diff_n = diff_n)
}

safe_fit_locked_sensitivity_model <- function(data, outcome, treatment, controls, design) {
  tryCatch(
    fit_locked_sensitivity_model(data, outcome, treatment, controls, design),
    error = function(e) NULL
  )
}

run_placebo_lead_test <- function(data, outcome, treatment, controls, design, lead_h = 1L) {
  lead_var <- paste0("f", lead_h, "_", treatment)
  tmp <- add_lead(data, treatment, n = lead_h, group = "state", order = "year", new_name = lead_var)
  fit <- safe_fit_locked_sensitivity_model(tmp, outcome, lead_var, controls, design)
  if (is.null(fit)) return(tibble::tibble())

  tidy_primary_term(fit$model, fit$term, design) |>
    dplyr::mutate(
      spec = if (setequal(controls, clean_control_set(data, analysis_causal_core_controls()))) "baseline" else "rich",
      outcome = outcome,
      treatment = treatment,
      placebo_lead = lead_h,
      diff_n = fit$diff_n,
      inference_standard = "conventional_clustered"
    )
}

run_leave_one_state_out <- function(data, outcome, treatment, controls, design) {
  states <- sort(unique(data$state))
  spec_name <- if (setequal(controls, clean_control_set(data, analysis_causal_core_controls()))) "baseline" else "rich"

  purrr::map_dfr(states, function(st) {
    split_data <- data |>
      dplyr::filter(state != st)
    fit <- safe_fit_locked_sensitivity_model(split_data, outcome, treatment, controls, design)
    if (is.null(fit)) return(tibble::tibble())

    tidy_primary_term(fit$model, fit$term, design) |>
      dplyr::mutate(
        spec = spec_name,
        dropped_state = st,
        outcome = outcome,
        treatment = treatment,
        diff_n = fit$diff_n,
        inference_standard = "conventional_clustered"
      )
  })
}

make_locked_sample_splits <- function(data, treatment) {
  state_cutoff <- data |>
    dplyr::group_by(state) |>
    dplyr::summarise(avg_treatment = mean(.data[[treatment]], na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(high_ineq_state = avg_treatment >= stats::quantile(avg_treatment, probs = 0.75, na.rm = TRUE)) |>
    dplyr::filter(high_ineq_state) |>
    dplyr::pull(state)

  list(
    full = data,
    exclude_dc = data |> dplyr::filter(state != "District of Columbia"),
    early_period = data |> dplyr::filter(year <= 2015),
    late_period = data |> dplyr::filter(year >= 2016),
    exclude_top_ineq_states = data |> dplyr::filter(!(state %in% state_cutoff))
  )
}

run_sample_split_fe <- function(data, outcome, treatment, controls, design) {
  spec_name <- if (setequal(controls, clean_control_set(data, analysis_causal_core_controls()))) "baseline" else "rich"
  splits <- make_locked_sample_splits(data, treatment)

  purrr::map_dfr(names(splits), function(split_name) {
    split_data <- splits[[split_name]]
    fit <- safe_fit_locked_sensitivity_model(split_data, outcome, treatment, controls, design)
    if (is.null(fit)) return(tibble::tibble())

    tidy_primary_term(fit$model, fit$term, design) |>
      dplyr::mutate(
        spec = spec_name,
        sample = split_name,
        outcome = outcome,
        treatment = treatment,
        diff_n = fit$diff_n,
        n_obs = stats::nobs(fit$model),
        n_states = dplyr::n_distinct(split_data$state),
        year_span = paste(range(split_data$year), collapse = "-"),
        inference_standard = "conventional_clustered"
      )
  })
}

run_exploratory_sensemakr_lm <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  benchmark_covariates = c("l1_unemployment_rate", "l1_log_real_mean_income")
) {
  if (!requireNamespace("sensemakr", quietly = TRUE)) {
    stop("sensemakr is not installed.", call. = FALSE)
  }

  controls <- controls[controls %in% names(data)]
  benchmark_covariates <- benchmark_covariates[benchmark_covariates %in% names(data)]
  rhs <- paste(c(treatment, controls, "factor(state)", "factor(year)"), collapse = " + ")
  fml <- as.formula(paste0(outcome, " ~ ", rhs))
  dat <- data |>
    dplyr::select(dplyr::all_of(unique(c(outcome, treatment, controls, "state", "year")))) |>
    tidyr::drop_na()

  lm_mod <- stats::lm(fml, data = dat)
  sm <- sensemakr::sensemakr(
    model = lm_mod,
    treatment = treatment,
    benchmark_covariates = benchmark_covariates
  )

  capture.output(
    summary(sm),
    file = path_project(cfg$paths$tables_root, paste0("sensemakr_exploratory_", outcome, "_", treatment, ".txt"))
  )
  invisible(sm)
}

run_primary_sensitivity <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatment <- analysis_primary_treatment()
  outcomes <- locked_sensitivity_outcomes()
  specs <- locked_sensitivity_specs(sample)
  designs <- locked_sensitivity_designs()

  placebo <- purrr::map_dfr(names(specs), function(spec_name) {
    controls <- specs[[spec_name]]
    purrr::map_dfr(outcomes, function(outcome) {
      purrr::map_dfr(designs, function(design) {
        run_placebo_lead_test(sample, outcome = outcome, treatment = treatment, controls = controls, design = design)
      })
    })
  })

  loo <- purrr::map_dfr(names(specs), function(spec_name) {
    controls <- specs[[spec_name]]
    purrr::map_dfr(outcomes, function(outcome) {
      purrr::map_dfr(designs, function(design) {
        run_leave_one_state_out(sample, outcome = outcome, treatment = treatment, controls = controls, design = design)
      })
    })
  })

  sample_splits <- purrr::map_dfr(names(specs), function(spec_name) {
    controls <- specs[[spec_name]]
    purrr::map_dfr(outcomes, function(outcome) {
      purrr::map_dfr(designs, function(design) {
        run_sample_split_fe(sample, outcome = outcome, treatment = treatment, controls = controls, design = design)
      })
    })
  })

  safe_write_csv(placebo, path_project(cfg$paths$tables_root, "placebo_lead_test.csv"))
  safe_write_csv(loo, path_project(cfg$paths$tables_root, "leave_one_state_out.csv"))
  safe_write_csv(sample_splits, path_project(cfg$paths$tables_root, "sample_split_fe.csv"))

  invisible(list(placebo = placebo, leave_one_out = loo, sample_splits = sample_splits))
}

if (sys.nframe() == 0) {
  panel <- read_panel()
  run_primary_sensitivity(panel)
}
