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
  needed <- unique(c(outcome_var, treatment_var, control_vars, "state", "year"))
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
      " | year"
    )
  )

  fixest::feols(
    fml = fml,
    data = dat,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    warn = FALSE
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
        diff_n <- wellbeing_longdiff_horizon(y)
        mod <- run_long_difference_wellbeing(sample, outcome = y, treatment = tr, controls = spec$controls, diff_n = diff_n)
        tidy_primary_term(mod, paste0("d", diff_n, "_", tr), paste0("long_diff_", diff_n, "y")) |>
          dplyr::mutate(spec = spec_name, outcome = y, treatment = tr, control_count = control_count, diff_n = diff_n)
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
  run_subgroup_reverse_causality_checks(panel)
}
