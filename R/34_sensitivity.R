# 34_sensitivity.R
# Sensitivity, placebo, and leave-one-state-out diagnostics.

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
source(file.path(project_root, "R", "30_model_fe.R"))

required_pkgs <- c("dplyr", "purrr", "tibble", "tidyr", "fixest", "sensemakr")
invisible(lapply(required_pkgs, require, character.only = TRUE))

read_panel <- function() {
  panel_path <- path_project(cfg$paths$final_root, "state_year_panel.rds")
  if (!file.exists(panel_path)) stop("Missing final panel: ", panel_path, call. = FALSE)
  safe_read_rds(panel_path)
}

run_placebo_lead_test <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  lead_h = 1
) {
  tmp <- add_lead(data, treatment, n = lead_h, group = "state", order = "year", new_name = paste0("f", lead_h, "_", treatment))
  lead_var <- paste0("f", lead_h, "_", treatment)
  mod <- run_twfe(tmp, outcome = outcome, treatment = lead_var, controls = controls)
  tidy_fixest_model(mod, keep = lead_var) |>
    dplyr::mutate(outcome = outcome, treatment = treatment, placebo_lead = lead_h)
}

run_leave_one_state_out <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls()
) {
  states <- unique(data$state)
  purrr::map_dfr(states, function(st) {
    mod <- run_twfe(data |> dplyr::filter(state != st), outcome = outcome, treatment = treatment, controls = controls)
    tidy_fixest_model(mod, keep = treatment) |>
      dplyr::mutate(dropped_state = st, outcome = outcome, treatment = treatment)
  })
}

run_sensemakr_lm <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  benchmark_covariates = c("l1_unemployment_rate", "l1_log_real_mean_income")
) {
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

  capture.output(summary(sm), file = path_project(cfg$paths$tables_root, paste0("sensemakr_", outcome, "_", treatment, ".txt")))
  invisible(sm)
}

run_sample_split_fe <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls()
) {
  state_cutoff <- data |>
    dplyr::group_by(state) |>
    dplyr::summarise(avg_treatment = mean(.data[[treatment]], na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(high_ineq_state = avg_treatment >= stats::quantile(avg_treatment, probs = 0.75, na.rm = TRUE)) |>
    dplyr::filter(high_ineq_state) |>
    dplyr::pull(state)

  splits <- list(
    full = data,
    exclude_dc = data |> dplyr::filter(state != "District of Columbia"),
    pre_2020 = data |> dplyr::filter(year <= 2019),
    post_2020 = data |> dplyr::filter(year >= 2021),
    exclude_top_ineq_states = data |> dplyr::filter(!(state %in% state_cutoff))
  )

  purrr::map_dfr(names(splits), function(split_name) {
    split_data <- splits[[split_name]]
    mod <- run_twfe(split_data, outcome = outcome, treatment = treatment, controls = controls)
    tidy_fixest_model(mod, keep = treatment) |>
      dplyr::mutate(
        sample = split_name,
        outcome = outcome,
        treatment = treatment,
        n_obs = stats::nobs(mod),
        n_states = dplyr::n_distinct(split_data$state)
      )
  })
}

run_primary_sensitivity <- function(panel = read_panel()) {
  treatment <- analysis_primary_treatment()
  outcome <- analysis_primary_outcome()

  placebo <- run_placebo_lead_test(panel, outcome = outcome, treatment = treatment)
  loo <- run_leave_one_state_out(panel, outcome = outcome, treatment = treatment)
  sample_splits <- run_sample_split_fe(panel, outcome = outcome, treatment = treatment)
  run_sensemakr_lm(panel, outcome = outcome, treatment = treatment)

  alt <- analysis_alternative_treatments()
  if (length(alt) > 0 && alt[[1]] != treatment) {
    alt_tr <- alt[[1]]
    placebo <- dplyr::bind_rows(placebo,
      run_placebo_lead_test(panel, outcome = outcome, treatment = alt_tr))
    loo <- dplyr::bind_rows(loo,
      run_leave_one_state_out(panel, outcome = outcome, treatment = alt_tr))
    sample_splits <- dplyr::bind_rows(sample_splits,
      run_sample_split_fe(panel, outcome = outcome, treatment = alt_tr))
    run_sensemakr_lm(panel, outcome = outcome, treatment = alt_tr)
  }

  safe_write_csv(placebo, path_project(cfg$paths$tables_root, "placebo_lead_test.csv"))
  safe_write_csv(loo, path_project(cfg$paths$tables_root, "leave_one_state_out.csv"))
  safe_write_csv(sample_splits, path_project(cfg$paths$tables_root, "sample_split_fe.csv"))

  invisible(list(placebo = placebo, leave_one_out = loo, sample_splits = sample_splits))
}

if (sys.nframe() == 0) {
  panel <- read_panel()
  run_primary_sensitivity(panel)
}
