# 36_extended_outcomes.R
# Secondary wellbeing and material-wellbeing extension models.

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

source(file.path(project_root, "R", "35_causal_robustness.R"))

secondary_wellbeing_outcomes <- function(data) {
  candidates <- cfg_vec(cfg$analysis$secondary_wellbeing_outcomes %||% c(
    "frequent_physical_distress_rate",
    "mean_bad_physical_days",
    "all_cause_age_adjusted_rate",
    "cardiovascular_age_adjusted_rate"
  ))
  candidates[candidates %in% names(data)]
}

material_wellbeing_outcomes <- function(data) {
  candidates <- cfg_vec(cfg$analysis$material_wellbeing_outcomes %||% c(
    "log_rpp_adj_median_hh_income",
    "employment_to_population_ratio"
  ))
  candidates[candidates %in% names(data)]
}

run_extended_outcomes_dynamic_fe <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatments <- wellbeing_treatments(sample)
  specs <- analysis_model_specs(include_weighted = FALSE)

  outcome_grid <- dplyr::bind_rows(
    tibble::tibble(family = "secondary_wellbeing", outcome = secondary_wellbeing_outcomes(sample)),
    tibble::tibble(family = "material_wellbeing", outcome = material_wellbeing_outcomes(sample))
  ) |>
    dplyr::filter(!is.na(outcome), nzchar(outcome))

  out <- purrr::map_dfr(names(specs), function(spec_name) {
    spec <- specs[[spec_name]]
    control_count <- length(clean_control_set(sample, spec$controls))
    purrr::map_dfr(treatments, function(tr) {
      purrr::pmap_dfr(outcome_grid, function(family, outcome) {
        mod <- run_dynamic_wellbeing_fe(sample, outcome = outcome, treatment = tr, controls = spec$controls)
        tidy_primary_term(mod, tr, "dynamic_fe_extension") |>
          dplyr::mutate(
            family = family,
            spec = spec_name,
            outcome = outcome,
            treatment = tr,
            control_count = control_count
          )
      })
    })
  })

  out_path <- path_project(cfg$paths$tables_root, "extended_outcomes_dynamic_fe.csv")
  safe_write_csv(out, out_path)
  invisible(out)
}

if (sys.nframe() == 0) {
  run_extended_outcomes_dynamic_fe()
}
