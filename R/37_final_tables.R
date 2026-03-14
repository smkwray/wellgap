# 37_final_tables.R
# Locked final tables for the public-facing writeup.

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
source(file.path(project_root, "R", "36_extended_outcomes.R"))

required_pkgs <- c("dplyr", "purrr", "tibble", "readr", "clubSandwich")
invisible(lapply(required_pkgs, require, character.only = TRUE))

cr2_term_stats <- function(model, term, cluster) {
  cluster_used <- cluster[fixest::obs(model)]
  out <- clubSandwich::coef_test(model, vcov = "CR2", cluster = cluster_used, test = "Satterthwaite")
  row <- out[out$Coef == term, , drop = FALSE]

  tibble::tibble(
    cr2_se = row$SE[[1]],
    cr2_t = row$tstat[[1]],
    cr2_df = row$df_Satt[[1]],
    cr2_p = row$p_Satt[[1]]
  )
}

tidy_locked_model <- function(model, term, model_type, cluster) {
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term == !!term) |>
    dplyr::mutate(model_type = model_type, n_obs = stats::nobs(model)) |>
    dplyr::bind_cols(cr2_term_stats(model, term, cluster))
}

locked_wellbeing_outcomes <- function() {
  c(analysis_primary_outcome(), analysis_secondary_outcome()) |>
    unique()
}

build_final_main_wellbeing_table <- function(panel = read_panel()) {
  sample <- prepare_causal_sample(panel)
  treatment <- analysis_primary_treatment()
  outcomes <- locked_wellbeing_outcomes()
  specs <- list(
    baseline = list(controls = analysis_causal_core_controls()),
    rich = list(controls = analysis_rich_controls())
  )

  dynamic_rows <- purrr::map_dfr(names(specs), function(spec_name) {
    controls <- clean_control_set(sample, specs[[spec_name]]$controls)
    purrr::map_dfr(outcomes, function(outcome) {
      dat <- prepare_dynamic_wellbeing_data(sample, outcome, treatment, controls)
      mod <- run_dynamic_wellbeing_fe(sample, outcome = outcome, treatment = treatment, controls = controls)
      tidy_locked_model(mod, treatment, "dynamic_fe", dat$state) |>
        dplyr::mutate(
          spec = spec_name,
          outcome = outcome,
          treatment = treatment,
          control_set = if (spec_name == "baseline") "causal_core" else "rich"
        )
    })
  })

  longdiff_rows <- purrr::map_dfr(names(specs), function(spec_name) {
    controls <- clean_control_set(sample, specs[[spec_name]]$controls)
    purrr::map_dfr(outcomes, function(outcome) {
      diff_n <- wellbeing_longdiff_horizon(outcome)
      prep <- prepare_long_difference_data(sample, outcome, treatment, controls, diff_n = diff_n)
      mod <- run_long_difference_wellbeing(sample, outcome = outcome, treatment = treatment, controls = controls, diff_n = diff_n)
      tidy_locked_model(mod, prep$treatment_var, paste0("long_diff_", diff_n, "y"), prep$data$state) |>
        dplyr::mutate(
          spec = spec_name,
          outcome = outcome,
          treatment = treatment,
          diff_n = diff_n,
          control_set = if (spec_name == "baseline") "causal_core" else "rich"
        )
    })
  })

  out <- dplyr::bind_rows(dynamic_rows, longdiff_rows)
  safe_write_csv(out, path_project(cfg$paths$tables_root, "final_main_wellbeing_table.csv"))
  invisible(out)
}

build_final_wellbeing_robustness_table <- function(panel = read_panel()) {
  treatment <- analysis_primary_treatment()
  outcomes <- locked_wellbeing_outcomes()

  cce <- safe_read_csv(path_project(cfg$paths$tables_root, "wellbeing_cce_dynamic_fe.csv")) |>
    dplyr::filter(treatment == !!treatment, outcome %in% outcomes) |>
    dplyr::mutate(branch = "latent_factor")

  subgroup <- safe_read_csv(path_project(cfg$paths$tables_root, "wellbeing_subgroup_dynamic_fe.csv")) |>
    dplyr::filter(treatment == !!treatment, outcome %in% outcomes) |>
    dplyr::mutate(branch = "subgroup_reverse_causality")

  measurement <- safe_read_csv(path_project(cfg$paths$tables_root, "wellbeing_measurement_sensitivity.csv")) |>
    dplyr::filter(treatment == !!treatment, outcome %in% outcomes) |>
    dplyr::mutate(branch = "measurement")

  falsification <- safe_read_csv(path_project(cfg$paths$tables_root, "wellbeing_falsification_dynamic_fe.csv")) |>
    dplyr::mutate(branch = "within_brfss_falsification")

  out <- dplyr::bind_rows(cce, subgroup, measurement, falsification)
  safe_write_csv(out, path_project(cfg$paths$tables_root, "final_robustness_wellbeing_table.csv"))
  invisible(out)
}

build_final_hard_outcome_table <- function() {
  out <- safe_read_csv(path_project(cfg$paths$tables_root, "wellbeing_hard_outcome_falsification.csv")) |>
    dplyr::arrange(p.value)
  safe_write_csv(out, path_project(cfg$paths$tables_root, "final_hard_outcome_table.csv"))
  invisible(out)
}

build_final_extension_outcomes_table <- function() {
  out <- safe_read_csv(path_project(cfg$paths$tables_root, "extended_outcomes_dynamic_fe.csv")) |>
    dplyr::arrange(p.value)
  safe_write_csv(out, path_project(cfg$paths$tables_root, "final_extension_outcomes_table.csv"))
  invisible(out)
}

run_final_table_builds <- function(panel = read_panel()) {
  main <- build_final_main_wellbeing_table(panel)
  robustness <- build_final_wellbeing_robustness_table(panel)
  hard <- build_final_hard_outcome_table()
  ext <- build_final_extension_outcomes_table()
  invisible(list(main = main, robustness = robustness, hard = hard, extension = ext))
}

if (sys.nframe() == 0) {
  run_final_table_builds()
}
