# 99_run_all.R
# Convenience orchestrator for the full R-first workflow.

source(file.path(getwd(), "R", "01_helpers.R"))

run_safely <- function(expr, label) {
  note("--- Running: {label} ---")
  out <- try(eval.parent(substitute(expr)), silent = TRUE)
  if (inherits(out, "try-error")) {
    message("Task failed: ", label)
    message(as.character(out))
    return(NULL)
  }
  out
}

run_core_pipeline <- function() {
  ensure_project_dirs()

  source(path_project("R", "10_download_acs.R"))
  source(path_project("R", "11_download_bea.R"))
  source(path_project("R", "12_download_brfss.R"))
  source(path_project("R", "15_download_mortality.R"))
  source(path_project("R", "14_download_controls.R"))
  source(path_project("R", "20_build_panel.R"))
  source(path_project("R", "30_model_fe.R"))
  source(path_project("R", "31_model_dml.R"))
  source(path_project("R", "32_model_event_study.R"))
  source(path_project("R", "33_model_grf.R"))
  source(path_project("R", "34_sensitivity.R"))
  source(path_project("R", "35_causal_robustness.R"))
  source(path_project("R", "36_extended_outcomes.R"))
  source(path_project("R", "37_final_tables.R"))
  source(path_project("R", "98_validate_project.R"))

  run_safely(download_acs_state_panel(), "ACS download")

  # BEA workflow: metadata discovery is safe even before config is finalized.
  run_safely({
    discover_bea_datasets()
    discover_bea_parameters()
    search_bea_tables("consumer spending")
    search_bea_tables("regional price")
    search_bea_tables("personal income")
    discover_bea_distribution_downloads()
  }, "BEA metadata discovery")

  run_safely(build_bea_distribution_state_year(), "BEA distribution build")

  # Uncomment after filling config$bea$tables with real table names:
  # run_safely({
  #   download_configured_bea_aliases()
  #   build_bea_state_core()
  # }, "BEA configured downloads")

  run_safely(build_brfss_panel(), "BRFSS build")
  run_safely(build_brfss_subgroup_panel(), "BRFSS subgroup build")
  run_safely(build_cdc_wonder_state_year(), "CDC WONDER hard-outcome build")
  run_safely(build_fhfa_state_hpi(), "FHFA HPI build")
  run_safely(build_building_permits_state_year(), "Building permits build")
  run_safely(build_state_minimum_wage(), "Minimum wage build")
  run_safely(build_union_membership_state_year(), "Union membership build")
  run_safely(build_qcew_state_year(), "QCEW build")

  panel <- run_safely(build_state_year_panel(), "State-year panel build")
  if (is.null(panel)) return(invisible(NULL))

  run_safely(run_project_validation(panel, require_model_outputs = FALSE), "Pre-model validation")
  run_safely(run_primary_fe_models(panel), "Baseline FE models")
  run_safely(run_primary_local_projections(panel), "Local projections")
  run_safely(run_primary_dml(panel), "DML models")
  run_safely({
    if ("large_acs_gini_shock" %in% names(panel)) {
      panel2 <- make_first_shock_timing(panel)
      mod <- run_sunab_event_study(panel2, outcome = cfg$analysis$primary_wellbeing_outcomes[[1]])
      out <- tidy_event_study(mod)
      safe_write_csv(out, path_project(cfg$paths$tables_root, "event_study_coefficients.csv"))
    }
  }, "Event-study models")
  run_safely({
    shock_var <- if ("large_disp_gap_shock" %in% names(panel)) "large_disp_gap_shock" else "large_acs_gini_shock"
    out <- run_causal_forest_shock(panel, outcome = cfg$analysis$primary_wellbeing_outcomes[[1]], treatment = shock_var)
    safe_write_csv(out$ate, path_project(cfg$paths$tables_root, "causal_forest_ate.csv"))
    safe_write_csv(out$importance, path_project(cfg$paths$tables_root, "causal_forest_importance.csv"))
  }, "Causal forest models")
  run_safely(run_primary_sensitivity(panel), "Sensitivity checks")
  run_safely(run_primary_wellbeing_causal_models(panel), "Wellbeing dynamic/long-difference robustness")
  run_safely(run_latent_factor_wellbeing_sensitivity(panel), "Wellbeing latent-factor sensitivity")
  run_safely(run_measurement_wellbeing_sensitivity(panel), "Wellbeing measurement sensitivity")
  run_safely(run_falsification_outcome_checks(panel), "Wellbeing falsification checks")
  run_safely(run_hard_outcome_falsification_checks(panel), "Wellbeing hard-outcome falsification checks")
  run_safely(run_subgroup_reverse_causality_checks(panel), "Wellbeing subgroup reverse-causality checks")
  run_safely(run_extended_outcomes_dynamic_fe(panel), "Extended wellbeing/material outcomes")
  run_safely(run_final_table_builds(panel), "Final locked tables")
  run_safely(run_project_validation(panel, require_model_outputs = TRUE), "Post-model validation")

  invisible(panel)
}

run_core_pipeline()
