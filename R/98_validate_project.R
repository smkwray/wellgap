# 98_validate_project.R
# Project-level validation and smoke checks for the state-year pipeline.

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

required_pkgs <- c("dplyr", "purrr", "tibble", "readr")
invisible(lapply(required_pkgs, require, character.only = TRUE))

read_panel <- function() {
  panel_path <- path_project(cfg$paths$final_root, "state_year_panel.rds")
  if (!file.exists(panel_path)) stop("Missing final panel: ", panel_path, call. = FALSE)
  safe_read_rds(panel_path)
}

make_check <- function(check, ok, detail, severity = "error") {
  tibble::tibble(
    check = check,
    status = if (isTRUE(ok)) "PASS" else if (identical(severity, "warn")) "WARN" else "FAIL",
    severity = severity,
    detail = as.character(detail)
  )
}

validate_required_files <- function(require_model_outputs = TRUE) {
  files <- c(
    "data/intermediate/bea_state_core.csv",
    "data/intermediate/bea_distribution_state_year.csv",
    "data/intermediate/acs_state_year.csv",
    "data/intermediate/brfss_state_year.csv",
    "data/intermediate/brfss_state_year_subgroups.csv",
    "data/intermediate/brfss_age_reference_weights.csv",
    "data/intermediate/cdc_wonder_state_year.csv",
    "data/intermediate/fhfa_state_hpi.csv",
    "data/intermediate/building_permits_state_year.csv",
    "data/intermediate/state_minimum_wage.csv",
    "data/intermediate/union_membership_state_year.csv",
    "data/intermediate/qcew_state_year.csv",
    "data/final/state_year_panel.rds"
  )

  if (isTRUE(require_model_outputs)) {
    files <- c(
      files,
      "output/tables/baseline_fe_coefficients.csv",
      "output/tables/fe_spec_coefficients.csv",
      "output/tables/local_projection_coefficients.csv",
      "output/tables/dml_results.csv",
      "output/tables/placebo_lead_test.csv",
      "output/tables/leave_one_state_out.csv",
      "output/tables/sample_split_fe.csv",
      "output/tables/event_study_coefficients.csv",
      "output/tables/wellbeing_dynamic_fe.csv",
      "output/tables/wellbeing_longdiff_coefficients.csv",
      "output/tables/wellbeing_cce_dynamic_fe.csv",
      "output/tables/wellbeing_measurement_sensitivity.csv",
      "output/tables/wellbeing_falsification_dynamic_fe.csv",
      "output/tables/wellbeing_hard_outcome_falsification.csv",
      "output/tables/extended_outcomes_dynamic_fe.csv",
      "output/tables/final_main_wellbeing_table.csv",
      "output/tables/final_robustness_wellbeing_table.csv",
      "output/tables/final_hard_outcome_table.csv",
      "output/tables/final_extension_outcomes_table.csv",
      "output/tables/wellbeing_subgroup_dynamic_fe.csv",
      "output/tables/causal_forest_ate.csv",
      "output/tables/causal_forest_importance.csv"
    )
  }

  purrr::map_dfr(files, function(rel_path) {
    abs_path <- path_project(rel_path)
    make_check(
      check = paste("file_exists", rel_path),
      ok = file.exists(abs_path),
      detail = rel_path
    )
  })
}

validate_panel_structure <- function(panel) {
  expected_years <- cfg_vec(cfg$windows$brfss_years %||% cfg$windows$acs_years)
  expected_states <- nrow(state_lookup())
  expected_rows <- expected_states * length(expected_years)
  actual_years <- sort(unique(panel$year))

  key_vars <- c(
    "acs_gini", "frequent_mental_distress_rate", "fair_poor_health_rate",
    "log_real_pce_pc", "disp_mean_median_gap_z", "disp_top10_share_z", "l1_acs_gini_z",
    "l1_disp_top10_share_z",
    "l1_disp_mean_median_gap_z", "l1_hpi_yoy", "l1_permits_per_1000",
    "l1_state_min_wage_nominal", "l1_union_membership_rate",
    "qcew_private_avg_wkly_wage", "l1_qcew_private_avg_wkly_wage",
    "frequent_mental_distress_rate_age_std", "fair_poor_health_rate_age_std",
    "frequent_mental_distress_rate_precision_wt", "fair_poor_health_rate_precision_wt",
    "l1_qcew_manufacturing_share",
    "suicide_age_adjusted_rate", "drug_poisoning_age_adjusted_rate",
    "all_cause_age_adjusted_rate", "cardiovascular_age_adjusted_rate",
    "employment_to_population_ratio", "log_rpp_adj_median_hh_income"
  )
  key_vars <- key_vars[key_vars %in% names(panel)]

  checks <- list(
    make_check(
      "panel_row_count",
      nrow(panel) == expected_rows,
      sprintf("rows=%s expected=%s", nrow(panel), expected_rows)
    ),
    make_check(
      "panel_state_count",
      dplyr::n_distinct(panel$state_fips) == expected_states,
      sprintf("states=%s expected=%s", dplyr::n_distinct(panel$state_fips), expected_states)
    ),
    make_check(
      "panel_years_match",
      identical(actual_years, sort(expected_years)),
      sprintf("years=%s", paste(actual_years, collapse = ","))
    ),
    make_check(
      "panel_no_duplicates",
      nrow(panel) == nrow(dplyr::distinct(panel, state_fips, year)),
      sprintf("duplicates=%s", nrow(panel) - nrow(dplyr::distinct(panel, state_fips, year)))
    ),
    make_check(
      "panel_2020_omitted",
      !any(panel$year == 2020),
      sprintf("has_2020=%s", any(panel$year == 2020))
    ),
    make_check(
      "panel_2021_lags_missing",
      sum(!is.na(panel$l1_acs_gini_z[panel$year == 2021])) == 0 &&
        sum(!is.na(panel$l1_disp_mean_median_gap_z[panel$year == 2021])) == 0,
      sprintf(
        "2021_non_missing_l1_acs_gini_z=%s 2021_non_missing_l1_disp_mean_median_gap_z=%s",
        sum(!is.na(panel$l1_acs_gini_z[panel$year == 2021])),
        sum(!is.na(panel$l1_disp_mean_median_gap_z[panel$year == 2021]))
      )
    )
  )

  coverage <- purrr::map_dfr(key_vars, function(var) {
    non_missing <- sum(!is.na(panel[[var]]))
    threshold <- dplyr::case_when(
      grepl("^l1_", var) ~ 500,
      grepl("^disp_", var) ~ 550,
      grepl("suicide_|drug_poisoning_|all_cause_|cardiovascular_", var) ~ 400,
      TRUE ~ 600
    )
    make_check(
      check = paste("coverage", var),
      ok = non_missing >= threshold,
      detail = sprintf("non_missing=%s threshold=%s", non_missing, threshold),
      severity = "warn"
    )
  })

  dplyr::bind_rows(checks, coverage)
}

validate_output_shapes <- function() {
  checks <- list()

  if (file.exists(path_project("output/tables/fe_spec_coefficients.csv"))) {
    fe <- safe_read_csv(path_project("output/tables/fe_spec_coefficients.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "fe_specs_present",
      all(c("baseline", "weighted") %in% fe$spec),
      sprintf("specs=%s", paste(sort(unique(fe$spec)), collapse = ","))
    )
    if ("treatment" %in% names(fe)) {
      checks[[length(checks) + 1]] <- make_check(
        "fe_distribution_treatment_present",
        "l1_disp_mean_median_gap_z" %in% fe$treatment,
        sprintf("treatments=%s", paste(sort(unique(fe$treatment)), collapse = ","))
      )
    }
  }

  if (file.exists(path_project("output/tables/dml_results.csv"))) {
    dml <- safe_read_csv(path_project("output/tables/dml_results.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "dml_specs_present",
      "baseline" %in% dml$spec,
      sprintf("specs=%s", paste(sort(unique(dml$spec)), collapse = ","))
    )
    if ("treatment" %in% names(dml)) {
      checks[[length(checks) + 1]] <- make_check(
        "dml_distribution_treatment_present",
        "l1_disp_mean_median_gap_z" %in% dml$treatment,
        sprintf("treatments=%s", paste(sort(unique(dml$treatment)), collapse = ","))
      )
    }
  }

  if (file.exists(path_project("output/tables/sample_split_fe.csv"))) {
    split <- safe_read_csv(path_project("output/tables/sample_split_fe.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "sample_split_rows",
      nrow(split) >= 4,
      sprintf("rows=%s", nrow(split))
    )
  }

  if (file.exists(path_project("output/tables/wellbeing_dynamic_fe.csv"))) {
    dyn <- safe_read_csv(path_project("output/tables/wellbeing_dynamic_fe.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "wellbeing_dynamic_primary_treatment_present",
      "l1_disp_top10_share_z" %in% dyn$treatment,
      sprintf("treatments=%s", paste(sort(unique(dyn$treatment)), collapse = ","))
    )
  }

  if (file.exists(path_project("output/tables/wellbeing_subgroup_dynamic_fe.csv"))) {
    subgroup <- safe_read_csv(path_project("output/tables/wellbeing_subgroup_dynamic_fe.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "wellbeing_subgroups_present",
      all(c("age_65_plus", "retired") %in% subgroup$subgroup),
      sprintf("subgroups=%s", paste(sort(unique(subgroup$subgroup)), collapse = ","))
    )
  }

  if (file.exists(path_project("output/tables/wellbeing_cce_dynamic_fe.csv"))) {
    cce <- safe_read_csv(path_project("output/tables/wellbeing_cce_dynamic_fe.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "wellbeing_cce_primary_treatment_present",
      "l1_disp_top10_share_z" %in% cce$treatment,
      sprintf("treatments=%s", paste(sort(unique(cce$treatment)), collapse = ","))
    )
  }

  if (file.exists(path_project("output/tables/wellbeing_measurement_sensitivity.csv"))) {
    meas <- safe_read_csv(path_project("output/tables/wellbeing_measurement_sensitivity.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "wellbeing_measurement_variants_present",
      all(c("dynamic_fe_age_std", "dynamic_fe_precision_weighted") %in% meas$model_type),
      sprintf("model_types=%s", paste(sort(unique(meas$model_type)), collapse = ","))
    )
  }

  if (file.exists(path_project("output/tables/wellbeing_falsification_dynamic_fe.csv"))) {
    fals <- safe_read_csv(path_project("output/tables/wellbeing_falsification_dynamic_fe.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "wellbeing_falsification_outcomes_present",
      all(c("frequent_physical_distress_rate", "mean_bad_physical_days") %in% fals$outcome),
      sprintf("outcomes=%s", paste(sort(unique(fals$outcome)), collapse = ","))
    )
  }

  if (file.exists(path_project("output/tables/wellbeing_hard_outcome_falsification.csv"))) {
    hard <- safe_read_csv(path_project("output/tables/wellbeing_hard_outcome_falsification.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "wellbeing_hard_outcomes_present",
      all(c("suicide_age_adjusted_rate", "drug_poisoning_age_adjusted_rate") %in% hard$outcome),
      sprintf("outcomes=%s", paste(sort(unique(hard$outcome)), collapse = ","))
    )
  }

  if (file.exists(path_project("output/tables/extended_outcomes_dynamic_fe.csv"))) {
    ext <- safe_read_csv(path_project("output/tables/extended_outcomes_dynamic_fe.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "extended_outcome_families_present",
      all(c("secondary_wellbeing", "material_wellbeing") %in% ext$family),
      sprintf("families=%s", paste(sort(unique(ext$family)), collapse = ","))
    )
  }

  if (file.exists(path_project("output/tables/final_main_wellbeing_table.csv"))) {
    final_main <- safe_read_csv(path_project("output/tables/final_main_wellbeing_table.csv"))
    checks[[length(checks) + 1]] <- make_check(
      "final_main_table_primary_treatment_present",
      analysis_primary_treatment() %in% final_main$treatment,
      sprintf("treatments=%s", paste(sort(unique(final_main$treatment)), collapse = ","))
    )
  }

  if (length(checks) == 0) return(tibble::tibble())
  dplyr::bind_rows(checks)
}

write_validation_report <- function(checks) {
  csv_path <- path_project(cfg$paths$logs_root, "validation_checks.csv")
  md_path <- path_project(cfg$paths$logs_root, "validation_report.md")
  safe_write_csv(checks, csv_path)

  lines <- c(
    "# Validation Report",
    "",
    sprintf("- Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("- PASS: %s", sum(checks$status == "PASS")),
    sprintf("- WARN: %s", sum(checks$status == "WARN")),
    sprintf("- FAIL: %s", sum(checks$status == "FAIL")),
    "",
    "| Check | Status | Detail |",
    "|---|---|---|"
  )

  detail_lines <- sprintf("| %s | %s | %s |", checks$check, checks$status, checks$detail)
  writeLines(c(lines, detail_lines), md_path)

  list(csv = csv_path, md = md_path)
}

run_project_validation <- function(panel = NULL, require_model_outputs = TRUE) {
  panel <- panel %||% read_panel()
  checks <- dplyr::bind_rows(
    validate_required_files(require_model_outputs = require_model_outputs),
    validate_panel_structure(panel),
    validate_output_shapes()
  )

  outputs <- write_validation_report(checks)
  note("Validation report written to {outputs$md}")

  if (any(checks$status == "FAIL")) {
    stop("Project validation failed. See ", outputs$md, call. = FALSE)
  }

  invisible(checks)
}

if (sys.nframe() == 0) {
  run_project_validation()
}
