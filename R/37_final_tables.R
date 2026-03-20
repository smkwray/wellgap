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

safe_cr2_term_stats <- function(model, term, cluster) {
  tryCatch(
    cr2_term_stats(model, term, cluster),
    error = function(e) {
      tibble::tibble(cr2_se = NA_real_, cr2_t = NA_real_, cr2_df = NA_real_, cr2_p = NA_real_)
    }
  )
}

tidy_locked_model <- function(model, term, model_type, cluster) {
  cr2 <- safe_cr2_term_stats(model, term, cluster)
  inf_std <- if (all(is.na(cr2$cr2_se))) "conventional_clustered" else "CR2_Satterthwaite"
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(term == !!term) |>
    dplyr::mutate(model_type = model_type, n_obs = stats::nobs(model)) |>
    dplyr::bind_cols(cr2) |>
    dplyr::mutate(inference_standard = inf_std)
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
  sample <- prepare_causal_sample(panel)
  treatment <- analysis_primary_treatment()
  outcomes <- locked_wellbeing_outcomes()
  controls <- clean_control_set(sample, analysis_causal_core_controls())

  # Branch 1: latent_factor (CCE dynamic FE with CR2)
  cce_rows <- purrr::map_dfr(outcomes, function(y) {
    cce_ctrl <- clean_control_set(sample, analysis_causal_core_controls())
    tmp <- add_lag(sample, y, n = 1, group = "state", order = "year", new_name = "y_l1")
    base_vars <- unique(c(y, treatment, "y_l1", cce_ctrl))
    tmp <- add_cce_terms(tmp, base_vars)
    cce_terms <- paste0("cce_", base_vars)
    needed <- unique(c(y, treatment, "y_l1", cce_ctrl, cce_terms, "state", "year"))
    dat <- tmp |>
      dplyr::select(dplyr::all_of(needed)) |>
      tidyr::drop_na(dplyr::all_of(c(y, treatment, "y_l1")))

    mod <- run_cce_dynamic_wellbeing_fe(sample, outcome = y, treatment = treatment, controls = controls)
    tidy_locked_model(mod, treatment, "cce_dynamic_fe", dat$state) |>
      dplyr::mutate(outcome = y, treatment = treatment, branch = "latent_factor",
                    spec = "baseline", control_count = length(cce_ctrl))
  })

  # Branch 2: subgroup_reverse_causality (refit on age-65+ and retired subsamples)
  subgroup_panel <- read_brfss_subgroups() |>
    dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year))

  subgroup_rows <- purrr::map_dfr(unique(subgroup_panel$subgroup), function(sg) {
    dat_base <- sample |>
      dplyr::select(-dplyr::all_of(intersect(outcomes, names(sample)))) |>
      dplyr::left_join(
        subgroup_panel |>
          dplyr::filter(subgroup == sg) |>
          dplyr::select(state_fips, year, dplyr::all_of(outcomes)),
        by = c("state_fips", "year")
      )

    purrr::map_dfr(outcomes, function(y) {
      dat <- prepare_dynamic_wellbeing_data(dat_base, y, treatment, controls)
      mod <- run_dynamic_wellbeing_fe(dat_base, outcome = y, treatment = treatment, controls = controls)
      tidy_locked_model(mod, treatment, "dynamic_fe_subgroup", dat$state) |>
        dplyr::mutate(outcome = y, treatment = treatment, subgroup = sg,
                      branch = "subgroup_reverse_causality", spec = "baseline")
    })
  })

  # Branch 3: measurement sensitivity (age-standardized and precision-weighted)
  measurement_rows <- purrr::map_dfr(outcomes, function(y) {
    rows <- list()
    std_y <- standardized_outcome_name(y)
    raw_wt <- precision_weight_name(y)

    if (std_y %in% names(sample)) {
      dat <- prepare_dynamic_wellbeing_data(sample, std_y, treatment, controls)
      mod <- run_dynamic_wellbeing_fe(sample, outcome = std_y, treatment = treatment, controls = controls)
      rows[["std"]] <- tidy_locked_model(mod, treatment, "dynamic_fe_age_std", dat$state) |>
        dplyr::mutate(outcome = y, outcome_variant = std_y, treatment = treatment,
                      branch = "measurement", spec = "baseline")
    }

    if (raw_wt %in% names(sample)) {
      dat <- prepare_dynamic_wellbeing_data(sample, y, treatment, controls, weight_var = raw_wt)
      mod <- run_dynamic_wellbeing_fe(sample, outcome = y, treatment = treatment, controls = controls, weight_var = raw_wt)
      rows[["pw"]] <- tidy_locked_model(mod, treatment, "dynamic_fe_precision_weighted", dat$state) |>
        dplyr::mutate(outcome = y, outcome_variant = y, treatment = treatment,
                      weight_var = raw_wt, branch = "measurement", spec = "baseline")
    }

    dplyr::bind_rows(rows)
  })

  # Branch 4: within-BRFSS falsification (physical distress outcomes)
  fals_outcomes <- wellbeing_falsification_outcomes(sample)
  falsification_rows <- purrr::map_dfr(fals_outcomes, function(y) {
    dat <- prepare_dynamic_wellbeing_data(sample, y, treatment, controls)
    mod <- run_dynamic_wellbeing_fe(sample, outcome = y, treatment = treatment, controls = controls)
    tidy_locked_model(mod, treatment, "dynamic_fe_falsification", dat$state) |>
      dplyr::mutate(outcome = y, treatment = treatment,
                    branch = "within_brfss_falsification", spec = "baseline")
  })

  out <- dplyr::bind_rows(cce_rows, subgroup_rows, measurement_rows, falsification_rows)
  safe_write_csv(out, path_project(cfg$paths$tables_root, "final_robustness_wellbeing_table.csv"))
  invisible(out)
}

build_final_hard_outcome_table <- function() {
  out <- safe_read_csv(path_project(cfg$paths$tables_root, "wellbeing_hard_outcome_falsification.csv")) |>
    dplyr::mutate(inference_standard = "conventional_clustered") |>
    dplyr::arrange(p.value)
  safe_write_csv(out, path_project(cfg$paths$tables_root, "final_hard_outcome_table.csv"))
  invisible(out)
}

build_final_extension_outcomes_table <- function() {
  out <- safe_read_csv(path_project(cfg$paths$tables_root, "extended_outcomes_dynamic_fe.csv")) |>
    dplyr::mutate(inference_standard = "conventional_clustered") |>
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
