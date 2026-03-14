# 30_model_fe.R
# Transparent baseline models: TWFE + local projections.

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

required_pkgs <- c("dplyr", "purrr", "stringr", "readr", "tibble", "tidyr", "fixest", "broom", "modelsummary")
invisible(lapply(required_pkgs, require, character.only = TRUE))

read_panel <- function() {
  panel_path <- path_project(cfg$paths$final_root, "state_year_panel.rds")
  if (!file.exists(panel_path)) stop("Missing final panel: ", panel_path, call. = FALSE)
  safe_read_rds(panel_path)
}

clean_control_set <- function(data, controls = analysis_primary_controls()) {
  controls <- controls[controls %in% names(data)]
  unique(controls)
}

build_fe_formula <- function(outcome, treatment, controls = NULL, fe = c("state", "year")) {
  rhs <- c(treatment, controls)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  stats::as.formula(
    paste0(
      outcome,
      " ~ ",
      paste(rhs, collapse = " + "),
      " | ",
      paste(fe, collapse = " + ")
    )
  )
}

run_twfe <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  cluster_var = cfg$analysis$cluster_var %||% "state",
  weight_var = NULL
) {
  controls <- clean_control_set(data, controls)
  needed <- unique(c(outcome, treatment, controls, "state", "year", weight_var))
  dat <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na(dplyr::all_of(c(outcome, treatment))) |>
    dplyr::mutate(state = as.factor(state), year = as.integer(year))

  fml <- build_fe_formula(outcome, treatment, controls)
  wt <- NULL
  if (!is.null(weight_var) && weight_var %in% names(dat)) wt <- stats::as.formula(paste0("~", weight_var))

  fixest::feols(
    fml = fml,
    data = dat,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    weights = wt,
    warn = FALSE
  )
}

tidy_fixest_model <- function(model, keep = NULL) {
  out <- broom::tidy(model, conf.int = TRUE)
  if (!is.null(keep)) out <- out |> dplyr::filter(term %in% keep)
  out
}

run_local_projections <- function(
  data,
  outcome,
  treatment,
  controls = analysis_primary_controls(),
  horizons = cfg_vec(cfg$analysis$horizons),
  cluster_var = cfg$analysis$cluster_var %||% "state",
  weight_var = NULL
) {
  controls <- clean_control_set(data, controls)

  purrr::map_dfr(horizons, function(h) {
    tmp <- data |>
      dplyr::group_by(state) |>
      dplyr::arrange(year, .by_group = TRUE) |>
      dplyr::mutate(
        y_h_raw = dplyr::lead(.data[[outcome]], h),
        y_h_year = dplyr::lead(year, h),
        y_h = if (h == 0) {
          .data[[outcome]]
        } else {
          dplyr::if_else(!is.na(y_h_year) & ((y_h_year - year) == h), y_h_raw, NA_real_)
        },
        y_l1_raw = dplyr::lag(.data[[outcome]], 1),
        y_l1_year = dplyr::lag(year, 1),
        y_l1 = dplyr::if_else(!is.na(y_l1_year) & ((year - y_l1_year) == 1), y_l1_raw, NA_real_)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-y_h_raw, -y_h_year, -y_l1_raw, -y_l1_year)

    extra_controls <- unique(c("y_l1", controls))
    extra_controls <- extra_controls[extra_controls %in% names(tmp)]

    mod <- run_twfe(
      data = tmp,
      outcome = "y_h",
      treatment = treatment,
      controls = extra_controls,
      cluster_var = cluster_var,
      weight_var = weight_var
    )

    tidy_fixest_model(mod, keep = treatment) |>
      dplyr::mutate(h = h, outcome = outcome, treatment = treatment)
  })
}

run_primary_fe_models <- function(panel = read_panel()) {
  specs <- analysis_model_specs(include_weighted = TRUE)
  treatments <- cfg_vec(cfg$analysis$primary_treatments)
  outcomes <- c(cfg_vec(cfg$analysis$primary_wellbeing_outcomes), cfg_vec(cfg$analysis$primary_consumption_outcomes))
  outcomes <- outcomes[outcomes %in% names(panel)]
  treatments <- treatments[treatments %in% names(panel)]

  if (length(outcomes) == 0 || length(treatments) == 0) {
    stop("No configured outcomes or treatments found in panel.", call. = FALSE)
  }

  models <- list()
  rows <- list()

  for (spec_name in names(specs)) {
    spec <- specs[[spec_name]]
    controls <- clean_control_set(panel, spec$controls)
    is_weighted <- !is.null(spec$weight_var)
    weight_name <- spec$weight_var %||% NA_character_

    for (tr in treatments) {
      for (y in outcomes) {
        mod_name <- paste(spec_name, y, tr, sep = "__")
        mod <- run_twfe(
          panel,
          outcome = y,
          treatment = tr,
          controls = controls,
          weight_var = spec$weight_var
        )
        models[[mod_name]] <- mod
        rows[[mod_name]] <- tidy_fixest_model(mod, keep = tr) |>
          dplyr::mutate(
            spec = spec_name,
            weighted = is_weighted,
            weight_var = weight_name,
            control_count = length(controls),
            outcome = y,
            treatment = tr,
            model = mod_name,
            n_obs = stats::nobs(mod)
          )
      }
    }
  }

  coef_df <- dplyr::bind_rows(rows)
  baseline_coef_df <- coef_df |> dplyr::filter(spec == "baseline")
  safe_write_csv(coef_df, path_project(cfg$paths$tables_root, "fe_spec_coefficients.csv"))
  safe_write_csv(baseline_coef_df, path_project(cfg$paths$tables_root, "baseline_fe_coefficients.csv"))
  baseline_models <- models[grepl("^baseline__", names(models))]
  capture.output(fixest::etable(models, tex = FALSE), file = path_project(cfg$paths$tables_root, "fe_spec_table.txt"))
  capture.output(fixest::etable(baseline_models, tex = FALSE), file = path_project(cfg$paths$tables_root, "baseline_fe_table.txt"))

  invisible(list(models = models, coefficients = coef_df, specs = specs))
}

run_primary_local_projections <- function(panel = read_panel()) {
  specs <- analysis_model_specs(include_weighted = TRUE)
  treatments <- cfg_vec(cfg$analysis$primary_treatments)
  outcomes <- c(cfg_vec(cfg$analysis$primary_wellbeing_outcomes), cfg_vec(cfg$analysis$primary_consumption_outcomes))
  outcomes <- outcomes[outcomes %in% names(panel)]
  treatments <- treatments[treatments %in% names(panel)]

  lp_df <- purrr::map_dfr(names(specs), function(spec_name) {
    spec <- specs[[spec_name]]
    controls <- clean_control_set(panel, spec$controls)
    is_weighted <- !is.null(spec$weight_var)
    weight_name <- spec$weight_var %||% NA_character_

    purrr::map_dfr(treatments, function(tr) {
      purrr::map_dfr(outcomes, function(y) {
        run_local_projections(
          panel,
          outcome = y,
          treatment = tr,
          controls = controls,
          weight_var = spec$weight_var
        ) |>
          dplyr::mutate(
            spec = spec_name,
            weighted = is_weighted,
            weight_var = weight_name,
            control_count = length(controls)
          )
      })
    })
  })

  safe_write_csv(lp_df, path_project(cfg$paths$tables_root, "local_projection_coefficients.csv"))
  invisible(lp_df)
}

if (sys.nframe() == 0) {
  panel <- read_panel()
  run_primary_fe_models(panel)
  run_primary_local_projections(panel)
}
