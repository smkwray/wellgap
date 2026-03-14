# 32_model_event_study.R
# Event-study scaffold for a discrete inequality-shock design.
# Use only if you can justify a treatment-timing interpretation.

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

required_pkgs <- c("dplyr", "fixest", "broom")
invisible(lapply(required_pkgs, require, character.only = TRUE))

read_panel <- function() {
  panel_path <- path_project(cfg$paths$final_root, "state_year_panel.rds")
  if (!file.exists(panel_path)) stop("Missing final panel: ", panel_path, call. = FALSE)
  safe_read_rds(panel_path)
}

make_first_shock_timing <- function(data, shock_var = "large_acs_gini_shock") {
  if (!(shock_var %in% names(data))) stop("shock_var not found in data.", call. = FALSE)

  timing <- data |>
    dplyr::group_by(state) |>
    dplyr::summarise(
      first_shock_year = {
        shock_idx <- which(.data[[shock_var]] %in% 1)
        shock_years <- year[shock_idx]
        if (length(shock_years) == 0) NA_integer_ else min(shock_years, na.rm = TRUE)
      },
      .groups = "drop"
    )

  data |>
    dplyr::left_join(timing, by = "state")
}

run_sunab_event_study <- function(
  data,
  outcome,
  first_treat_var = "first_shock_year",
  controls = cfg_vec(cfg$analysis$core_controls),
  cluster_var = cfg$analysis$cluster_var %||% "state"
) {
  controls <- controls[controls %in% names(data)]
  rhs <- paste(c(sprintf("sunab(%s, year)", first_treat_var), controls), collapse = " + ")
  fml <- as.formula(paste0(outcome, " ~ ", rhs, " | state + year"))

  fixest::feols(
    fml = fml,
    data = data,
    cluster = stats::as.formula(paste0("~", cluster_var)),
    warn = FALSE
  )
}

tidy_event_study <- function(model) {
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(grepl("^year::", term))
}

if (sys.nframe() == 0) {
  panel <- read_panel()
  if ("large_acs_gini_shock" %in% names(panel)) {
    panel2 <- make_first_shock_timing(panel)
    mod <- run_sunab_event_study(panel2, outcome = cfg$analysis$primary_wellbeing_outcomes[[1]])
    out <- tidy_event_study(mod)
    safe_write_csv(out, path_project(cfg$paths$tables_root, "event_study_coefficients.csv"))
  }
}
