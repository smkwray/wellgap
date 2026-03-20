# 01_helpers.R
# Helper functions used across the project.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

find_project_root <- function(start = getwd()) {
  cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(cur, "config", "config.yml"))) return(cur)
    parent <- dirname(cur)
    if (identical(parent, cur)) {
      stop("Could not find project root containing config/config.yml", call. = FALSE)
    }
    cur <- parent
  }
}

project_root <- get0("project_root", ifnotfound = find_project_root())
cfg <- yaml::read_yaml(file.path(project_root, "config", "config.yml"))

path_project <- function(...) {
  file.path(project_root, ...)
}

cfg_vec <- function(x) {
  if (is.null(x)) return(NULL)
  unlist(x, use.names = FALSE)
}

analysis_final_estimand <- function() {
  cfg$analysis$final_estimand %||% list()
}

analysis_primary_outcome <- function() {
  analysis_final_estimand()$primary_outcome %||% cfg_vec(cfg$analysis$primary_wellbeing_outcomes)[[1]]
}

analysis_secondary_outcome <- function() {
  analysis_final_estimand()$secondary_outcome %||% cfg_vec(cfg$analysis$primary_wellbeing_outcomes)[[2]]
}

analysis_primary_treatment <- function() {
  cfg$analysis$final_estimand$primary_treatment %||%
    cfg$analysis$primary_treatment %||%
    analysis_wellbeing_primary_treatments()[[1]]
}

analysis_alternative_treatments <- function() {
  cfg_vec(cfg$analysis$alternative_treatments %||% cfg$analysis$primary_treatments)
}

analysis_all_treatments <- function() {
  unique(c(analysis_primary_treatment(), analysis_alternative_treatments()))
}

analysis_causal_core_controls <- function() {
  cfg_vec(cfg$analysis$causal_core_controls %||% cfg$analysis$baseline_controls %||% cfg$analysis$core_controls)
}

analysis_primary_controls <- function() {
  analysis_causal_core_controls()
}

analysis_rich_controls <- function() {
  unique(c(analysis_primary_controls(), cfg_vec(cfg$analysis$rich_controls %||% cfg$analysis$core_controls)))
}

analysis_causal_main_years <- function() {
  cfg_vec(cfg$analysis$causal_main_years %||% cfg$windows$brfss_years %||% cfg$windows$acs_years)
}

analysis_wellbeing_primary_treatments <- function() {
  cfg_vec(cfg$analysis$wellbeing_primary_treatments %||%
    c(cfg$analysis$primary_treatment, cfg$analysis$alternative_treatments))
}

analysis_wellbeing_secondary_treatments <- function() {
  cfg_vec(cfg$analysis$wellbeing_secondary_treatments)
}

analysis_model_specs <- function(include_weighted = TRUE) {
  baseline <- unique(analysis_primary_controls())
  rich <- unique(analysis_rich_controls())
  same_as_baseline <- setequal(baseline, rich) && length(baseline) == length(rich)

  specs <- list(
    baseline = list(controls = baseline, weight_var = NULL)
  )

  if (!same_as_baseline) {
    specs$rich <- list(controls = rich, weight_var = NULL)
  }

  if (isTRUE(include_weighted) && !is.null(cfg$analysis$population_weight_var)) {
    specs$weighted <- list(
      controls = baseline,
      weight_var = cfg$analysis$population_weight_var
    )
  }

  specs
}

note <- function(..., .envir = parent.frame()) {
  message(glue::glue(..., .envir = .envir))
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

ensure_project_dirs <- function() {
  dirs <- c(
    cfg$paths$raw_root,
    cfg$paths$intermediate_root,
    cfg$paths$final_root,
    cfg$paths$figures_root,
    cfg$paths$tables_root,
    cfg$paths$logs_root,
    file.path(cfg$paths$raw_root, "acs"),
    file.path(cfg$paths$raw_root, "bea"),
    file.path(cfg$paths$raw_root, "brfss"),
    file.path(cfg$paths$raw_root, "hps"),
    file.path(cfg$paths$raw_root, "controls"),
    file.path(cfg$paths$raw_root, "cdc_wonder"),
    file.path(cfg$paths$raw_root, "cex")
  )
  invisible(lapply(path_project(dirs), ensure_dir))
  invisible(dirs)
}

download_if_needed <- function(url, dest, overwrite = FALSE, quiet = FALSE) {
  ensure_dir(dirname(dest))
  if (!file.exists(dest) || isTRUE(overwrite)) {
    if (!quiet) note("Downloading {basename(dest)}")
    utils::download.file(url = url, destfile = dest, mode = "wb", quiet = quiet)
  } else {
    if (!quiet) note("Skipping existing file {basename(dest)}")
  }
  dest
}

safe_read_csv <- function(path, ...) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE, ...)
}

safe_write_csv <- function(df, path) {
  ensure_dir(dirname(path))
  readr::write_csv(df, path)
  path
}

safe_write_rds <- function(obj, path) {
  ensure_dir(dirname(path))
  saveRDS(obj, path)
  path
}

safe_read_rds <- function(path) {
  readRDS(path)
}

state_lookup <- function() {
  tibble::tribble(
    ~state_fips, ~state, ~state_abbr,
    "01", "Alabama", "AL",
    "02", "Alaska", "AK",
    "04", "Arizona", "AZ",
    "05", "Arkansas", "AR",
    "06", "California", "CA",
    "08", "Colorado", "CO",
    "09", "Connecticut", "CT",
    "10", "Delaware", "DE",
    "11", "District of Columbia", "DC",
    "12", "Florida", "FL",
    "13", "Georgia", "GA",
    "15", "Hawaii", "HI",
    "16", "Idaho", "ID",
    "17", "Illinois", "IL",
    "18", "Indiana", "IN",
    "19", "Iowa", "IA",
    "20", "Kansas", "KS",
    "21", "Kentucky", "KY",
    "22", "Louisiana", "LA",
    "23", "Maine", "ME",
    "24", "Maryland", "MD",
    "25", "Massachusetts", "MA",
    "26", "Michigan", "MI",
    "27", "Minnesota", "MN",
    "28", "Mississippi", "MS",
    "29", "Missouri", "MO",
    "30", "Montana", "MT",
    "31", "Nebraska", "NE",
    "32", "Nevada", "NV",
    "33", "New Hampshire", "NH",
    "34", "New Jersey", "NJ",
    "35", "New Mexico", "NM",
    "36", "New York", "NY",
    "37", "North Carolina", "NC",
    "38", "North Dakota", "ND",
    "39", "Ohio", "OH",
    "40", "Oklahoma", "OK",
    "41", "Oregon", "OR",
    "42", "Pennsylvania", "PA",
    "44", "Rhode Island", "RI",
    "45", "South Carolina", "SC",
    "46", "South Dakota", "SD",
    "47", "Tennessee", "TN",
    "48", "Texas", "TX",
    "49", "Utah", "UT",
    "50", "Vermont", "VT",
    "51", "Virginia", "VA",
    "53", "Washington", "WA",
    "54", "West Virginia", "WV",
    "55", "Wisconsin", "WI",
    "56", "Wyoming", "WY"
  )
}

state_division_lookup <- function() {
  tibble::tribble(
    ~state_fips, ~division,
    "09", "New England",
    "23", "New England",
    "25", "New England",
    "33", "New England",
    "44", "New England",
    "50", "New England",
    "34", "Middle Atlantic",
    "36", "Middle Atlantic",
    "42", "Middle Atlantic",
    "17", "East North Central",
    "18", "East North Central",
    "26", "East North Central",
    "39", "East North Central",
    "55", "East North Central",
    "19", "West North Central",
    "20", "West North Central",
    "27", "West North Central",
    "29", "West North Central",
    "31", "West North Central",
    "38", "West North Central",
    "46", "West North Central",
    "10", "South Atlantic",
    "11", "South Atlantic",
    "12", "South Atlantic",
    "13", "South Atlantic",
    "24", "South Atlantic",
    "37", "South Atlantic",
    "45", "South Atlantic",
    "51", "South Atlantic",
    "54", "South Atlantic",
    "01", "East South Central",
    "21", "East South Central",
    "28", "East South Central",
    "47", "East South Central",
    "05", "West South Central",
    "22", "West South Central",
    "40", "West South Central",
    "48", "West South Central",
    "04", "Mountain",
    "08", "Mountain",
    "16", "Mountain",
    "30", "Mountain",
    "32", "Mountain",
    "35", "Mountain",
    "49", "Mountain",
    "56", "Mountain",
    "02", "Pacific",
    "06", "Pacific",
    "15", "Pacific",
    "41", "Pacific",
    "53", "Pacific"
  )
}

standardize_state_fips <- function(x) {
  x <- stringr::str_extract(as.character(x), "\\d+")
  x <- dplyr::if_else(
    !is.na(x) & nchar(x) > 2 & stringr::str_detect(x, "^\\d{2}0+$"),
    substr(x, 1, 2),
    x
  )
  stringr::str_pad(x, width = 2, side = "left", pad = "0")
}

zscore <- function(x) {
  if (all(is.na(x))) return(x)
  as.numeric((x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE))
}

winsorize_vec <- function(x, probs = c(0.01, 0.99)) {
  q <- stats::quantile(x, probs = probs, na.rm = TRUE)
  pmin(pmax(x, q[[1]]), q[[2]])
}

add_lag <- function(data, var, n = 1, group = "state", order = "year", new_name = NULL) {
  new_name <- new_name %||% paste0("l", n, "_", var)
  data |>
    dplyr::group_by(.data[[group]]) |>
    dplyr::arrange(.data[[order]], .by_group = TRUE) |>
    dplyr::mutate(
      !!new_name := {
        out <- dplyr::lag(.data[[var]], n)
        prev_order <- dplyr::lag(.data[[order]], n)
        invalid <- is.na(prev_order) | ((.data[[order]] - prev_order) != n)
        out[invalid] <- NA
        out
      }
    ) |>
    dplyr::ungroup()
}

add_lead <- function(data, var, n = 1, group = "state", order = "year", new_name = NULL) {
  new_name <- new_name %||% paste0("f", n, "_", var)
  data |>
    dplyr::group_by(.data[[group]]) |>
    dplyr::arrange(.data[[order]], .by_group = TRUE) |>
    dplyr::mutate(
      !!new_name := {
        out <- dplyr::lead(.data[[var]], n)
        next_order <- dplyr::lead(.data[[order]], n)
        invalid <- is.na(next_order) | ((next_order - .data[[order]]) != n)
        out[invalid] <- NA
        out
      }
    ) |>
    dplyr::ungroup()
}

add_diff <- function(data, var, group = "state", order = "year", new_name = NULL) {
  new_name <- new_name %||% paste0("d_", var)
  data |>
    dplyr::group_by(.data[[group]]) |>
    dplyr::arrange(.data[[order]], .by_group = TRUE) |>
    dplyr::mutate(
      !!new_name := {
        prev_value <- dplyr::lag(.data[[var]], 1)
        prev_order <- dplyr::lag(.data[[order]], 1)
        out <- .data[[var]] - prev_value
        invalid <- is.na(prev_order) | ((.data[[order]] - prev_order) != 1)
        out[invalid] <- NA
        out
      }
    ) |>
    dplyr::ungroup()
}

residualize_with_fe <- function(data, vars, fe = c("state", "year")) {
  out <- data
  for (v in vars) {
    fml <- as.formula(sprintf("%s ~ 1 | %s", v, paste(fe, collapse = " + ")))
    mod <- fixest::feols(fml, data = data, warn = FALSE)
    out[[paste0(v, "_resid")]] <- stats::residuals(mod)
  }
  out
}

first_non_missing_name <- function(df, candidates) {
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) == 0) return(NA_character_)
  hits[[1]]
}

parse_bea_value <- function(data_value, unit_multiplier = NULL) {
  x <- readr::parse_number(as.character(data_value))
  if (!is.null(unit_multiplier)) {
    mult <- suppressWarnings(as.numeric(unit_multiplier))
    mult[is.na(mult)] <- 0
    x <- x * (10 ^ mult)
  }
  x
}

weighted_mean_na <- function(x, w) {
  keep <- !is.na(x) & !is.na(w)
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(x[keep], w[keep])
}

effective_n <- function(w) {
  keep <- !is.na(w)
  if (!any(keep)) return(NA_real_)
  sum(w[keep])^2 / sum(w[keep]^2)
}

maybe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  safe_read_csv(path)
}

maybe_read_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  safe_read_rds(path)
}

ensure_project_dirs()
