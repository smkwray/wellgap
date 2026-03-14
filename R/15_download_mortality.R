# 15_download_mortality.R
# Standardize manually exported CDC WONDER state-year mortality files.

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

required_pkgs <- c("dplyr", "purrr", "stringr", "readr", "tibble", "janitor")
invisible(lapply(required_pkgs, require, character.only = TRUE))

cdc_wonder_manifest <- function() {
  tibble::tribble(
    ~outcome, ~filename,
    "all_cause", "cdc_wonder_all_cause_state_year.csv",
    "cardiovascular", "cdc_wonder_cardiovascular_state_year.csv",
    "suicide", "cdc_wonder_suicide_state_year.csv",
    "drug_poisoning", "cdc_wonder_drug_poisoning_state_year.csv"
  ) |>
    dplyr::mutate(path = path_project(cfg$paths$raw_root, "cdc_wonder", filename))
}

pick_first_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

standardize_state_label <- function(x) {
  lab <- stringr::str_trim(as.character(x))
  lab_name <- stringr::str_replace(lab, "\\s*\\(\\d{2}\\)$", "")
  lab_fips <- stringr::str_match(lab, "\\((\\d{2})\\)$")[, 2]
  lookup <- state_lookup() |>
    dplyr::mutate(
      state_upper = toupper(state),
      state_abbr_upper = toupper(state_abbr)
    )

  out <- rep(NA_character_, length(lab))
  lab_upper <- toupper(lab)
  lab_name_upper <- toupper(lab_name)

  out <- dplyr::if_else(lab_upper %in% lookup$state_upper, lookup$state_fips[match(lab_upper, lookup$state_upper)], out)
  out <- dplyr::if_else(is.na(out) & lab_name_upper %in% lookup$state_upper, lookup$state_fips[match(lab_name_upper, lookup$state_upper)], out)
  out <- dplyr::if_else(is.na(out) & lab_upper %in% lookup$state_abbr_upper, lookup$state_fips[match(lab_upper, lookup$state_abbr_upper)], out)
  out <- dplyr::if_else(is.na(out) & !is.na(lab_fips), standardize_state_fips(lab_fips), out)
  out <- dplyr::if_else(is.na(out) & stringr::str_detect(lab, "^\\d{2}$"), standardize_state_fips(lab), out)
  out
}

standardize_cdc_wonder_export <- function(path, outcome) {
  if (!file.exists(path)) return(NULL)

  raw <- safe_read_csv(path) |>
    janitor::clean_names()

  state_col <- pick_first_col(raw, c("state", "residence_state", "state_code"))
  year_col <- pick_first_col(raw, c("year", "year_code"))
  deaths_col <- pick_first_col(raw, c("deaths", "death_count"))
  age_adj_col <- pick_first_col(raw, c("age_adjusted_rate", "age_adjusted_death_rate", "age_adjusted_rate_deaths_per_100_000"))
  crude_col <- pick_first_col(raw, c("crude_rate", "crude_death_rate"))

  if (is.na(state_col) || is.na(year_col)) {
    stop("CDC WONDER export missing state/year columns: ", path, call. = FALSE)
  }

  out <- raw |>
    dplyr::transmute(
      state_fips = standardize_state_label(.data[[state_col]]),
      year = as.integer(.data[[year_col]]),
      deaths = if (!is.na(deaths_col)) readr::parse_number(as.character(.data[[deaths_col]])) else NA_real_,
      age_adjusted_rate = if (!is.na(age_adj_col)) readr::parse_number(as.character(.data[[age_adj_col]])) else NA_real_,
      crude_rate = if (!is.na(crude_col)) readr::parse_number(as.character(.data[[crude_col]])) else NA_real_
    ) |>
    dplyr::filter(
      !is.na(state_fips),
      !is.na(year),
      state_fips %in% state_lookup()$state_fips
    ) |>
    dplyr::group_by(state_fips, year) |>
    dplyr::summarise(
      deaths = dplyr::first(na.omit(deaths)),
      age_adjusted_rate = dplyr::first(na.omit(age_adjusted_rate)),
      crude_rate = dplyr::first(na.omit(crude_rate)),
      .groups = "drop"
    ) |>
    dplyr::rename(
      !!paste0(outcome, "_deaths") := deaths,
      !!paste0(outcome, "_age_adjusted_rate") := age_adjusted_rate,
      !!paste0(outcome, "_crude_rate") := crude_rate
    )

  out
}

build_cdc_wonder_state_year <- function() {
  manifest <- cdc_wonder_manifest()
  available <- manifest[file.exists(manifest$path), ]

  if (nrow(available) == 0) {
    note("No CDC WONDER exports found under {path_project(cfg$paths$raw_root, 'cdc_wonder')}; skipping hard-outcome build.")
    return(invisible(NULL))
  }

  pieces <- purrr::map2(available$path, available$outcome, standardize_cdc_wonder_export)
  out <- purrr::reduce(pieces, dplyr::full_join, by = c("state_fips", "year")) |>
    dplyr::left_join(state_lookup(), by = "state_fips") |>
    dplyr::arrange(state, year)

  csv_path <- path_project(cfg$paths$intermediate_root, "cdc_wonder_state_year.csv")
  rds_path <- path_project(cfg$paths$intermediate_root, "cdc_wonder_state_year.rds")
  safe_write_csv(out, csv_path)
  safe_write_rds(out, rds_path)
  note("Saved CDC WONDER state-year outcomes to {rds_path}")
  invisible(out)
}

if (sys.nframe() == 0) {
  build_cdc_wonder_state_year()
}
