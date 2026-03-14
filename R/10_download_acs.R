# 10_download_acs.R
# Download ACS 1-year state data for inequality and controls.

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

required_pkgs <- c("dplyr", "purrr", "tidyr", "readr")
invisible(lapply(required_pkgs, require, character.only = TRUE))

ensure_tidycensus <- function() {
  if (!requireNamespace("tidycensus", quietly = TRUE)) {
    stop(
      paste(
        "Install `tidycensus` to regenerate ACS from R,",
        "or use the existing data/intermediate/acs_state_year.csv fallback."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

init_census_key <- function() {
  key_name <- cfg$api_keys$census %||% "CENSUS_API_KEY"
  key_value <- Sys.getenv(key_name)
  if (!nzchar(key_value)) {
    stop(
      sprintf("Set %s in your environment before running ACS downloads.", key_name),
      call. = FALSE
    )
  }
  tidycensus::census_api_key(key_value, install = FALSE, overwrite = FALSE)
  invisible(key_value)
}

pull_acs_base_year <- function(year, survey = cfg$acs$survey %||% "acs1") {
  vars <- unlist(cfg$acs$base_vars, use.names = TRUE)
  dat <- tidycensus::get_acs(
    geography = "state",
    variables = vars,
    year = year,
    survey = survey,
    output = "wide",
    cache_table = TRUE
  )

  out <- dat |>
    dplyr::transmute(
      state = NAME,
      state_fips = GEOID,
      year = year,
      acs_gini = B19083_001E,
      median_hh_income = B19013_001E,
      total_population = B01003_001E,
      labor_force = B23025_003E,
      unemployed = B23025_005E,
      housing_units = B25001_001E,
      unemployment_rate = dplyr::if_else(labor_force > 0, unemployed / labor_force, NA_real_),
      acs_pandemic_flag = as.integer(year == 2020)
    )

  out
}

pull_acs_education_year <- function(year, survey = cfg$acs$survey %||% "acs1") {
  dat <- tidycensus::get_acs(
    geography = "state",
    table = "B15003",
    year = year,
    survey = survey,
    output = "wide",
    cache_table = TRUE
  )

  dat |>
    dplyr::transmute(
      state = NAME,
      state_fips = GEOID,
      year = year,
      edu_total = B15003_001E,
      ba_plus_n = B15003_022E + B15003_023E + B15003_024E + B15003_025E,
      college_share = dplyr::if_else(edu_total > 0, ba_plus_n / edu_total, NA_real_)
    )
}

pull_acs_race_year <- function(year, survey = cfg$acs$survey %||% "acs1") {
  dat <- tidycensus::get_acs(
    geography = "state",
    table = "B03002",
    year = year,
    survey = survey,
    output = "wide",
    cache_table = TRUE
  )

  dat |>
    dplyr::transmute(
      state = NAME,
      state_fips = GEOID,
      year = year,
      race_total = B03002_001E,
      white_nh_share = dplyr::if_else(race_total > 0, B03002_003E / race_total, NA_real_),
      black_share = dplyr::if_else(race_total > 0, B03002_004E / race_total, NA_real_),
      asian_share = dplyr::if_else(race_total > 0, B03002_006E / race_total, NA_real_),
      hispanic_share = dplyr::if_else(race_total > 0, B03002_012E / race_total, NA_real_)
    )
}

normalize_acs_years <- function(years, survey = cfg$acs$survey %||% "acs1") {
  years <- sort(unique(as.integer(years)))
  if (identical(survey, "acs1") && 2020 %in% years) {
    note("Dropping ACS 2020 from the pull because the standard 2020 ACS 1-year endpoint is unavailable.")
    years <- setdiff(years, 2020L)
  }
  years
}

download_acs_state_panel <- function(
  years = cfg_vec(cfg$windows$acs_years),
  survey = cfg$acs$survey %||% "acs1"
) {
  ensure_tidycensus()
  init_census_key()
  years <- normalize_acs_years(years, survey = survey)

  note("Pulling ACS state-year panel for years: {paste(years, collapse = ', ')}")

  base <- purrr::map_dfr(years, pull_acs_base_year, survey = survey)
  edu  <- purrr::map_dfr(years, pull_acs_education_year, survey = survey)
  race <- purrr::map_dfr(years, pull_acs_race_year, survey = survey)

  acs_state_year <- base |>
    dplyr::left_join(edu, by = c("state", "state_fips", "year")) |>
    dplyr::left_join(race, by = c("state", "state_fips", "year")) |>
    dplyr::arrange(state, year)

  raw_csv <- path_project(cfg$paths$intermediate_root, "acs_state_year.csv")
  raw_rds <- path_project(cfg$paths$intermediate_root, "acs_state_year.rds")

  safe_write_csv(acs_state_year, raw_csv)
  safe_write_rds(acs_state_year, raw_rds)

  note("Saved ACS state-year panel to {raw_rds}")
  invisible(acs_state_year)
}

if (sys.nframe() == 0) {
  download_acs_state_panel()
}
