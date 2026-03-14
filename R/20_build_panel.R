# 20_build_panel.R
# Merge ACS, BEA, BRFSS, and optional control files into a state-year panel.

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

read_intermediate_dataset <- function(stem) {
  rds_path <- path_project(cfg$paths$intermediate_root, paste0(stem, ".rds"))
  csv_path <- path_project(cfg$paths$intermediate_root, paste0(stem, ".csv"))

  if (file.exists(rds_path)) return(safe_read_rds(rds_path))
  if (file.exists(csv_path)) return(safe_read_csv(csv_path))

  stop(
    "Missing intermediate file for ", stem, ": expected one of ",
    rds_path, " or ", csv_path,
    call. = FALSE
  )
}

read_required_core <- function() {
  bea_core_path <- path_project(cfg$paths$intermediate_root, "bea_state_core.csv")

  if (!file.exists(bea_core_path)) stop("Missing BEA core file: ", bea_core_path, call. = FALSE)

  list(
    acs = read_intermediate_dataset("acs_state_year") |>
      janitor::clean_names() |>
      dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year)),
    brfss = read_intermediate_dataset("brfss_state_year") |>
      janitor::clean_names() |>
      dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year)),
    bea = safe_read_csv(bea_core_path) |>
      janitor::clean_names() |>
      dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year))
  )
}

read_optional_standardized <- function(filename) {
  path <- path_project(cfg$paths$intermediate_root, filename)
  if (!file.exists(path)) return(NULL)
  safe_read_csv(path) |> janitor::clean_names()
}

prepare_distribution_file <- function(df) {
  df |>
    dplyr::mutate(
      state_fips = standardize_state_fips(state_fips),
      year = as.integer(year),
      disp_mean_median_gap = dplyr::if_else(
        !is.na(disp_mean) & !is.na(disp_median) & disp_mean > 0 & disp_median > 0,
        log(disp_mean) - log(disp_median),
        NA_real_
      ),
      pers_mean_median_gap = dplyr::if_else(
        !is.na(pers_mean) & !is.na(pers_median) & pers_mean > 0 & pers_median > 0,
        log(pers_mean) - log(pers_median),
        NA_real_
      )
    )
}

add_standardized_treatments <- function(panel) {
  if ("acs_gini" %in% names(panel)) panel$acs_gini_z <- zscore(panel$acs_gini)
  if ("disp_top20_share" %in% names(panel)) panel$disp_top20_share_z <- zscore(panel$disp_top20_share)
  if ("disp_top10_share" %in% names(panel)) panel$disp_top10_share_z <- zscore(panel$disp_top10_share)
  if ("disp_mean_median_gap" %in% names(panel)) panel$disp_mean_median_gap_z <- zscore(panel$disp_mean_median_gap)
  if ("pers_mean_median_gap" %in% names(panel)) panel$pers_mean_median_gap_z <- zscore(panel$pers_mean_median_gap)
  panel
}

add_core_derived_variables <- function(panel) {
  panel <- panel |>
    dplyr::mutate(
      state_fips = standardize_state_fips(state_fips),
      year = as.integer(year),
      population_used = dplyr::coalesce(bea_population, total_population),
      employed = dplyr::if_else(!is.na(labor_force) & !is.na(unemployed), labor_force - unemployed, NA_real_),
      employment_to_population_ratio = dplyr::if_else(
        !is.na(employed) & !is.na(total_population) & total_population > 0,
        employed / total_population,
        NA_real_
      ),
      rpp_adjusted_median_hh_income = dplyr::if_else(
        !is.na(median_hh_income) & !is.na(rpp_all_items) & rpp_all_items > 0,
        median_hh_income / (rpp_all_items / 100),
        NA_real_
      ),
      real_pce_pc = dplyr::if_else(!is.na(real_pce) & !is.na(population_used) & population_used > 0, real_pce / population_used, NA_real_),
      current_pce_pc = dplyr::if_else(!is.na(current_pce) & !is.na(population_used) & population_used > 0, current_pce / population_used, NA_real_),
      real_mean_income = dplyr::if_else(!is.na(real_personal_income) & !is.na(population_used) & population_used > 0, real_personal_income / population_used, NA_real_),
      current_mean_income = dplyr::if_else(!is.na(personal_income) & !is.na(population_used) & population_used > 0, personal_income / population_used, NA_real_),
      log_rpp_adj_median_hh_income = dplyr::if_else(
        !is.na(rpp_adjusted_median_hh_income) & rpp_adjusted_median_hh_income > 0,
        log(rpp_adjusted_median_hh_income),
        NA_real_
      ),
      log_real_pce_pc = dplyr::if_else(!is.na(real_pce_pc) & real_pce_pc > 0, log(real_pce_pc), NA_real_),
      log_real_mean_income = dplyr::if_else(!is.na(real_mean_income) & real_mean_income > 0, log(real_mean_income), NA_real_),
      pce_income_ratio = dplyr::if_else(!is.na(current_pce) & !is.na(personal_income) & personal_income != 0, current_pce / personal_income, NA_real_)
    ) |>
    dplyr::group_by(state_fips) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::mutate(
      prev_year = dplyr::lag(year),
      prev_log_real_pce_pc = dplyr::lag(log_real_pce_pc),
      prev_log_real_mean_income = dplyr::lag(log_real_mean_income),
      real_pce_pc_growth = dplyr::if_else(
        !is.na(prev_year) & (year - prev_year) == 1,
        100 * (log_real_pce_pc - prev_log_real_pce_pc),
        NA_real_
      ),
      real_mean_income_growth = dplyr::if_else(
        !is.na(prev_year) & (year - prev_year) == 1,
        100 * (log_real_mean_income - prev_log_real_mean_income),
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-prev_year, -prev_log_real_pce_pc, -prev_log_real_mean_income)

  panel
}

merge_optional_controls <- function(panel) {
  fhfa <- read_optional_standardized("fhfa_state_hpi.csv")
  permits <- read_optional_standardized("building_permits_state_year.csv")
  minwage <- read_optional_standardized("state_minimum_wage.csv")
  union <- read_optional_standardized("union_membership_state_year.csv")
  qcew <- read_optional_standardized("qcew_state_year.csv")
  wonder <- read_optional_standardized("cdc_wonder_state_year.csv")

  if (!is.null(fhfa)) {
    fhfa <- fhfa |> dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year))
    hpi_col <- first_non_missing_name(fhfa, c("hpi", "hpi_index", "index"))
    if (!is.na(hpi_col)) {
      fhfa2 <- fhfa |>
        dplyr::transmute(state_fips, year, hpi = as.numeric(.data[[hpi_col]])) |>
        dplyr::group_by(state_fips) |>
        dplyr::arrange(year, .by_group = TRUE) |>
        dplyr::mutate(
          prev_year = dplyr::lag(year),
          prev_year_3 = dplyr::lag(year, 3),
          prev_hpi = dplyr::lag(hpi),
          prev_hpi_3 = dplyr::lag(hpi, 3),
          hpi_yoy = dplyr::if_else(
            !is.na(prev_year) & (year - prev_year) == 1,
            100 * (log(hpi) - log(prev_hpi)),
            NA_real_
          ),
          hpi_3y = dplyr::if_else(
            !is.na(prev_year_3) & (year - prev_year_3) == 3,
            100 * (log(hpi) - log(prev_hpi_3)),
            NA_real_
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-prev_year, -prev_year_3, -prev_hpi, -prev_hpi_3)
      panel <- dplyr::left_join(panel, fhfa2, by = c("state_fips", "year"))
    }
  }

  if (!is.null(permits)) {
    permits <- permits |> dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year))
    permits_col <- first_non_missing_name(permits, c("permits", "total_permits", "units"))
    percap_col <- first_non_missing_name(permits, c("permits_per_1000", "permits_pc", "permits_per_capita"))
    permits2 <- permits |>
      dplyr::mutate(
        permits = if (!is.na(permits_col)) as.numeric(.data[[permits_col]]) else NA_real_,
        permits_per_1000 = if (!is.na(percap_col)) as.numeric(.data[[percap_col]]) else NA_real_
      ) |>
      dplyr::select(state_fips, year, permits, permits_per_1000)
    panel <- dplyr::left_join(panel, permits2, by = c("state_fips", "year"))
    if ("permits_per_1000" %in% names(panel)) {
      panel <- panel |>
        dplyr::mutate(
          permits_per_1000 = dplyr::if_else(
            is.na(permits_per_1000) & !is.na(permits) & !is.na(population_used) & population_used > 0,
            1000 * permits / population_used,
            permits_per_1000
          )
        )
    }
  }

  if (!is.null(minwage)) {
    minwage <- minwage |> dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year))
    mw_col <- first_non_missing_name(minwage, c("state_min_wage_nominal", "minimum_wage", "min_wage", "wage"))
    if (!is.na(mw_col)) {
      panel <- dplyr::left_join(
        panel,
        minwage |> dplyr::transmute(state_fips, year, state_min_wage_nominal = as.numeric(.data[[mw_col]])),
        by = c("state_fips", "year")
      )
    }
  }

  if (!is.null(union)) {
    union <- union |> dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year))
    union_col <- first_non_missing_name(union, c("union_membership_rate", "union_rate", "membership_rate"))
    if (!is.na(union_col)) {
      panel <- dplyr::left_join(
        panel,
        union |> dplyr::transmute(state_fips, year, union_membership_rate = as.numeric(.data[[union_col]])),
        by = c("state_fips", "year")
      )
    }
  }

  if (!is.null(qcew)) {
    qcew <- qcew |> dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year))
    keep_cols <- intersect(
      c(
        "state_fips", "year", "qcew_private_annual_avg_emplvl", "qcew_private_avg_wkly_wage",
        "qcew_mining_share", "qcew_construction_share", "qcew_manufacturing_share",
        "qcew_trade_transport_share", "qcew_information_share",
        "qcew_finance_real_estate_share", "qcew_professional_business_share",
        "qcew_education_health_share", "qcew_leisure_hospitality_share"
      ),
      names(qcew)
    )
    panel <- dplyr::left_join(panel, dplyr::select(qcew, dplyr::all_of(keep_cols)), by = c("state_fips", "year"))
  }

  if (!is.null(wonder)) {
    wonder <- wonder |>
      dplyr::mutate(state_fips = standardize_state_fips(state_fips), year = as.integer(year))
    keep_cols <- setdiff(names(wonder), c("state", "state_abbr"))
    panel <- dplyr::left_join(panel, dplyr::select(wonder, dplyr::all_of(keep_cols)), by = c("state_fips", "year"))
  }

  panel
}

add_lags_and_diffs <- function(panel) {
  vars_to_diff <- intersect(
    c("acs_gini_z", "disp_top20_share_z", "disp_top10_share_z", "disp_mean_median_gap_z", "pers_mean_median_gap_z"),
    names(panel)
  )

  for (v in vars_to_diff) {
    panel <- add_diff(panel, v, group = "state", order = "year", new_name = paste0("d_", v))
  }

  vars_to_lag <- intersect(
    c(
      "acs_gini_z", "disp_top20_share_z", "disp_top10_share_z", "disp_mean_median_gap_z",
      "pers_mean_median_gap_z", "log_real_mean_income", "rpp_all_items", "rpp_housing",
      "unemployment_rate", "employment_to_population_ratio", "log_rpp_adj_median_hh_income",
      "hpi_yoy", "permits_per_1000", "college_share",
      "black_share", "hispanic_share", "white_nh_share", "asian_share",
      "state_min_wage_nominal", "union_membership_rate",
      "qcew_private_annual_avg_emplvl", "qcew_private_avg_wkly_wage",
      "qcew_mining_share", "qcew_construction_share", "qcew_manufacturing_share",
      "qcew_trade_transport_share", "qcew_information_share",
      "qcew_finance_real_estate_share", "qcew_professional_business_share",
      "qcew_education_health_share", "qcew_leisure_hospitality_share"
    ),
    names(panel)
  )

  for (v in vars_to_lag) {
    panel <- add_lag(panel, v, n = 1, group = "state", order = "year", new_name = paste0("l1_", v))
  }

  if ("d_acs_gini_z" %in% names(panel)) {
    cutoff <- stats::quantile(panel$d_acs_gini_z, probs = cfg$analysis$large_shock_quantile %||% 0.75, na.rm = TRUE)
    panel <- panel |> dplyr::mutate(large_acs_gini_shock = as.integer(d_acs_gini_z >= cutoff))
  }

  if ("d_disp_mean_median_gap_z" %in% names(panel)) {
    cutoff <- stats::quantile(panel$d_disp_mean_median_gap_z, probs = cfg$analysis$large_shock_quantile %||% 0.75, na.rm = TRUE)
    panel <- panel |> dplyr::mutate(large_disp_gap_shock = as.integer(d_disp_mean_median_gap_z >= cutoff))
  }

  panel
}

build_state_year_panel <- function() {
  core <- read_required_core()

  panel <- core$acs |>
    dplyr::left_join(core$bea, by = c("state_fips", "year"), suffix = c("_acs", "_bea")) |>
    dplyr::left_join(core$brfss, by = c("state_fips", "year"), suffix = c("", "_brfss"))

  distribution_path <- path_project(cfg$paths$intermediate_root, "bea_distribution_state_year.csv")
  if (file.exists(distribution_path)) {
    dist <- safe_read_csv(distribution_path) |>
      janitor::clean_names() |>
      prepare_distribution_file() |>
      dplyr::select(-dplyr::any_of("state"))
    panel <- dplyr::left_join(panel, dist, by = c("state_fips", "year"))
  } else {
    note("Optional BEA distribution file not found; proceeding with ACS Gini only.")
  }

  panel <- panel |>
    dplyr::mutate(
      state = dplyr::coalesce(state_acs, state_bea, state),
      state_fips = standardize_state_fips(state_fips)
    ) |>
    dplyr::select(-dplyr::matches("^state_(acs|bea)$"))

  panel <- add_core_derived_variables(panel)
  panel <- merge_optional_controls(panel)
  panel <- add_standardized_treatments(panel)
  panel <- add_lags_and_diffs(panel)

  panel <- panel |>
    dplyr::left_join(
      state_lookup() |> dplyr::select(state_fips, state_abbr),
      by = "state_fips",
      suffix = c("", "_lookup")
    ) |>
    dplyr::mutate(state_abbr = dplyr::coalesce(state_abbr, state_abbr_lookup)) |>
    dplyr::select(-dplyr::any_of("state_abbr_lookup")) |>
    dplyr::arrange(state, year)

  csv_path <- path_project(cfg$paths$final_root, "state_year_panel.csv")
  rds_path <- path_project(cfg$paths$final_root, "state_year_panel.rds")

  safe_write_csv(panel, csv_path)
  safe_write_rds(panel, rds_path)

  note("Saved merged state-year panel to {rds_path}")
  invisible(panel)
}

if (sys.nframe() == 0) {
  build_state_year_panel()
}
