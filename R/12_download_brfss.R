# 12_download_brfss.R
# Download BRFSS annual microdata and aggregate to state-year outcomes.

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

required_pkgs <- c("dplyr", "purrr", "stringr", "readr", "tibble", "tidyr", "rvest", "xml2", "haven", "survey", "janitor")
invisible(lapply(required_pkgs, require, character.only = TRUE))

brfss_annual_page <- function(year) {
  sprintf("https://www.cdc.gov/brfss/annual_data/annual_%s.html", year)
}

discover_brfss_year_files <- function(year, save = TRUE) {
  page_url <- brfss_annual_page(year)
  page <- rvest::read_html(page_url)
  links <- rvest::html_elements(page, "a")

  out <- tibble::tibble(
    text = rvest::html_text2(links),
    href = rvest::html_attr(links, "href")
  ) |>
    dplyr::mutate(url = xml2::url_absolute(href, page_url)) |>
    dplyr::filter(stringr::str_detect(url, "(?i)\\.(zip|xpt|txt|pdf)$")) |>
    dplyr::distinct()

  if (save) {
    safe_write_csv(out, path_project(cfg$paths$raw_root, "brfss", paste0("brfss_manifest_", year, ".csv")))
  }

  out
}

choose_brfss_data_url <- function(manifest) {
  transport_zip_hits <- manifest |>
    dplyr::filter(stringr::str_detect(url, "(?i)\\.zip$")) |>
    dplyr::filter(
      stringr::str_detect(text, "(?i)sas transport") |
        stringr::str_detect(url, "(?i)(xpt|transport)")
    )

  if (nrow(transport_zip_hits) > 0) return(transport_zip_hits$url[[1]])

  zip_hits <- manifest |>
    dplyr::filter(stringr::str_detect(url, "(?i)\\.zip$")) |>
    dplyr::filter(
      !stringr::str_detect(url, "(?i)codebook|layout|questionnaire|format") &
        !stringr::str_detect(text, "(?i)ascii")
    )

  if (nrow(zip_hits) > 0) return(zip_hits$url[[1]])

  xpt_hits <- manifest |>
    dplyr::filter(stringr::str_detect(url, "(?i)\\.xpt$"))

  if (nrow(xpt_hits) > 0) return(xpt_hits$url[[1]])

  stop("No ZIP or XPT data file found in BRFSS manifest.", call. = FALSE)
}

download_brfss_year <- function(year, overwrite = FALSE) {
  manifest <- discover_brfss_year_files(year, save = TRUE)
  data_url <- choose_brfss_data_url(manifest)
  dest <- path_project(cfg$paths$raw_root, "brfss", paste0("brfss_", year, "_", basename(data_url)))
  download_if_needed(data_url, dest, overwrite = overwrite)
  invisible(dest)
}

read_brfss_year <- function(year, download = TRUE) {
  raw_dir <- path_project(cfg$paths$raw_root, "brfss")
  candidates <- list.files(raw_dir, pattern = paste0("^brfss_", year, "_"), full.names = TRUE)

  if (length(candidates) == 0 && isTRUE(download)) {
    download_brfss_year(year)
    candidates <- list.files(raw_dir, pattern = paste0("^brfss_", year, "_"), full.names = TRUE)
  }

  if (length(candidates) == 0) {
    stop("No BRFSS file found for year ", year, call. = FALSE)
  }

  preferred <- candidates[
    stringr::str_detect(basename(candidates), "(?i)(xpt|transport)") |
      stringr::str_detect(basename(candidates), "(?i)\\.xpt$")
  ]
  if (length(preferred) == 0 && isTRUE(download)) {
    download_brfss_year(year)
    candidates <- list.files(raw_dir, pattern = paste0("^brfss_", year, "_"), full.names = TRUE)
    preferred <- candidates[
      stringr::str_detect(basename(candidates), "(?i)(xpt|transport)") |
        stringr::str_detect(basename(candidates), "(?i)\\.xpt$")
    ]
  }
  if (length(preferred) > 0) {
    f <- preferred[[1]]
  } else {
    f <- candidates[[1]]
  }
  ext <- tolower(tools::file_ext(f))
  needed_vars <- c("_STATE", "_LLCPWT", "_STSTR", "_PSU", "_AGE_G", "_AGE80", "EMPLOY1", "GENHLTH", "MENTHLTH", "PHYSHLTH")

  if (ext == "zip") {
    zip_list <- utils::unzip(f, list = TRUE)
    zip_names <- trimws(zip_list$Name)
    xpt_idx <- which(stringr::str_detect(zip_names, "(?i)\\.xpt$"))[1]
    xpt_file <- zip_list$Name[[xpt_idx]]
    if (is.na(xpt_file)) stop("No XPT file found inside BRFSS zip for year ", year, call. = FALSE)
    tmp_dir <- tempfile(pattern = paste0("brfss_", year, "_"))
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
    extracted <- utils::unzip(f, files = xpt_file, exdir = tmp_dir)
    dat <- haven::read_xpt(extracted[[1]], col_select = tidyselect::any_of(needed_vars))
  } else if (ext == "xpt") {
    dat <- haven::read_xpt(f, col_select = tidyselect::any_of(needed_vars))
  } else {
    stop("Unsupported BRFSS file extension: ", ext, call. = FALSE)
  }

  janitor::clean_names(dat)
}

recode_brfss_year <- function(df, year) {
  pick_var <- function(candidates) {
    hit <- candidates[candidates %in% names(df)]
    if (length(hit) == 0) return(NA_character_)
    hit[[1]]
  }

  vars <- list(
    state = pick_var(c("_state", "state")),
    wt = pick_var(c("_llcpwt", "llcpwt")),
    strata = pick_var(c("_ststr", "ststr")),
    psu = pick_var(c("_psu", "psu")),
    age_group = pick_var(c("_age_g", "age_g")),
    age80 = pick_var(c("_age80", "age80")),
    employ = pick_var(c("employ1", "employ")),
    genhlth = pick_var("genhlth"),
    menthlth = pick_var("menthlth"),
    physhlth = pick_var("physhlth")
  )

  required_vars <- c("state", "wt", "strata", "psu", "genhlth", "menthlth", "physhlth")
  missing_vars <- required_vars[is.na(unlist(vars[required_vars], use.names = FALSE))]
  if (length(missing_vars) > 0) {
    stop("Missing required BRFSS variables: ", paste(missing_vars, collapse = ", "), call. = FALSE)
  }

  tibble::tibble(
    state_fips = standardize_state_fips(df[[vars$state]]),
    wt = as.numeric(df[[vars$wt]]),
    strata = as.numeric(df[[vars$strata]]),
    psu = as.numeric(df[[vars$psu]]),
    age_group = if (!is.na(vars$age_group)) as.integer(df[[vars$age_group]]) else NA_integer_,
    age80 = if (!is.na(vars$age80)) as.integer(df[[vars$age80]]) else NA_integer_,
    employ1 = if (!is.na(vars$employ)) as.integer(df[[vars$employ]]) else NA_integer_,
    genhlth = as.integer(df[[vars$genhlth]]),
    menthlth_raw = as.integer(df[[vars$menthlth]]),
    physhlth_raw = as.integer(df[[vars$physhlth]])
  ) |>
    dplyr::mutate(
      year = year,
      fair_poor_health = dplyr::case_when(
        genhlth %in% c(4L, 5L) ~ 1,
        genhlth %in% c(1L, 2L, 3L) ~ 0,
        TRUE ~ NA_real_
      ),
      menthlth_days = dplyr::case_when(
        menthlth_raw == 88L ~ 0,
        menthlth_raw %in% 0:30 ~ as.numeric(menthlth_raw),
        TRUE ~ NA_real_
      ),
      physhlth_days = dplyr::case_when(
        physhlth_raw == 88L ~ 0,
        physhlth_raw %in% 0:30 ~ as.numeric(physhlth_raw),
        TRUE ~ NA_real_
      ),
      age_65_plus = dplyr::case_when(
        !is.na(age80) & age80 >= 65 ~ 1,
        !is.na(age80) & age80 < 65 ~ 0,
        TRUE ~ NA_real_
      ),
      retired = dplyr::case_when(
        employ1 == 7L ~ 1,
        !is.na(employ1) & employ1 %in% c(1L, 2L, 3L, 4L, 5L, 6L, 8L) ~ 0,
        TRUE ~ NA_real_
      ),
      frequent_mental_distress = dplyr::if_else(!is.na(menthlth_days), as.numeric(menthlth_days >= 14), NA_real_),
      frequent_physical_distress = dplyr::if_else(!is.na(physhlth_days), as.numeric(physhlth_days >= 14), NA_real_)
    )
}

add_brfss_age_band <- function(clean_df) {
  clean_df |>
    dplyr::mutate(
      age_band3 = dplyr::case_when(
        !is.na(age80) & age80 >= 18 & age80 <= 44 ~ "age_18_44",
        !is.na(age80) & age80 >= 45 & age80 <= 64 ~ "age_45_64",
        !is.na(age80) & age80 >= 65 ~ "age_65_plus",
        is.na(age80) & !is.na(age_group) & age_group %in% c(1L, 2L, 3L) ~ "age_18_44",
        is.na(age80) & !is.na(age_group) & age_group %in% c(4L, 5L) ~ "age_45_64",
        is.na(age80) & !is.na(age_group) & age_group >= 6L ~ "age_65_plus",
        TRUE ~ NA_character_
      )
    )
}

brfss_outcome_map <- function() {
  c(
    fair_poor_health = "fair_poor_health_rate",
    frequent_mental_distress = "frequent_mental_distress_rate",
    frequent_physical_distress = "frequent_physical_distress_rate",
    menthlth_days = "mean_bad_mental_days",
    physhlth_days = "mean_bad_physical_days"
  )
}

survey_mean_by_group <- function(clean_df, outcome_vars, group_vars) {
  old_lonely <- getOption("survey.lonely.psu")
  options(survey.lonely.psu = "adjust")
  on.exit(options(survey.lonely.psu = old_lonely), add = TRUE)

  design <- survey::svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~wt,
    data = clean_df,
    nest = TRUE
  )

  out <- survey::svyby(
    formula = stats::as.formula(paste0("~", paste(outcome_vars, collapse = " + "))),
    by = stats::as.formula(paste0("~", paste(group_vars, collapse = " + "))),
    design = design,
    FUN = survey::svymean,
    vartype = "se",
    na.rm = TRUE,
    keep.var = TRUE,
    drop.empty.groups = FALSE
  )

  out <- janitor::clean_names(as.data.frame(out)) |>
    tibble::as_tibble()

  if ("statistic" %in% names(out) && length(outcome_vars) == 1L) {
    names(out)[names(out) == "statistic"] <- outcome_vars[[1]]
  }

  for (v in outcome_vars) {
    stat_name <- paste0("statistic_", v)
    se_name <- paste0("se_", v)

    if (stat_name %in% names(out)) {
      names(out)[names(out) == stat_name] <- v
    }
    if (se_name %in% names(out)) {
      names(out)[names(out) == se_name] <- paste0(v, "_se")
    }
  }

  if ("se" %in% names(out) && length(outcome_vars) == 1L) {
    names(out)[names(out) == "se"] <- paste0(outcome_vars[[1]], "_se")
  }

  if ("state_fips" %in% names(out)) {
    out <- out |>
      dplyr::mutate(state_fips = standardize_state_fips(as.character(state_fips)))
  }
  if ("year" %in% names(out)) {
    out <- out |>
      dplyr::mutate(year = as.integer(as.character(year)))
  }
  if ("age_band3" %in% names(out)) {
    out <- out |>
      dplyr::mutate(age_band3 = as.character(age_band3))
  }

  out
}

compute_brfss_age_reference <- function(years = cfg_vec(cfg$windows$brfss_years)) {
  ref <- purrr::map_dfr(years, function(yr) {
    raw <- read_brfss_year(yr, download = TRUE)
    clean <- recode_brfss_year(raw, yr) |>
      add_brfss_age_band() |>
      dplyr::filter(!is.na(age_band3))

    clean |>
      dplyr::group_by(age_band3) |>
      dplyr::summarise(total_wt = sum(wt, na.rm = TRUE), .groups = "drop")
  }) |>
    dplyr::group_by(age_band3) |>
    dplyr::summarise(total_wt = sum(total_wt, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(ref_weight = total_wt / sum(total_wt, na.rm = TRUE)) |>
    dplyr::arrange(age_band3)

  safe_write_csv(ref, path_project(cfg$paths$intermediate_root, "brfss_age_reference_weights.csv"))
  ref
}

standardize_brfss_age_rates <- function(age_estimates, ref_weights) {
  outcome_names <- unname(brfss_outcome_map())

  long <- purrr::map_dfr(outcome_names, function(outcome_name) {
    se_name <- paste0(outcome_name, "_se")
    age_estimates |>
      dplyr::transmute(
        state_fips,
        year,
        age_band3,
        outcome = outcome_name,
        estimate = .data[[outcome_name]],
        se = .data[[se_name]]
      )
  }) |>
    dplyr::left_join(ref_weights |> dplyr::select(age_band3, ref_weight), by = "age_band3")

  standardized <- long |>
    dplyr::group_by(state_fips, year, outcome) |>
    dplyr::summarise(
      weight_sum = sum(ref_weight[!is.na(estimate)], na.rm = TRUE),
      estimate = ifelse(
        weight_sum > 0,
        sum((ref_weight / weight_sum) * estimate, na.rm = TRUE),
        NA_real_
      ),
      se = ifelse(
        weight_sum > 0,
        sqrt(sum(((ref_weight / weight_sum) * se)^2, na.rm = TRUE)),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::select(-weight_sum) |>
    tidyr::pivot_wider(
      names_from = outcome,
      values_from = c(estimate, se),
      names_glue = "{outcome}_age_std{ifelse(.value == 'se', '_se', '')}"
    )

  standardized
}

aggregate_brfss_state_year <- function(clean_df, ref_weights = NULL) {
  outcome_vars <- names(brfss_outcome_map())
  name_map <- brfss_outcome_map()

  estimates <- survey_mean_by_group(clean_df, outcome_vars, c("state_fips", "year")) |>
    dplyr::rename(!!!stats::setNames(names(name_map), unname(name_map))) |>
    dplyr::rename(!!!stats::setNames(paste0(names(name_map), "_se"), paste0(unname(name_map), "_se")))

  counts <- clean_df |>
    dplyr::group_by(state_fips, year) |>
    dplyr::summarise(
      brfss_unweighted_n = dplyr::n(),
      brfss_effective_n = effective_n(wt),
      .groups = "drop"
    )

  age_std <- NULL
  if (!is.null(ref_weights)) {
    age_estimates <- clean_df |>
      add_brfss_age_band() |>
      dplyr::filter(!is.na(age_band3)) |>
      survey_mean_by_group(outcome_vars, c("state_fips", "year", "age_band3")) |>
      dplyr::rename(!!!stats::setNames(names(name_map), unname(name_map))) |>
      dplyr::rename(!!!stats::setNames(paste0(names(name_map), "_se"), paste0(unname(name_map), "_se")))

    age_std <- standardize_brfss_age_rates(age_estimates, ref_weights)
  }

  out <- estimates |>
    dplyr::left_join(counts, by = c("state_fips", "year"))

  if (!is.null(age_std)) {
    out <- out |>
      dplyr::left_join(age_std, by = c("state_fips", "year"))
  }

  out <- out |>
    dplyr::mutate(
      frequent_mental_distress_rate_precision_wt = dplyr::if_else(
        !is.na(frequent_mental_distress_rate_se) & frequent_mental_distress_rate_se > 0,
        1 / (frequent_mental_distress_rate_se^2),
        NA_real_
      ),
      fair_poor_health_rate_precision_wt = dplyr::if_else(
        !is.na(fair_poor_health_rate_se) & fair_poor_health_rate_se > 0,
        1 / (fair_poor_health_rate_se^2),
        NA_real_
      )
    )

  if ("frequent_mental_distress_rate_age_std_se" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        frequent_mental_distress_rate_age_std_precision_wt = dplyr::if_else(
          !is.na(frequent_mental_distress_rate_age_std_se) & frequent_mental_distress_rate_age_std_se > 0,
          1 / (frequent_mental_distress_rate_age_std_se^2),
          NA_real_
        )
      )
  }

  if ("fair_poor_health_rate_age_std_se" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        fair_poor_health_rate_age_std_precision_wt = dplyr::if_else(
          !is.na(fair_poor_health_rate_age_std_se) & fair_poor_health_rate_age_std_se > 0,
          1 / (fair_poor_health_rate_age_std_se^2),
          NA_real_
        )
      )
  }

  out |>
    dplyr::inner_join(state_lookup(), by = "state_fips") |>
    dplyr::arrange(state, year)
}

aggregate_brfss_subgroup <- function(clean_df, subgroup_var, subgroup_label) {
  keep <- clean_df[[subgroup_var]] %in% 1
  if (!any(keep, na.rm = TRUE)) return(tibble::tibble())

  clean_df |>
    dplyr::filter(.data[[subgroup_var]] %in% 1) |>
    aggregate_brfss_state_year() |>
    dplyr::mutate(subgroup = subgroup_label) |>
    dplyr::select(subgroup, dplyr::everything())
}

build_brfss_subgroup_panel <- function(years = cfg_vec(cfg$windows$brfss_years), overwrite = FALSE) {
  note("Building BRFSS subgroup state-year panel for years: {paste(years, collapse = ', ')}")

  out <- purrr::map_dfr(years, function(yr) {
    note("Processing BRFSS subgroup year {yr}")
    raw <- read_brfss_year(yr, download = TRUE)
    clean <- recode_brfss_year(raw, yr)

    dplyr::bind_rows(
      aggregate_brfss_subgroup(clean, "age_65_plus", "age_65_plus"),
      aggregate_brfss_subgroup(clean, "retired", "retired")
    )
  })

  csv_path <- path_project(cfg$paths$intermediate_root, "brfss_state_year_subgroups.csv")
  rds_path <- path_project(cfg$paths$intermediate_root, "brfss_state_year_subgroups.rds")
  safe_write_csv(out, csv_path)
  safe_write_rds(out, rds_path)

  note("Saved BRFSS subgroup state-year outcomes to {rds_path}")
  invisible(out)
}

build_brfss_panel <- function(years = cfg_vec(cfg$windows$brfss_years), overwrite = FALSE) {
  note("Building BRFSS state-year panel for years: {paste(years, collapse = ', ')}")

  ref_weights <- compute_brfss_age_reference(years)

  out <- purrr::map_dfr(years, function(yr) {
    note("Processing BRFSS year {yr}")
    raw <- read_brfss_year(yr, download = TRUE)
    clean <- recode_brfss_year(raw, yr)
    aggregate_brfss_state_year(clean, ref_weights = ref_weights)
  })

  csv_path <- path_project(cfg$paths$intermediate_root, "brfss_state_year.csv")
  rds_path <- path_project(cfg$paths$intermediate_root, "brfss_state_year.rds")
  safe_write_csv(out, csv_path)
  safe_write_rds(out, rds_path)

  note("Saved BRFSS state-year outcomes to {rds_path}")
  invisible(out)
}

if (sys.nframe() == 0) {
  build_brfss_panel()
}
