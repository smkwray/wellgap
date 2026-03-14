# 11_download_bea.R
# BEA API helpers + distribution-file discovery.

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

required_pkgs <- c("dplyr", "purrr", "stringr", "readr", "tibble", "tidyr", "readxl", "httr2", "jsonlite", "rvest", "xml2", "janitor")
invisible(lapply(required_pkgs, require, character.only = TRUE))

bea_key <- function() {
  key_name <- cfg$api_keys$bea %||% "BEA_API_KEY"
  key_value <- Sys.getenv(key_name)
  if (!nzchar(key_value)) {
    stop(sprintf("Set %s in your environment before running BEA downloads.", key_name), call. = FALSE)
  }
  key_value
}

bea_request <- function(query) {
  resp <- httr2::request(cfg$bea$api_base %||% "https://apps.bea.gov/api/data") |>
    httr2::req_url_query(
      UserID = bea_key(),
      ResultFormat = "json",
      !!!query
    ) |>
    httr2::req_perform()

  txt <- httr2::resp_body_string(resp)
  out <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)

  if (!is.null(out$BEAAPI$Error)) {
    stop(paste("BEA API error:", out$BEAAPI$Error$errorMessage), call. = FALSE)
  }

  out$BEAAPI$Results
}

discover_bea_datasets <- function(save = TRUE) {
  res <- bea_request(list(method = "GetDataSetList"))
  dat <- tibble::as_tibble(res$Dataset)
  if (save) safe_write_csv(dat, path_project(cfg$paths$raw_root, "bea", "bea_dataset_catalog.csv"))
  dat
}

discover_bea_parameters <- function(dataset = "Regional", save = TRUE) {
  res <- bea_request(list(method = "GetParameterList", datasetname = dataset))
  dat <- tibble::as_tibble(res$Parameter) |> janitor::clean_names()
  if (save) safe_write_csv(dat, path_project(cfg$paths$raw_root, "bea", paste0("bea_", tolower(dataset), "_parameter_catalog.csv")))
  dat
}

discover_bea_parameter_values <- function(dataset = "Regional", parameter = "TableName", save = TRUE) {
  res <- bea_request(list(
    method = "GetParameterValues",
    datasetname = dataset,
    ParameterName = parameter
  ))
  dat <- tibble::as_tibble(res$ParamValue) |> janitor::clean_names()
  if (save) {
    safe_write_csv(
      dat,
      path_project(cfg$paths$raw_root, "bea", paste0("bea_", tolower(dataset), "_", tolower(parameter), "_values.csv"))
    )
  }
  dat
}

search_bea_tables <- function(pattern, dataset = "Regional", save = TRUE) {
  tables <- discover_bea_parameter_values(dataset = dataset, parameter = "TableName", save = save)
  nm_col <- first_non_missing_name(tables, c("key", "param_value"))
  desc_col <- first_non_missing_name(tables, c("desc", "description", "key_description"))

  if (is.na(nm_col) && is.na(desc_col)) {
    stop("Could not find table-name columns in BEA metadata.")
  }

  text_to_search <- if (!is.na(desc_col) && !is.na(nm_col)) {
    dplyr::coalesce(tables[[desc_col]], tables[[nm_col]])
  } else if (!is.na(desc_col)) {
    tables[[desc_col]]
  } else {
    tables[[nm_col]]
  }

  out <- tables |>
    dplyr::mutate(search_text = text_to_search) |>
    dplyr::filter(stringr::str_detect(search_text, regex(pattern, ignore_case = TRUE)))

  if (save) {
    safe_write_csv(out, path_project(cfg$paths$raw_root, "bea", paste0("bea_table_search_", gsub("[^A-Za-z0-9]+", "_", pattern), ".csv")))
  }

  out
}

download_bea_table <- function(table_name, dataset = "Regional", geo_fips = "STATE", year = "ALL", line_code = "ALL") {
  if (identical(geo_fips, "STATE") && identical(line_code, "ALL")) {
    state_geo_fips <- paste0(state_lookup()$state_fips, "000")
    out <- purrr::map_dfr(state_geo_fips, function(one_geo) {
      res <- bea_request(list(
        method = "GetData",
        datasetname = dataset,
        TableName = table_name,
        GeoFips = one_geo,
        Year = year,
        LineCode = line_code
      ))
      tibble::as_tibble(res$Data)
    })
    return(janitor::clean_names(out))
  }

  res <- bea_request(list(
    method = "GetData",
    datasetname = dataset,
    TableName = table_name,
    GeoFips = geo_fips,
    Year = year,
    LineCode = line_code
  ))
  tibble::as_tibble(res$Data) |> janitor::clean_names()
}

download_configured_bea_alias <- function(alias) {
  spec <- cfg$bea$tables[[alias]]
  if (is.null(spec)) {
    stop(sprintf("Alias '%s' not found in config$bea$tables.", alias), call. = FALSE)
  }

  if (is.null(spec$table_name) || !nzchar(spec$table_name)) {
    note("Skipping BEA alias {alias}: no table_name set in config/config.yml")
    return(invisible(NULL))
  }

  dat <- download_bea_table(
    table_name = spec$table_name,
    dataset = spec$dataset %||% "Regional",
    geo_fips = spec$geo_fips %||% "STATE",
    year = spec$year %||% "ALL",
    line_code = spec$line_code %||% "ALL"
  )

  out_path <- path_project(cfg$paths$intermediate_root, paste0(alias, ".csv"))
  safe_write_csv(dat, out_path)
  note("Saved BEA alias {alias} -> {out_path}")
  invisible(dat)
}

download_configured_bea_aliases <- function() {
  aliases <- names(cfg$bea$tables)
  invisible(lapply(aliases, download_configured_bea_alias))
}

discover_bea_distribution_downloads <- function(
  page_url = cfg$bea$distribution_page %||% "https://www.bea.gov/data/special-topics/distribution-of-personal-income",
  save = TRUE
) {
  page <- rvest::read_html(page_url)
  links <- rvest::html_elements(page, "a")
  hrefs <- rvest::html_attr(links, "href")
  texts <- rvest::html_text2(links)

  out <- tibble::tibble(
    text = texts,
    href = hrefs
  ) |>
    dplyr::mutate(
      url = xml2::url_absolute(href, page_url)
    ) |>
    dplyr::filter(stringr::str_detect(url, "(?i)\\.(xlsx|csv|zip)$")) |>
    dplyr::distinct()

  if (save) {
    safe_write_csv(out, path_project(cfg$paths$raw_root, "bea", "bea_distribution_download_manifest.csv"))
  }

  out
}

download_bea_distribution_files <- function(max_files = NULL, overwrite = FALSE) {
  manifest <- discover_bea_distribution_downloads(save = TRUE)
  if (!is.null(max_files)) manifest <- head(manifest, max_files)

  purrr::walk(manifest$url, function(u) {
    dest <- path_project(cfg$paths$raw_root, "bea", basename(u))
    try(download_if_needed(u, dest, overwrite = overwrite), silent = FALSE)
  })

  invisible(manifest)
}

find_bea_distribution_state_workbook <- function(manifest = NULL) {
  manifest <- manifest %||% discover_bea_distribution_downloads(save = TRUE)

  hit <- manifest |>
    dplyr::filter(stringr::str_to_lower(basename(url)) == "distributional-metrics.xlsx") |>
    dplyr::slice(1)

  if (nrow(hit) == 0) {
    stop("Could not find the BEA state distribution workbook in the download manifest.", call. = FALSE)
  }

  hit$url[[1]]
}

normalize_bea_distribution_description <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[*]", "") |>
    stringr::str_squish()
}

read_bea_distribution_sheet <- function(path, sheet, prefix) {
  dat <- readxl::read_excel(path, sheet = sheet, skip = 2)
  year_cols <- names(dat)[stringr::str_detect(names(dat), "^\\d{4}")]

  if (length(year_cols) == 0) {
    stop("Could not identify BEA distribution year columns in sheet: ", sheet, call. = FALSE)
  }

  dat |>
    dplyr::filter(
      !is.na(.data[["GeoFIPS"]]),
      !is.na(.data[["Description"]]),
      stringr::str_detect(.data[["GeoFIPS"]], "^[0-9]+$")
    ) |>
    dplyr::mutate(
      state_fips = stringr::str_pad(
        as.character(as.integer(readr::parse_number(.data[["GeoFIPS"]]) / 1000)),
        width = 2,
        side = "left",
        pad = "0"
      ),
      state = .data[["GeoName"]],
      description = normalize_bea_distribution_description(.data[["Description"]])
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(year_cols),
      names_to = "year",
      values_to = "value_raw",
      values_transform = list(value_raw = as.character)
    ) |>
    dplyr::transmute(
      state_fips,
      state,
      year = readr::parse_number(year),
      metric = dplyr::case_when(
        description == "gini coefficient" ~ paste0(prefix, "_gini"),
        description == "top 10% share" ~ paste0(prefix, "_top10_share"),
        description == "median personal income (nominal dollars)" ~ "pers_median",
        description == "mean personal income (nominal dollars)" ~ "pers_mean",
        description == "median disposable personal income (nominal dollars)" ~ "disp_median",
        description == "mean disposable personal income (nominal dollars)" ~ "disp_mean",
        description == "eq. 80/20 ratio" ~ paste0(prefix, "_eq_80_20_ratio"),
        description == "eq. 90/10 ratio" ~ paste0(prefix, "_eq_90_10_ratio"),
        TRUE ~ NA_character_
      ),
      value = readr::parse_number(as.character(value_raw))
    ) |>
    dplyr::filter(!is.na(metric), !is.na(state_fips), !is.na(year)) |>
    dplyr::group_by(state_fips, state, year, metric) |>
    dplyr::summarise(value = dplyr::first(value), .groups = "drop")
}

build_bea_distribution_state_year <- function(overwrite = FALSE) {
  manifest <- discover_bea_distribution_downloads(save = TRUE)
  workbook_url <- find_bea_distribution_state_workbook(manifest)
  workbook_path <- path_project(cfg$paths$raw_root, "bea", basename(workbook_url))
  out_csv <- path_project(cfg$paths$intermediate_root, "bea_distribution_state_year.csv")
  out_rds <- path_project(cfg$paths$intermediate_root, "bea_distribution_state_year.rds")

  download_if_needed(workbook_url, workbook_path, overwrite = overwrite)

  pers <- read_bea_distribution_sheet(workbook_path, "PI Distributional Metrics", "pers")
  disp <- read_bea_distribution_sheet(workbook_path, "DPI Distributional Metrics", "disp")

  out <- dplyr::bind_rows(pers, disp) |>
    tidyr::pivot_wider(names_from = metric, values_from = value) |>
    dplyr::left_join(
      state_lookup() |>
        dplyr::select(state_fips, state_lookup = state),
      by = "state_fips"
    ) |>
    dplyr::mutate(
      state = dplyr::coalesce(state, state_lookup)
    ) |>
    dplyr::select(-dplyr::any_of("state_lookup")) |>
    dplyr::arrange(state_fips, year)

  safe_write_csv(out, out_csv)
  safe_write_rds(out, out_rds)
  note("Saved BEA distribution state-year data to {out_csv}")
  invisible(out)
}

extract_bea_series_from_file <- function(path, pattern, value_name) {
  dat <- safe_read_csv(path) |> janitor::clean_names()

  desc_col <- first_non_missing_name(dat, c("line_description", "description", "series_description"))
  geo_col  <- first_non_missing_name(dat, c("geo_fips", "geofips"))
  name_col <- first_non_missing_name(dat, c("geo_name", "geoname"))
  year_col <- first_non_missing_name(dat, c("time_period", "year"))
  val_col  <- first_non_missing_name(dat, c("data_value", "value"))
  um_col   <- first_non_missing_name(dat, c("unit_mult", "unit_multiplier"))

  if (any(is.na(c(desc_col, geo_col, year_col, val_col)))) {
    stop("Could not find required BEA columns in: ", path, call. = FALSE)
  }

  out <- dat |>
    dplyr::filter(stringr::str_detect(.data[[desc_col]], regex(pattern, ignore_case = TRUE))) |>
    dplyr::transmute(
      state_fips = standardize_state_fips(.data[[geo_col]]),
      state = if (!is.na(name_col)) .data[[name_col]] else NA_character_,
      year = suppressWarnings(as.integer(.data[[year_col]])),
      !!value_name := parse_bea_value(.data[[val_col]], if (!is.na(um_col)) .data[[um_col]] else NULL)
    ) |>
    dplyr::filter(!is.na(state_fips), !is.na(year)) |>
    dplyr::group_by(state_fips, state, year) |>
    dplyr::summarise(!!value_name := dplyr::first(.data[[value_name]]), .groups = "drop")

  out
}

extract_bea_series_from_candidates <- function(paths, pattern, value_name) {
  paths <- unique(paths[file.exists(paths)])
  for (path in paths) {
    out <- try(extract_bea_series_from_file(path, pattern, value_name), silent = TRUE)
    if (inherits(out, "try-error")) next
    if (nrow(out) > 0) return(out)
  }
  NULL
}

build_bea_state_core <- function() {
  alias_map <- cfg$bea$aliases
  pce_path <- path_project(cfg$paths$intermediate_root, paste0(alias_map$pce, ".csv"))
  inc_path <- path_project(cfg$paths$intermediate_root, paste0(alias_map$income, ".csv"))
  rpp_path <- path_project(cfg$paths$intermediate_root, paste0(alias_map$rpp, ".csv"))

  needed <- c(pce_path, inc_path, rpp_path)
  missing <- needed[!file.exists(needed)]
  if (length(missing) > 0) {
    stop(
      paste(
        "Missing one or more configured BEA files.",
        "Run BEA metadata discovery, set table names in config, then run download_configured_bea_aliases().",
        "Missing paths:",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  out_list <- list(
    real_pce = extract_bea_series_from_candidates(c(pce_path, inc_path), cfg$bea$regex$real_pce, "real_pce"),
    current_pce = extract_bea_series_from_candidates(c(pce_path, inc_path), cfg$bea$regex$current_pce, "current_pce"),
    real_personal_income = extract_bea_series_from_candidates(c(inc_path, pce_path), cfg$bea$regex$real_personal_income, "real_personal_income"),
    personal_income = extract_bea_series_from_candidates(c(inc_path, pce_path), cfg$bea$regex$personal_income, "personal_income"),
    bea_population = extract_bea_series_from_candidates(c(inc_path, pce_path), cfg$bea$regex$population, "bea_population"),
    rpp_all_items = extract_bea_series_from_candidates(c(rpp_path), cfg$bea$regex$rpp_all_items, "rpp_all_items")
  )

  missing_series <- names(out_list)[vapply(out_list, is.null, logical(1))]
  if (length(missing_series) > 0) {
    stop(
      "Could not extract required BEA series from configured alias files: ",
      paste(missing_series, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(cfg$bea$regex$rpp_housing) && nzchar(cfg$bea$regex$rpp_housing)) {
    maybe_housing <- try(extract_bea_series_from_file(rpp_path, cfg$bea$regex$rpp_housing, "rpp_housing"), silent = TRUE)
    if (!inherits(maybe_housing, "try-error")) out_list <- append(out_list, list(rpp_housing = maybe_housing))
  }

  bea_core <- purrr::reduce(out_list, dplyr::full_join, by = c("state_fips", "state", "year")) |>
    dplyr::arrange(state_fips, year)

  safe_write_csv(bea_core, path_project(cfg$paths$intermediate_root, "bea_state_core.csv"))
  note("Saved standardized BEA core file.")
  invisible(bea_core)
}

if (sys.nframe() == 0) {
  note("Running BEA metadata discovery only. Fill config/config.yml before full downloads.")
  discover_bea_datasets()
  discover_bea_parameters()
  search_bea_tables("consumer spending")
  search_bea_tables("regional price")
  search_bea_tables("personal income")
  discover_bea_distribution_downloads()
}
