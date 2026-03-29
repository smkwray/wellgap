# 14_download_controls.R
# Control-variable resource discovery and selected scrapers.

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

required_pkgs <- c("dplyr", "purrr", "stringr", "readr", "tibble", "rvest", "xml2", "janitor")
invisible(lapply(required_pkgs, require, character.only = TRUE))

default_user_agent <- function() {
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
}

fetch_html_page <- function(url) {
  resp <- httr2::request(url) |>
    httr2::req_user_agent(default_user_agent()) |>
    httr2::req_perform()

  xml2::read_html(httr2::resp_body_raw(resp))
}

download_with_ua <- function(url, dest, overwrite = FALSE) {
  ensure_dir(dirname(dest))
  if (!file.exists(dest) || isTRUE(overwrite)) {
    note("Downloading {basename(dest)}")
    resp <- httr2::request(url) |>
      httr2::req_user_agent(default_user_agent()) |>
      httr2::req_perform()
    writeBin(httr2::resp_body_raw(resp), dest)
  } else {
    note("Skipping existing file {basename(dest)}")
  }
  dest
}

playwright_wrapper_path <- function() {
  env_path <- Sys.getenv("PLAYWRIGHT_WRAPPER", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }

  project_script <- path_project("scripts", "playwright_cli.sh")
  if (file.exists(project_script)) {
    return(project_script)
  }

  ""
}

run_playwright_cli <- function(args) {
  pwcli <- playwright_wrapper_path()
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)

  cmd <- sprintf(
    "%s %s > %s 2>&1",
    shQuote(pwcli),
    paste(vapply(args, shQuote, character(1)), collapse = " "),
    shQuote(tmp)
  )
  status <- system(cmd)
  out <- if (file.exists(tmp)) readLines(tmp, warn = FALSE) else character()
  attr(out, "status") <- status
  out
}

extract_playwright_result <- function(output) {
  txt <- paste(output, collapse = "\n")
  m <- regexec("(?s)### Result\\n(.*?)\\n### Ran Playwright code", txt, perl = TRUE)
  hit <- regmatches(txt, m)[[1]]
  if (length(hit) < 2) return(NA_character_)
  trimws(hit[[2]])
}

parse_max_number <- function(x) {
  purrr::map_dbl(as.character(x), function(val) {
    nums <- stringr::str_extract_all(val %||% "", "\\d+\\.?\\d*")[[1]]
    nums <- suppressWarnings(as.numeric(nums))
    nums <- nums[!is.na(nums)]
    if (length(nums) == 0) return(NA_real_)
    max(nums)
  })
}

discover_links <- function(page_url, pattern = "(?i)\\.(csv|xlsx|zip|xls|txt|pdf)$", save_path = NULL) {
  page <- try(fetch_html_page(page_url), silent = TRUE)
  if (inherits(page, "try-error")) {
    note("Could not open {page_url}; saving empty manifest.")
    out <- tibble::tibble(text = character(), href = character(), url = character())
    if (!is.null(save_path)) safe_write_csv(out, save_path)
    return(out)
  }
  links <- rvest::html_elements(page, "a")

  out <- tibble::tibble(
    text = rvest::html_text2(links),
    href = rvest::html_attr(links, "href")
  ) |>
    dplyr::mutate(url = xml2::url_absolute(href, page_url)) |>
    dplyr::filter(stringr::str_detect(url, pattern)) |>
    dplyr::distinct()

  if (!is.null(save_path)) safe_write_csv(out, save_path)
  out
}

download_dol_min_wage_resources <- function(overwrite = FALSE) {
  page_url <- cfg$controls$dol_min_wage_page
  manifest <- discover_links(
    page_url = page_url,
    save_path = path_project(cfg$paths$raw_root, "controls", "dol_min_wage_manifest.csv")
  )

  tables <- try(rvest::read_html(page_url) |> rvest::html_table(fill = TRUE), silent = TRUE)
  if (!inherits(tables, "try-error") && length(tables) > 0) {
    best <- NULL
    for (tb in tables) {
      nm <- names(tb)
      if (any(stringr::str_detect(nm, regex("state", ignore_case = TRUE)))) {
        best <- janitor::clean_names(tb)
        break
      }
    }
    if (!is.null(best)) {
      safe_write_csv(best, path_project(cfg$paths$intermediate_root, "state_minimum_wage_current_table.csv"))
    }
  }

  purrr::walk(manifest$url, function(u) {
    dest <- path_project(cfg$paths$raw_root, "controls", basename(u))
    try(download_with_ua(u, dest, overwrite = overwrite), silent = TRUE)
  })

  invisible(manifest)
}

discover_union_membership_resources <- function() {
  page_url <- cfg$controls$union_membership_page
  manifest <- discover_links(
    page_url = page_url,
    save_path = path_project(cfg$paths$raw_root, "controls", "union_membership_manifest.csv")
  )

  tables <- try(rvest::read_html(page_url) |> rvest::html_table(fill = TRUE), silent = TRUE)
  if (inherits(tables, "try-error")) {
    page <- try(fetch_html_page(page_url), silent = TRUE)
    if (!inherits(page, "try-error")) tables <- try(rvest::html_table(page, fill = TRUE), silent = TRUE)
  }
  if (!inherits(tables, "try-error")) {
    for (i in seq_along(tables)) {
      tb <- janitor::clean_names(tables[[i]])
      safe_write_csv(tb, path_project(cfg$paths$raw_root, "controls", sprintf("union_membership_table_%02d.csv", i)))
    }
  }

  invisible(manifest)
}

discover_fhfa_hpi_resources <- function(overwrite = FALSE) {
  page_url <- cfg$controls$fhfa_hpi_page
  manifest <- discover_links(
    page_url = page_url,
    save_path = path_project(cfg$paths$raw_root, "controls", "fhfa_hpi_manifest.csv")
  )

  purrr::walk(manifest$url, function(u) {
    dest <- path_project(cfg$paths$raw_root, "controls", basename(u))
    try(download_with_ua(u, dest, overwrite = overwrite), silent = TRUE)
  })

  invisible(manifest)
}

discover_building_permits_resources <- function(overwrite = FALSE) {
  page_url <- cfg$controls$building_permits_page
  manifest <- discover_links(
    page_url = page_url,
    save_path = path_project(cfg$paths$raw_root, "controls", "building_permits_manifest.csv")
  )

  purrr::walk(manifest$url, function(u) {
    dest <- path_project(cfg$paths$raw_root, "controls", basename(u))
    try(download_with_ua(u, dest, overwrite = overwrite), silent = TRUE)
  })

  invisible(manifest)
}

discover_qcew_resources <- function() {
  page_url <- cfg$controls$qcew_page
  manifest <- discover_links(
    page_url = page_url,
    save_path = path_project(cfg$paths$raw_root, "controls", "qcew_manifest.csv")
  )
  invisible(manifest)
}

qcew_state_area_code <- function(state_fips) {
  paste0(standardize_state_fips(state_fips), "000")
}

qcew_area_slice_url <- function(year, state_fips) {
  sprintf("https://data.bls.gov/cew/data/api/%s/a/area/%s.csv", as.integer(year), qcew_state_area_code(state_fips))
}

qcew_annual_archive_url <- function(year) {
  sprintf("https://data.bls.gov/cew/data/files/%s/csv/%s_annual_by_area.zip", as.integer(year), as.integer(year))
}

qcew_annual_archive_path <- function(year) {
  path_project(cfg$paths$raw_root, "controls", sprintf("%s_annual_by_area.zip", as.integer(year)))
}

qcew_api_year_cache <- local({
  cache <- new.env(parent = emptyenv())

  function(year, reset = FALSE) {
    key <- as.character(as.integer(year))
    if (isTRUE(reset) && exists(key, envir = cache, inherits = FALSE)) rm(list = key, envir = cache)
    if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache, inherits = FALSE))

    probe <- try(
      httr2::request(qcew_area_slice_url(year, "01")) |>
        httr2::req_user_agent(default_user_agent()) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform(),
      silent = TRUE
    )

    ok <- !inherits(probe, "try-error") && identical(httr2::resp_status(probe), 200L)
    assign(key, ok, envir = cache)
    ok
  }
})

download_qcew_annual_archive <- function(year, overwrite = FALSE) {
  archive_path <- qcew_annual_archive_path(year)
  download_with_ua(qcew_annual_archive_url(year), archive_path, overwrite = overwrite)
}

qcew_archive_members <- local({
  cache <- new.env(parent = emptyenv())

  function(archive_path, reset = FALSE) {
    key <- normalizePath(archive_path, winslash = "/", mustWork = FALSE)
    if (isTRUE(reset) && exists(key, envir = cache, inherits = FALSE)) rm(list = key, envir = cache)
    if (!exists(key, envir = cache, inherits = FALSE)) {
      assign(key, utils::unzip(archive_path, list = TRUE)$Name, envir = cache)
    }
    get(key, envir = cache, inherits = FALSE)
  }
})

qcew_statewide_archive_member <- function(year, state_fips, archive_path) {
  members <- qcew_archive_members(archive_path, reset = FALSE)
  target <- sprintf("%s.annual %s ", as.integer(year), qcew_state_area_code(state_fips))
  hit <- members[basename(members) |> startsWith(target)]
  if (length(hit) == 0) {
    stop("Could not find QCEW statewide archive entry for ", state_fips, " in ", archive_path, call. = FALSE)
  }
  hit[[1]]
}

read_qcew_statewide_archive <- function(year, state_fips, overwrite = FALSE, archive_path = NULL) {
  archive_path <- archive_path %||% download_qcew_annual_archive(year, overwrite = overwrite)
  member <- qcew_statewide_archive_member(year, state_fips, archive_path)
  tmpdir <- tempfile(pattern = "qcew-archive-")
  ensure_dir(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

  extracted <- utils::unzip(archive_path, files = member, exdir = tmpdir, overwrite = TRUE)
  safe_read_csv(extracted) |>
    dplyr::mutate(
      state_fips = standardize_state_fips(state_fips),
      year = as.integer(year)
    )
}

fetch_qcew_area_slice <- function(year, state_fips, overwrite = FALSE) {
  if (!isTRUE(qcew_api_year_cache(year, reset = overwrite))) {
    return(read_qcew_statewide_archive(year, state_fips, overwrite = overwrite))
  }

  url <- qcew_area_slice_url(year, state_fips)
  raw_path <- path_project(
    cfg$paths$raw_root, "controls", "qcew",
    sprintf("qcew_%s_%s.csv", as.integer(year), qcew_state_area_code(state_fips))
  )

  ensure_dir(dirname(raw_path))
  if (!file.exists(raw_path) || isTRUE(overwrite)) {
    resp <- httr2::request(url) |>
      httr2::req_user_agent(default_user_agent()) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()
    writeBin(httr2::resp_body_raw(resp), raw_path)
  }

  safe_read_csv(raw_path) |>
    dplyr::mutate(
      state_fips = standardize_state_fips(state_fips),
      year = as.integer(year)
    )
}

summarise_qcew_state_slice <- function(dat, state_fips, year) {
  dat <- dat |>
    dplyr::mutate(
      own_code = as.character(own_code),
      industry_code = as.character(industry_code),
      agglvl_code = as.integer(agglvl_code),
      size_code = as.character(size_code),
      annual_avg_emplvl = as.numeric(annual_avg_emplvl),
      annual_avg_wkly_wage = as.numeric(annual_avg_wkly_wage)
    )

  private_total <- dat |>
    dplyr::filter(own_code == "5", agglvl_code == 51, industry_code == "10", size_code == "0") |>
    dplyr::slice(1)

  total_emplvl <- if (nrow(private_total) == 0) NA_real_ else private_total$annual_avg_emplvl[[1]]
  total_wage <- if (nrow(private_total) == 0) NA_real_ else private_total$annual_avg_wkly_wage[[1]]

  sector_rows <- dat |>
    dplyr::filter(own_code == "5", agglvl_code == 54, size_code == "0")

  sector_map <- tibble::tribble(
    ~industry_code, ~sector,
    "21", "qcew_mining_share",
    "23", "qcew_construction_share",
    "31-33", "qcew_manufacturing_share",
    "42", "qcew_trade_transport_share",
    "44-45", "qcew_trade_transport_share",
    "48-49", "qcew_trade_transport_share",
    "51", "qcew_information_share",
    "52", "qcew_finance_real_estate_share",
    "53", "qcew_finance_real_estate_share",
    "54", "qcew_professional_business_share",
    "55", "qcew_professional_business_share",
    "56", "qcew_professional_business_share",
    "61", "qcew_education_health_share",
    "62", "qcew_education_health_share",
    "71", "qcew_leisure_hospitality_share",
    "72", "qcew_leisure_hospitality_share"
  )

  sectors <- sector_rows |>
    dplyr::inner_join(sector_map, by = "industry_code") |>
    dplyr::group_by(sector) |>
    dplyr::summarise(sector_emplvl = sum(annual_avg_emplvl, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = sector, values_from = sector_emplvl)

  out <- tibble::tibble(
    state_fips = standardize_state_fips(state_fips),
    year = as.integer(year),
    qcew_private_annual_avg_emplvl = total_emplvl,
    qcew_private_avg_wkly_wage = total_wage
  ) |>
    dplyr::bind_cols(sectors)

  share_cols <- setdiff(names(out), c("state_fips", "year", "qcew_private_annual_avg_emplvl", "qcew_private_avg_wkly_wage"))
  for (nm in share_cols) {
    out[[nm]] <- dplyr::if_else(
      !is.na(out$qcew_private_annual_avg_emplvl) & out$qcew_private_annual_avg_emplvl > 0,
      out[[nm]] / out$qcew_private_annual_avg_emplvl,
      NA_real_
    )
  }

  out
}

build_qcew_state_year <- function(
  years = cfg_vec(cfg$windows$brfss_years %||% cfg$windows$acs_years),
  overwrite = FALSE,
  mc.cores = 1L
) {
  state_map <- state_lookup() |> dplyr::select(state_fips, state, state_abbr)
  years <- sort(unique(as.integer(years)))

  if (!identical(as.integer(mc.cores), 1L)) {
    note("QCEW build currently uses sequential archive reads to avoid macOS fork-safety crashes.")
  }

  out <- purrr::map_dfr(years, function(year) {
    note("QCEW state-year for {year}")
    if (isTRUE(qcew_api_year_cache(year, reset = overwrite))) {
      purrr::map_dfr(state_map$state_fips, function(state_fips) {
        dat <- fetch_qcew_area_slice(year, state_fips, overwrite = overwrite)
        summarise_qcew_state_slice(dat, state_fips, year)
      })
    } else {
      archive_path <- download_qcew_annual_archive(year, overwrite = overwrite)
      qcew_archive_members(archive_path, reset = overwrite)

      purrr::map_dfr(state_map$state_fips, function(state_fips) {
        dat <- read_qcew_statewide_archive(year, state_fips, overwrite = overwrite, archive_path = archive_path)
        summarise_qcew_state_slice(dat, state_fips, year)
      })
    }
  }) |>
    dplyr::left_join(state_map, by = "state_fips") |>
    dplyr::select(
      state_fips, state, state_abbr, year,
      qcew_private_annual_avg_emplvl, qcew_private_avg_wkly_wage,
      qcew_mining_share, qcew_construction_share, qcew_manufacturing_share,
      qcew_trade_transport_share, qcew_information_share,
      qcew_finance_real_estate_share, qcew_professional_business_share,
      qcew_education_health_share, qcew_leisure_hospitality_share
    ) |>
    dplyr::arrange(state, year)

  out_path <- path_project(cfg$paths$intermediate_root, "qcew_state_year.csv")
  safe_write_csv(out, out_path)
  invisible(out)
}

discover_laus_resources <- function() {
  page_url <- cfg$controls$laus_page
  manifest <- discover_links(
    page_url = page_url,
    save_path = path_project(cfg$paths$raw_root, "controls", "laus_manifest.csv")
  )
  invisible(manifest)
}

download_controls_manifests <- function(overwrite = FALSE) {
  note("Discovering optional control-data resources.")
  download_dol_min_wage_resources(overwrite = overwrite)
  discover_union_membership_resources()
  discover_fhfa_hpi_resources(overwrite = overwrite)
  discover_building_permits_resources(overwrite = overwrite)
  discover_qcew_resources()
  discover_laus_resources()

  note("Saved control-data manifests to data/raw/controls/")
  invisible(TRUE)
}

build_fhfa_state_hpi <- function(overwrite = FALSE) {
  url <- "https://www.fhfa.gov/hpi/download/quarterly_datasets/hpi_at_state.csv"
  raw_path <- path_project(cfg$paths$raw_root, "controls", "hpi_at_state.csv")
  download_with_ua(url, raw_path, overwrite = overwrite)

  dat <- safe_read_csv(raw_path, col_names = c("state_abbr", "year", "quarter", "hpi")) |>
    dplyr::mutate(
      state_abbr = stringr::str_trim(state_abbr),
      year = as.integer(year),
      quarter = as.integer(quarter),
      hpi = as.numeric(hpi)
    ) |>
    dplyr::filter(year >= cfg$windows$state_year_start, year <= cfg$windows$state_year_end) |>
    dplyr::group_by(state_abbr, year) |>
    dplyr::summarise(hpi = mean(hpi, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(state_lookup() |> dplyr::select(state_fips, state, state_abbr), by = "state_abbr") |>
    dplyr::select(state_fips, state, state_abbr, year, hpi) |>
    dplyr::arrange(state, year)

  out_path <- path_project(cfg$paths$intermediate_root, "fhfa_state_hpi.csv")
  safe_write_csv(dat, out_path)
  invisible(dat)
}

download_dol_min_wage_history_tables <- function(overwrite = FALSE) {
  url <- "https://www.dol.gov/agencies/whd/state/minimum-wage/history"
  raw_path <- path_project(cfg$paths$raw_root, "controls", "dol_min_wage_history_tables.json")
  if (file.exists(raw_path) && !isTRUE(overwrite)) return(raw_path)

  pwcli <- playwright_wrapper_path()
  if (!nzchar(pwcli) || !file.exists(pwcli)) {
    stop(
      "Playwright wrapper not found. Set PLAYWRIGHT_WRAPPER to a compatible script path.",
      call. = FALSE
    )
  }

  note("Rendering DOL minimum wage history page in Playwright")
  open_out <- run_playwright_cli(c("open", url))
  on.exit(try(run_playwright_cli("close"), silent = TRUE), add = TRUE)

  if (!any(grepl("Page Title: Changes in Basic Minimum Wages", open_out, fixed = TRUE))) {
    note("Playwright open output did not confirm the expected DOL title; continuing to table extraction.")
  }

  js <- "() => JSON.stringify(Array.from(document.querySelectorAll(\"table\")).map(t => t.outerHTML))"
  eval_out <- run_playwright_cli(c("eval", js))
  result <- extract_playwright_result(eval_out)
  if (is.na(result) || !nzchar(result)) {
    stop("Could not extract rendered DOL minimum-wage tables from Playwright output.", call. = FALSE)
  }

  writeLines(result, raw_path)
  raw_path
}

read_dol_min_wage_tables <- function(overwrite = FALSE) {
  raw_path <- download_dol_min_wage_history_tables(overwrite = overwrite)
  html_tables <- jsonlite::fromJSON(paste(readLines(raw_path, warn = FALSE), collapse = "\n"))
  if (length(html_tables) == 1 && is.character(html_tables) && grepl("^\\s*\\[", html_tables[[1]])) {
    html_tables <- jsonlite::fromJSON(html_tables[[1]])
  }
  purrr::map(html_tables, function(html) {
    rvest::html_table(xml2::read_html(html), fill = TRUE)[[1]]
  })
}

build_state_minimum_wage <- function(overwrite = FALSE) {
  state_map <- state_lookup() |> dplyr::select(state_fips, state, state_abbr)
  source_tables <- read_dol_min_wage_tables(overwrite = overwrite)

  long <- purrr::map_dfr(source_tables, function(tb) {
    if (ncol(tb) < 2) return(tibble::tibble())
    names(tb)[1] <- "state"

    tb |>
      dplyr::mutate(state = stringr::str_squish(as.character(state))) |>
      dplyr::filter(state %in% c("Federal (FLSA)", state_map$state)) |>
      tidyr::pivot_longer(-state, names_to = "year_label", values_to = "wage_raw") |>
      dplyr::mutate(
        year = readr::parse_integer(stringr::str_extract(year_label, "\\d{4}")),
        wage_raw = as.character(wage_raw)
      ) |>
      dplyr::filter(!is.na(year))
  }) |>
    dplyr::distinct(state, year, .keep_all = TRUE)

  federal <- long |>
    dplyr::filter(state == "Federal (FLSA)") |>
    dplyr::transmute(year, federal_min_wage = parse_max_number(wage_raw))

  out <- long |>
    dplyr::filter(state %in% state_map$state) |>
    dplyr::transmute(state, year, state_wage = parse_max_number(wage_raw)) |>
    dplyr::left_join(federal, by = "year") |>
    dplyr::mutate(
      state_min_wage_nominal = dplyr::case_when(
        !is.na(state_wage) & !is.na(federal_min_wage) ~ pmax(state_wage, federal_min_wage),
        !is.na(state_wage) ~ state_wage,
        TRUE ~ federal_min_wage
      )
    ) |>
    dplyr::filter(year >= cfg$windows$state_year_start, year <= cfg$windows$state_year_end) |>
    dplyr::left_join(state_map, by = "state") |>
    dplyr::select(state_fips, state, state_abbr, year, state_min_wage_nominal, federal_min_wage, state_wage) |>
    dplyr::arrange(state, year)

  out_path <- path_project(cfg$paths$intermediate_root, "state_minimum_wage.csv")
  safe_write_csv(out, out_path)
  invisible(out)
}

union_membership_release_urls <- function() {
  c(
    "https://www.bls.gov/news.release/archives/union2_01232013.htm",
    "https://www.bls.gov/news.release/archives/union2_01242014.htm",
    "https://www.bls.gov/news.release/archives/union2_01232015.htm",
    "https://www.bls.gov/news.release/archives/union2_01282016.htm",
    "https://www.bls.gov/news.release/archives/union2_01262017.htm",
    "https://www.bls.gov/news.release/archives/union2_01192018.htm",
    "https://www.bls.gov/news.release/archives/union2_01182019.htm",
    "https://www.bls.gov/news.release/archives/union2_01222020.htm",
    "https://www.bls.gov/news.release/archives/union2_01222021.htm",
    "https://www.bls.gov/news.release/archives/union2_01202022.htm",
    "https://www.bls.gov/news.release/archives/union2_01192023.htm",
    "https://www.bls.gov/news.release/archives/union2_01232024.htm",
    "https://www.bls.gov/news.release/archives/union2_01282025.htm"
  )
}

select_union_state_table <- function(tables) {
  purrr::detect(tables, function(tb) {
    ncol(tb) >= 11 &&
      identical(names(tb)[[1]], "State") &&
      any(tb[[1]] %in% c("Alabama", "District of Columbia"), na.rm = TRUE)
  })
}

parse_union_state_table <- function(tb) {
  if (is.null(tb)) stop("Could not find the BLS state union-membership table.", call. = FALSE)

  valid_states <- state_lookup()$state
  start_cols <- seq(2, ncol(tb), by = 5)
  year_labels <- names(tb)[start_cols]
  names(tb) <- paste0("col", seq_len(ncol(tb)))

  data_rows <- tb[-c(1, 2), , drop = FALSE] |>
    dplyr::transmute(
      state = stringr::str_squish(as.character(.data[["col1"]])),
      dplyr::across(dplyr::everything())
    ) |>
    dplyr::filter(state %in% valid_states)

  purrr::map_dfr(start_cols, function(start_col) {
    year <- readr::parse_integer(stringr::str_extract(year_labels[[which(start_cols == start_col)]], "\\d{4}"))
    col_name <- function(idx) paste0("col", idx)

    tibble::tibble(
      state = data_rows$state,
      year = year,
      union_employed_thousands = readr::parse_number(as.character(data_rows[[col_name(start_col)]])),
      union_members_thousands = readr::parse_number(as.character(data_rows[[col_name(start_col + 1)]])),
      union_membership_rate = readr::parse_number(as.character(data_rows[[col_name(start_col + 2)]])),
      represented_thousands = readr::parse_number(as.character(data_rows[[col_name(start_col + 3)]])),
      represented_rate = readr::parse_number(as.character(data_rows[[col_name(start_col + 4)]]))
    )
  })
}

build_union_membership_state_year <- function(overwrite = FALSE) {
  state_map <- state_lookup() |> dplyr::select(state_fips, state, state_abbr)

  out <- purrr::map_dfr(union_membership_release_urls(), function(url) {
    raw_path <- path_project(cfg$paths$raw_root, "controls", basename(url))
    download_with_ua(url, raw_path, overwrite = overwrite)

    tables <- rvest::html_table(xml2::read_html(raw_path), fill = TRUE)
    parse_union_state_table(select_union_state_table(tables))
  }) |>
    dplyr::filter(state %in% state_map$state) |>
    dplyr::distinct(state, year, .keep_all = TRUE) |>
    dplyr::filter(year >= cfg$windows$state_year_start, year <= cfg$windows$state_year_end) |>
    dplyr::left_join(state_map, by = "state") |>
    dplyr::select(
      state_fips, state, state_abbr, year,
      union_employed_thousands, union_members_thousands, union_membership_rate,
      represented_thousands, represented_rate
    ) |>
    dplyr::arrange(state, year)

  out_path <- path_project(cfg$paths$intermediate_root, "union_membership_state_year.csv")
  safe_write_csv(out, out_path)
  invisible(out)
}

read_permits_xlsx_year <- function(year, overwrite = FALSE) {
  url <- sprintf("https://www.census.gov/construction/bps/xls/stateannual_%s99.xls", year)
  raw_path <- path_project(cfg$paths$raw_root, "controls", sprintf("stateannual_%s99.xls", year))
  download_with_ua(url, raw_path, overwrite = overwrite)

  sig <- readBin(raw_path, what = "raw", n = 4)
  read_path <- raw_path
  if (length(sig) >= 2 && identical(rawToChar(sig[1:2]), "PK")) {
    read_path <- path_project(cfg$paths$raw_root, "controls", sprintf("stateannual_%s99.xlsx", year))
    file.copy(raw_path, read_path, overwrite = TRUE)
  }

  dat <- readxl::read_excel(read_path)
  if (ncol(dat) < 2) {
    stop("Unexpected building permits workbook layout for year ", year, call. = FALSE)
  }
  names(dat)[1:2] <- c("state", "permits")

  dat |>
    dplyr::transmute(state = stringr::str_squish(as.character(state)), permits = as.character(permits)) |>
    dplyr::filter(state %in% state_lookup()$state) |>
    dplyr::mutate(permits = readr::parse_number(permits)) |>
    dplyr::filter(!is.na(permits)) |>
    dplyr::mutate(year = as.integer(year))
}

read_permits_txt_year <- function(year, overwrite = FALSE) {
  url <- sprintf("https://www.census.gov/construction/bps/txt/tb2u%s.txt", year)
  raw_path <- path_project(cfg$paths$raw_root, "controls", sprintf("tb2u%s.txt", year))
  download_with_ua(url, raw_path, overwrite = overwrite)

  lines <- readLines(raw_path, warn = FALSE)
  states <- state_lookup()$state
  escape_regex <- function(x) gsub("([][{}()+*^$.|?\\\\])", "\\\\\\1", x)
  state_pattern <- paste0("^\\s*(", paste(escape_regex(states), collapse = "|"), ")\\s{2,}")
  hits <- lines[stringr::str_detect(lines, state_pattern)]

  if (length(hits) == 0) {
    stop("No state rows found in permits text file for year ", year, call. = FALSE)
  }

  out <- purrr::map_dfr(hits, function(line) {
    trimmed <- trimws(line)
    state <- purrr::detect(states, ~ stringr::str_detect(trimmed, paste0("^", escape_regex(.x), "\\s{2,}")))
    if (is.null(state)) return(NULL)

    tail <- sub(paste0("^", escape_regex(state), "\\s+"), "", trimmed, perl = TRUE)
    nums <- readr::parse_number(unlist(strsplit(stringr::str_squish(tail), "\\s+")))
    if (length(nums) == 0 || is.na(nums[[1]])) return(NULL)

    tibble::tibble(state = state, permits = nums[[1]], year = as.integer(year))
  })

  out
}

build_building_permits_state_year <- function(years = cfg$windows$state_year_start:cfg$windows$state_year_end, overwrite = FALSE) {
  out <- purrr::map_dfr(years, function(year) {
    note("Building permits state-year for {year}")
    if (year >= 2019) {
      read_permits_xlsx_year(year, overwrite = overwrite)
    } else {
      read_permits_txt_year(year, overwrite = overwrite)
    }
  }) |>
    dplyr::left_join(state_lookup() |> dplyr::select(state_fips, state, state_abbr), by = "state") |>
    dplyr::select(state_fips, state, state_abbr, year, permits) |>
    dplyr::arrange(state, year)

  out_path <- path_project(cfg$paths$intermediate_root, "building_permits_state_year.csv")
  safe_write_csv(out, out_path)
  invisible(out)
}

if (sys.nframe() == 0) {
  download_controls_manifests()
}
