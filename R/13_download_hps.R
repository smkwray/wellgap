# 13_download_hps.R
# Discover and optionally download Household Pulse Survey / HTOPS public-use files.
# This is an extension module. Harmonization across waves should be handled explicitly.

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

discover_hps_files <- function(page_url = cfg$hps$puf_page, save = TRUE) {
  page <- rvest::read_html(page_url)
  links <- rvest::html_elements(page, "a")

  out <- tibble::tibble(
    text = rvest::html_text2(links),
    href = rvest::html_attr(links, "href")
  ) |>
    dplyr::mutate(
      url = xml2::url_absolute(href, page_url)
    ) |>
    dplyr::filter(stringr::str_detect(url, "(?i)\\.(csv|zip|xlsx)$")) |>
    dplyr::distinct()

  if (save) {
    safe_write_csv(out, path_project(cfg$paths$raw_root, "hps", "hps_htops_download_manifest.csv"))
  }

  out
}

download_hps_files <- function(pattern = NULL, max_files = NULL, overwrite = FALSE) {
  manifest <- discover_hps_files(save = TRUE)

  if (!is.null(pattern)) {
    manifest <- manifest |>
      dplyr::filter(stringr::str_detect(text, regex(pattern, ignore_case = TRUE)) |
                      stringr::str_detect(url, regex(pattern, ignore_case = TRUE)))
  }

  if (!is.null(max_files)) {
    manifest <- head(manifest, max_files)
  }

  purrr::walk(manifest$url, function(u) {
    dest <- path_project(cfg$paths$raw_root, "hps", basename(u))
    try(download_if_needed(u, dest, overwrite = overwrite), silent = FALSE)
  })

  invisible(manifest)
}

aggregate_hps_with_mapping <- function(df, mapping, state_var = "EST_ST", weight_var = "PWEIGHT", year_var = "YEAR") {
  stopifnot(length(mapping) > 0)

  if (!(state_var %in% names(df))) stop("state_var not found in data.", call. = FALSE)
  if (!(weight_var %in% names(df))) stop("weight_var not found in data.", call. = FALSE)
  if (!(year_var %in% names(df))) stop("year_var not found in data.", call. = FALSE)

  out <- tibble::as_tibble(df) |>
    dplyr::mutate(
      state_fips = standardize_state_fips(.data[[state_var]]),
      hps_weight = as.numeric(.data[[weight_var]]),
      year = as.integer(.data[[year_var]])
    )

  for (nm in names(mapping)) {
    spec <- mapping[[nm]]
    var_name <- spec$var
    positive <- as.numeric(unlist(spec$positive))
    if (!(var_name %in% names(out))) {
      note("Skipping mapped outcome {nm}; variable {var_name} not found.")
      next
    }
    out[[nm]] <- dplyr::case_when(
      out[[var_name]] %in% positive ~ 1,
      !is.na(out[[var_name]]) ~ 0,
      TRUE ~ NA_real_
    )
  }

  value_vars <- intersect(names(mapping), names(out))
  if (length(value_vars) == 0) {
    stop("No mapped variables were found in the provided HPS / HTOPS data.", call. = FALSE)
  }

  out |>
    dplyr::group_by(state_fips, year) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(value_vars),
        ~ weighted_mean_na(.x, hps_weight),
        .names = "{.col}"
      ),
      hps_n = dplyr::n(),
      hps_effective_n = effective_n(hps_weight),
      .groups = "drop"
    ) |>
    dplyr::left_join(state_lookup(), by = "state_fips")
}

example_hps_mapping <- list(
  food_insufficiency = list(var = "CURFOODSUF", positive = c(3, 4)),
  difficulty_expenses = list(var = "DIFFPAY", positive = c(3, 4))
)

if (sys.nframe() == 0) {
  discover_hps_files()
}
