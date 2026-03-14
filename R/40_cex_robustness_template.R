# 40_cex_robustness_template.R
# Template for a CEX robustness exercise.
# This module is intentionally scaffold-like because CEX file structures and geography
# choices vary by year. Use it after the main BEA state panel is working.

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

required_pkgs <- c("dplyr", "purrr", "stringr", "readr", "tibble", "rvest", "xml2")
invisible(lapply(required_pkgs, require, character.only = TRUE))

discover_cex_resources <- function(page_url = cfg$cex$pumd_page) {
  page <- rvest::read_html(page_url)
  links <- rvest::html_elements(page, "a")
  out <- tibble::tibble(
    text = rvest::html_text2(links),
    href = rvest::html_attr(links, "href")
  ) |>
    dplyr::mutate(url = xml2::url_absolute(href, page_url)) |>
    dplyr::filter(stringr::str_detect(url, "(?i)\\.(zip|csv|txt|pdf)$")) |>
    dplyr::distinct()

  safe_write_csv(out, path_project(cfg$paths$raw_root, "cex", "cex_manifest.csv"))
  out
}

# Suggested workflow:
# 1. Download the chosen CEX PUMD year(s)
# 2. Identify the family/integration files you need
# 3. Keep only selected states / selected MSAs with available geography
# 4. Build annual expenditure aggregates using the relevant weights
# 5. Merge to ACS / BEA inequality at the same geography and year
# 6. Treat the result as a validation / robustness exercise, not the core panel

# Suggested standardized output columns for later merge:
#   geo_id, geo_name, year,
#   total_expenditure,
#   food_expenditure,
#   housing_expenditure,
#   transport_expenditure,
#   health_expenditure,
#   cex_weight,
#   selected_geo_type

if (sys.nframe() == 0) {
  discover_cex_resources()
}
