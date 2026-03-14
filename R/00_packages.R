# 00_packages.R
# Install and load project packages by profile so bootstrap runs can stay lightweight.

package_profiles <- list(
  base = c(
    "dplyr", "tidyr", "purrr", "stringr", "readr", "readxl", "tibble",
    "janitor", "lubridate", "glue", "yaml"
  ),
  download = c(
    "haven", "survey", "srvyr", "httr2", "jsonlite", "rvest", "xml2", "tidycensus"
  ),
  modeling = c(
    "fixest", "broom", "modelsummary", "clubSandwich", "zoo",
    "DoubleML", "mlr3", "mlr3learners", "ranger", "glmnet",
    "xgboost", "earth", "grf", "sensemakr"
  ),
  optional = c(
    "did", "HonestDiD", "fwildclusterboot", "augsynth", "synthdid"
  )
)

expand_profiles <- function(profiles) {
  profiles <- unique(trimws(profiles))
  profiles <- profiles[nzchar(profiles)]
  if ("full" %in% profiles) {
    profiles <- unique(c("base", "download", "modeling", setdiff(profiles, "full")))
  }

  unknown <- setdiff(profiles, names(package_profiles))
  if (length(unknown) > 0) {
    stop("Unknown package profile(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  unique(unlist(package_profiles[profiles], use.names = FALSE))
}

install_if_missing <- function(pkgs) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  }
}

load_packages <- function(pkgs) {
  for (pkg in pkgs) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }
}

setup_project_packages <- function(
  profiles = c("base", "download"),
  include_optional = FALSE,
  install = TRUE,
  load = TRUE
) {
  pkgs <- expand_profiles(profiles)
  if (isTRUE(include_optional)) {
    pkgs <- unique(c(pkgs, package_profiles$optional))
  }

  if (isTRUE(install)) install_if_missing(pkgs)
  if (isTRUE(load)) load_packages(pkgs)

  options(
    scipen = 999,
    dplyr.summarise.inform = FALSE
  )

  invisible(pkgs)
}

default_profiles <- function() {
  raw <- Sys.getenv("INEQCAUSE_PACKAGE_PROFILE", unset = "base,download")
  strsplit(raw, ",", fixed = TRUE)[[1]]
}

loaded_profiles <- default_profiles()
setup_project_packages(loaded_profiles)

message(
  "Project packages installed/loaded for profiles: ",
  paste(trimws(loaded_profiles), collapse = ", ")
)
