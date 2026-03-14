# 33_model_grf.R
# Causal forest heterogeneity module for a binary large-inequality-shock treatment.

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

required_pkgs <- c("dplyr", "purrr", "tibble", "tidyr", "grf")
invisible(lapply(required_pkgs, require, character.only = TRUE))

read_panel <- function() {
  panel_path <- path_project(cfg$paths$final_root, "state_year_panel.rds")
  if (!file.exists(panel_path)) stop("Missing final panel: ", panel_path, call. = FALSE)
  safe_read_rds(panel_path)
}

run_causal_forest_shock <- function(
  data,
  outcome,
  treatment = "large_acs_gini_shock",
  x_vars = c(
    "l1_log_real_mean_income", "l1_rpp_all_items", "l1_unemployment_rate",
    "l1_hpi_yoy", "l1_permits_per_1000", "l1_college_share",
    "l1_black_share", "l1_hispanic_share", "l1_state_min_wage_nominal",
    "l1_union_membership_rate", "l1_qcew_private_avg_wkly_wage",
    "l1_qcew_manufacturing_share", "l1_qcew_professional_business_share"
  )
) {
  x_vars <- x_vars[x_vars %in% names(data)]
  if (length(x_vars) == 0) stop("No x_vars found in data for causal forest.", call. = FALSE)
  keep <- unique(c(outcome, treatment, x_vars))
  dat <- data |>
    dplyr::select(dplyr::all_of(keep)) |>
    tidyr::drop_na()

  if (!all(dat[[treatment]] %in% c(0, 1))) {
    stop("Treatment for causal forest must be binary 0/1.", call. = FALSE)
  }

  X <- as.matrix(dat[x_vars])
  Y <- dat[[outcome]]
  W <- dat[[treatment]]

  y_forest <- grf::regression_forest(X, Y, num.trees = 2000)
  w_forest <- grf::regression_forest(X, W, num.trees = 2000)

  cf <- grf::causal_forest(
    X = X,
    Y = Y,
    W = W,
    Y.hat = predict(y_forest)$predictions,
    W.hat = predict(w_forest)$predictions,
    num.trees = 4000
  )

  ate <- grf::average_treatment_effect(cf)
  tau <- predict(cf)$predictions
  importance <- tibble::tibble(
    feature = x_vars,
    importance = as.numeric(grf::variable_importance(cf))
  ) |>
    dplyr::arrange(dplyr::desc(importance))
  ate_estimate <- if (!is.null(names(ate)) && "estimate" %in% names(ate)) ate[["estimate"]] else ate[[1]]
  ate_se <- if (!is.null(names(ate)) && "std.err" %in% names(ate)) ate[["std.err"]] else ate[[2]]

  list(
    forest = cf,
    ate = tibble::tibble(
      outcome = outcome,
      treatment = treatment,
      ate = unname(ate_estimate),
      se = unname(ate_se)
    ),
    cates = tibble::tibble(row_id = seq_along(tau), tau_hat = tau),
    importance = importance
  )
}

if (sys.nframe() == 0) {
  panel <- read_panel()
  shock_var <- if ("large_disp_gap_shock" %in% names(panel)) "large_disp_gap_shock" else "large_acs_gini_shock"
  out <- run_causal_forest_shock(panel, outcome = cfg$analysis$primary_wellbeing_outcomes[[1]], treatment = shock_var)
  safe_write_csv(out$ate, path_project(cfg$paths$tables_root, "causal_forest_ate.csv"))
  safe_write_csv(out$importance, path_project(cfg$paths$tables_root, "causal_forest_importance.csv"))
}
