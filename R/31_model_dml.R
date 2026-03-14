# 31_model_dml.R
# Double machine learning robustness for continuous inequality treatments.

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

required_pkgs <- c("dplyr", "purrr", "tibble", "tidyr", "fixest")
invisible(lapply(required_pkgs, require, character.only = TRUE))

dml_backend <- function() {
  if (
    requireNamespace("DoubleML", quietly = TRUE) &&
    requireNamespace("mlr3", quietly = TRUE) &&
    requireNamespace("mlr3learners", quietly = TRUE)
  ) {
    return("DoubleML")
  }

  if (requireNamespace("ddml", quietly = TRUE)) {
    return("ddml")
  }

  stop(
    "No supported DML backend is installed. Install either DoubleML + mlr3 + mlr3learners or ddml.",
    call. = FALSE
  )
}

read_panel <- function() {
  panel_path <- path_project(cfg$paths$final_root, "state_year_panel.rds")
  if (!file.exists(panel_path)) stop("Missing final panel: ", panel_path, call. = FALSE)
  safe_read_rds(panel_path)
}

make_dml_learners <- function() {
  learners <- list(
    glmnet = mlr3::lrn("regr.cv_glmnet"),
    ranger = mlr3::lrn("regr.ranger", num.trees = 1000, min.node.size = 5),
    xgboost = mlr3::lrn(
      "regr.xgboost",
      nrounds = 250L,
      max_depth = 3L,
      eta = 0.05,
      subsample = 0.8,
      colsample_bytree = 0.8
    )
  )

  if ("regr.earth" %in% mlr3::mlr_learners$keys()) {
    learners$earth <- mlr3::lrn("regr.earth")
  }

  learners
}

make_ddml_learner <- function(learner) {
  switch(
    learner,
    glmnet = list(what = ddml::mdl_glmnet),
    ranger = list(what = ddml::mdl_ranger, args = list(num.trees = 1000)),
    xgboost = list(
      what = ddml::mdl_xgboost,
      args = list(
        nrounds = 250L,
        max_depth = 3L,
        learning_rate = 0.05,
        subsample = 0.8,
        colsample_bytree = 0.8
      )
    ),
    ols = list(what = ddml::ols),
    stop("Unknown ddml learner: ", learner, call. = FALSE)
  )
}

residualize_for_dml <- function(data, outcome, treatment, x_vars, fe = c("state", "year")) {
  x_vars <- x_vars[x_vars %in% names(data)]
  keep <- unique(c(outcome, treatment, x_vars, fe))
  dat <- data |>
    dplyr::select(dplyr::all_of(keep)) |>
    tidyr::drop_na(dplyr::all_of(c(outcome, treatment)))

  out <- dat
  for (v in c(outcome, treatment, x_vars)) {
    fml <- as.formula(sprintf("%s ~ 1 | %s", v, paste(fe, collapse = " + ")))
    dat_fe <- dat |>
      dplyr::select(dplyr::all_of(unique(c(v, fe))))
    keep_rows <- stats::complete.cases(dat_fe)
    mod <- fixest::feols(fml, data = dat_fe, warn = FALSE)
    resid <- rep(NA_real_, nrow(dat))
    resid[keep_rows] <- stats::residuals(mod)
    out[[v]] <- resid
  }

  out
}

run_dml_plr <- function(
  data,
  outcome,
  treatment,
  x_vars = analysis_primary_controls(),
  learner = "glmnet",
  n_folds = 5,
  n_rep = 1,
  seed = cfg$project$seed %||% 123
) {
  backend <- dml_backend()
  if (identical(backend, "ddml")) {
    return(
      run_dml_plr_ddml(
        data = data,
        outcome = outcome,
        treatment = treatment,
        x_vars = x_vars,
        learner = learner,
        n_folds = n_folds,
        seed = seed
      )
    )
  }

  x_vars <- x_vars[x_vars %in% names(data)]
  if (length(x_vars) == 0) stop("No x_vars found in data for DML.", call. = FALSE)

  dat <- residualize_for_dml(data, outcome = outcome, treatment = treatment, x_vars = x_vars)
  dat <- dat |> tidyr::drop_na()
  dat_dt <- data.table::as.data.table(dat)

  learners <- make_dml_learners()
  if (!(learner %in% names(learners))) stop("Unknown learner: ", learner, call. = FALSE)

  set.seed(seed)
  ml_y <- learners[[learner]]$clone()
  ml_d <- learners[[learner]]$clone()

  dml_data <- DoubleML::DoubleMLData$new(
    data = dat_dt,
    y_col = outcome,
    d_cols = treatment,
    x_cols = x_vars
  )

  dml_obj <- DoubleML::DoubleMLPLR$new(
    data = dml_data,
    ml_l = ml_y,
    ml_m = ml_d,
    n_folds = n_folds,
    n_rep = n_rep
  )
  dml_obj$fit()

  tibble::tibble(
    outcome = outcome,
    treatment = treatment,
    learner = learner,
    backend = backend,
    theta = as.numeric(dml_obj$coef),
    se = as.numeric(dml_obj$se),
    t_stat = as.numeric(dml_obj$t_stat),
    p_value = as.numeric(dml_obj$pval),
    n = nrow(dat)
  )
}

run_dml_plr_ddml <- function(
  data,
  outcome,
  treatment,
  x_vars = analysis_primary_controls(),
  learner = "glmnet",
  n_folds = 5,
  seed = cfg$project$seed %||% 123
) {
  x_vars <- x_vars[x_vars %in% names(data)]
  if (length(x_vars) == 0) stop("No x_vars found in data for DML.", call. = FALSE)

  dat <- residualize_for_dml(data, outcome = outcome, treatment = treatment, x_vars = x_vars) |>
    tidyr::drop_na()

  spec <- make_ddml_learner(learner)

  set.seed(seed)
  fit <- ddml::ddml_plm(
    y = dat[[outcome]],
    D = dat[[treatment]],
    X = as.matrix(dat[x_vars]),
    learners = spec,
    sample_folds = n_folds,
    silent = TRUE
  )

  coef_value <- as.numeric(fit$coef[[1]])
  coef_table <- summary(fit)[, , 1, drop = FALSE][, , 1]
  term_name <- intersect(c("D_r", treatment), rownames(coef_table))[1]
  if (is.na(term_name)) stop("Could not find DML treatment term in ddml OLS summary.", call. = FALSE)

  tibble::tibble(
    outcome = outcome,
    treatment = treatment,
    learner = learner,
    backend = "ddml",
    theta = coef_value,
    se = as.numeric(coef_table[term_name, "Std. Error"]),
    t_stat = as.numeric(coef_table[term_name, "t value"]),
    p_value = as.numeric(coef_table[term_name, "Pr(>|t|)"]),
    n = nrow(dat)
  )
}

benchmark_dml <- function(
  data,
  outcome,
  treatment,
  x_vars = analysis_primary_controls(),
  learners = NULL
) {
  if (is.null(learners)) {
    learners <- if (identical(dml_backend(), "DoubleML")) c("glmnet", "ranger", "xgboost") else c("glmnet", "ranger", "xgboost")
  }
  purrr::map_dfr(learners, function(lrn) {
    run_dml_plr(data, outcome = outcome, treatment = treatment, x_vars = x_vars, learner = lrn)
  })
}

run_primary_dml <- function(panel = read_panel()) {
  x_specs <- list(
    baseline = analysis_primary_controls(),
    rich = analysis_rich_controls()
  )
  x_specs <- x_specs[!duplicated(vapply(x_specs, paste, character(1), collapse = "|"))]
  treatments <- cfg_vec(cfg$analysis$primary_treatments)
  outcomes <- c(cfg_vec(cfg$analysis$primary_wellbeing_outcomes), cfg_vec(cfg$analysis$primary_consumption_outcomes))
  treatments <- treatments[treatments %in% names(panel)]
  outcomes <- outcomes[outcomes %in% names(panel)]

  out <- purrr::map_dfr(names(x_specs), function(spec_name) {
    x_vars <- x_specs[[spec_name]]

    purrr::map_dfr(treatments, function(tr) {
      purrr::map_dfr(outcomes, function(y) {
        benchmark_dml(panel, outcome = y, treatment = tr, x_vars = x_vars) |>
          dplyr::mutate(
            spec = spec_name,
            x_var_count = length(intersect(x_vars, names(panel)))
          )
      })
    })
  })

  safe_write_csv(out, path_project(cfg$paths$tables_root, "dml_results.csv"))
  invisible(out)
}

if (sys.nframe() == 0) {
  panel <- read_panel()
  run_primary_dml(panel)
}
