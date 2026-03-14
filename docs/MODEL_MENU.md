# Model menu
## R-only causal model ladder for inequality -> wellbeing / real consumption

This project should use a **model ladder**, not one magic estimator.

The recommended sequence is:

1. transparent fixed effects
2. dynamic specifications
3. double machine learning
4. heterogeneity models
5. sensitivity / falsification

---

## 1) Why the ladder matters

The hard part of this project is **identification**, not prediction.

Machine learning is useful here for:
- flexible adjustment for many controls
- nonlinear nuisance functions
- heterogeneous-effect discovery

Machine learning is **not** a substitute for:
- panel structure
- timing assumptions
- placebo checks
- sensitivity analysis
- careful treatment definition

---

## 2) Recommended model hierarchy

| Level | Model | Main R package(s) | Use in this project | Status |
|---|---|---|---|---|
| 1 | Two-way fixed effects (TWFE) | `fixest` | baseline causal specification | required |
| 2 | Dynamic / local projection FE | `fixest` | timing and persistence | required |
| 3 | Double machine learning, partially linear regression | `DoubleML`, `mlr3`, `mlr3learners` | flexible adjustment for continuous inequality treatment | required robustness |
| 4 | Causal forest | `grf` | effect heterogeneity for large inequality shocks | recommended secondary |
| 5 | Event-study / staggered timing | `fixest::sunab`, `did`, `HonestDiD` | only if you define a discrete inequality shock/adoption event | optional |
| 6 | Synthetic DID / augmented synthetic control | `synthdid`, `augsynth` | case-study module for one or two states | optional |
| 7 | Omitted-variable sensitivity | `sensemakr` | robustness to hidden confounding | required robustness |
| 8 | Wild-cluster bootstrap | `fwildclusterboot` | inference with few clusters | recommended if available |

---

## 3) Primary transparent model

## 3.1 Baseline TWFE

Recommended form:

```r
fixest::feols(
  outcome ~ l1_treatment + l1_log_real_mean_income + l1_rpp_all_items +
    l1_unemployment_rate + l1_hpi_yoy + l1_permits_per_1000 +
    l1_college_share + l1_black_share + l1_hispanic_share |
    state + year,
  data = panel,
  cluster = ~ state
)
```

### Why this is the first model
It gives:
- clear identifying variation
- state fixed effects
- year fixed effects
- interpretable coefficients
- cluster-robust inference

### When to weight
Use:
- **unweighted** if your estimand is the average state effect
- **population-weighted** as robustness if you want the average person-weighted state-year effect

### Recommended outputs
- coefficient table
- confidence intervals
- residual checks
- within-state trend plots

---

## 3.2 Recommended baseline outcomes

### Wellbeing lane
Start with:
- `frequent_mental_distress_rate`
- `fair_poor_health_rate`

### Consumption lane
Start with:
- `log_real_pce_pc`
- `real_pce_pc_growth`

---

## 4) Dynamic models

## 4.1 Local projections

Why use them:
- response can unfold over time
- inequality may not affect health or spending immediately
- easier to interpret than system-wide time-series models in this setting

Recommended horizon set:
- `h = 0, 1, 2, 3`

Recommended structure:
```r
Y_{s,t+h} = beta_h * DeltaIneq_{s,t} + controls_{s,t-1} + alpha_s + lambda_t + error
```

In practice, the script in this pack uses a horizon loop in `fixest`.

### What to look for
- immediate effect vs delayed effect
- persistence
- sign stability across horizons

---

## 4.2 Event-study module

Only use this if you can define a **discrete event**:
- first large inequality shock
- externally induced inequality jump
- one-off treatment timing

Recommended packages:
- `fixest::sunab()`
- `did`
- `HonestDiD`

### Important warning
Do **not** force a continuous-treatment question into a weak event-study design if the shock definition is artificial.

---

## 5) Main causal-ML estimator

## 5.1 Double machine learning (PLR)

Recommended formulation:

```text
Y = theta * D + g(X) + error
D = m(X) + noise
```

Where:
- `Y` = wellbeing or consumption outcome
- `D` = inequality treatment
- `X` = high-dimensional controls and interactions

### Why DML fits this project
- the treatment is continuous
- controls may matter nonlinearly
- there are many plausible interactions
- state-year data are too small for deep learning, but large enough for regularized flexible learners

### How to implement in this pack
1. residualize outcome, treatment, and controls on state and year FE
2. run `DoubleML::DoubleMLPLR`
3. compare results across nuisance learners

### Recommended nuisance learners in R
Use 2–3 learners, not 10.

Best menu:
- `regr.cv_glmnet` — stable, conservative, good default
- `regr.ranger` — flexible random forest
- `regr.earth` — spline/hinge basis, interpretable nonlinearities
- `regr.xgboost` — only shallow trees, because sample size is modest

### Recommended default order
1. `cv_glmnet`
2. `ranger`
3. `earth`
4. `xgboost` as a tougher robustness check

### Why not deep learning
State-year panels are usually on the order of a few hundred observations, not millions. Neural nets are badly matched to this sample size and make interpretation harder.

---

## 5.2 Practical DML advice

### What to put in `X`
Put in:
- real mean income
- price levels
- unemployment
- housing pressure
- permits
- education share
- demographic shares
- minimum wage
- unionization
- nonlinear transforms and interactions if useful

Do **not** put in:
- variables that are almost the same concept as the treatment
- obvious downstream mediators unless it is a dedicated mediation exercise

### Cross-fitting
Use:
- 5 folds as default
- 10 folds only if sample size and runtime allow

### Standardization
Standardize the treatment before DML so that the effect is interpretable as:
- effect of a 1 standard deviation change in inequality

### Inference interpretation
Use DML as a **robustness layer**, not the only inferential basis.  
The fixed-effects model with clustered inference remains the primary transparent benchmark.

---

## 6) Heterogeneous-effect models

## 6.1 Causal forest

Recommended use:
- convert the treatment into a binary **large inequality shock** indicator
- estimate who is more sensitive to that shock

Suggested heterogeneity features:
- baseline income
- baseline hardship
- housing-cost pressure
- unemployment
- college share
- racial/ethnic composition
- minimum wage / unionization
- region indicators

### Recommended R package
- `grf`

### Good outputs
- average treatment effect of a large shock
- distribution of conditional average treatment effects
- rank-ordered heterogeneity plot
- variable-importance table

### Important warning
Variable importance from a forest is **not** proof that a variable is a causal driver.  
Treat it as a guide to where heterogeneity may exist.

---

## 6.2 Interaction-heavy FE models

Before or alongside the forest, also estimate simple interaction models:

```r
fixest::feols(
  outcome ~ l1_treatment * high_housing_pressure +
    l1_treatment * low_income_state +
    controls | state + year,
  data = panel,
  cluster = ~ state
)
```

Why:
- easier to explain
- easier to publish
- easier to compare with forest results

---

## 7) Sensitivity and falsification tools

## 7.1 Omitted-variable sensitivity
Use:
- `sensemakr`

Purpose:
- quantify how strong an unobserved confounder would need to be
- benchmark hidden confounding against observed controls like unemployment or real income

---

## 7.2 Leave-one-state-out
Purpose:
- test whether results are driven by one or two states

Implement:
- re-estimate the main TWFE model dropping one state at a time
- plot the coefficient distribution

---

## 7.3 Placebo leads
Purpose:
- check whether future inequality “predicts” current outcomes

If leads of the treatment are large and significant, your timing story is weaker.

---

## 7.4 Sample splits
Recommended splits:
- exclude DC
- exclude top-inequality states
- pre-2020 only
- post-2020 only
- weighted vs unweighted

---

## 8) Model recommendations by lane

## 8.1 Wellbeing lane

### Best first specification
- treatment: lagged standardized ACS Gini or BEA disposable mean-median gap
- outcome: BRFSS frequent mental distress
- estimator: TWFE
- robustness: DML

### Why
- strongest overlap of clean annual treatment and annual outcome
- BRFSS is large and stable enough for state-year aggregation

---

## 8.2 Real consumption lane

### Best first specification
- treatment: lagged standardized ACS Gini or BEA disposable top share
- outcome: log real PCE per capita
- estimator: TWFE
- robustness: DML + local projections

### Why
- BEA state real PCE is the strongest public state-level real-consumption series
- DML helps protect against nonlinear price/income/housing confounding

---

## 9) Recommended minimal estimator set for the first paper

Use these four:

1. `fixest::feols()` — main baseline
2. local projections via `fixest` — dynamics
3. `DoubleML::DoubleMLPLR` — nonlinear adjustment robustness
4. `sensemakr` + leave-one-state-out — sensitivity

That set is enough for a credible first paper.

---

## 10) Recommended R packages

### Core econometrics
- `fixest`
- `broom`
- `modelsummary`
- `clubSandwich`

### Causal ML
- `DoubleML`
- `mlr3`
- `mlr3learners`
- `ranger`
- `glmnet`
- `earth`
- `xgboost`
- `grf`

### Sensitivity / timing
- `sensemakr`
- `did`
- `HonestDiD`
- `fwildclusterboot` (if installed)

---

## 11) What not to do

Avoid these mistakes:

- using SHAP values as “causal drivers”
- reporting only one fancy ML model and no FE baseline
- using deep nets on a tiny panel
- calling a correlation causal because cross-fitting was used
- over-controlling with poverty/median-based variables that mechanically absorb the inequality concept
- ignoring price-level adjustment when the outcome is “real” wellbeing or consumption

---

## 12) Recommended default settings in this pack

### Main treatment candidates
- `l1_acs_gini_z`
- `l1_disp_mean_median_gap_z`
- `l1_disp_top20_share_z`

### Main outcomes
- `frequent_mental_distress_rate`
- `fair_poor_health_rate`
- `log_real_pce_pc`

### Main controls
- `l1_log_real_mean_income`
- `l1_rpp_all_items`
- `l1_unemployment_rate`
- `l1_hpi_yoy`
- `l1_permits_per_1000`
- demographic lags

### ML default
- DML with `cv_glmnet` and `ranger`
- causal forest only for the binary large-shock heterogeneity exercise
