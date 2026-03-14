# wellgap

Does rising income inequality affect population wellbeing? This project builds a state-year panel for the United States and applies a series of increasingly strict observational models to find out. The short answer: apparent associations between inequality and wellbeing weaken as the statistical design becomes more defensible. Under the strictest inference, the main results are null to mixed.

## Question

Holding average income and price levels roughly constant, do increases in top-end income concentration reduce self-reported wellbeing at the state level?

## Data and sample

The analysis panel covers all 50 states plus DC. The main estimation sample is **2012--2019**. The year 2020 is omitted from the main design because the standard ACS 1-year data path is unavailable in its usual form for that year and because pandemic-period disruptions make that year difficult to treat as part of the same smooth annual design. The broader panel (2012--2019, 2021--2024) is available for extension and robustness work.

**Primary data sources:**

| Source | Role |
|---|---|
| ACS 1-year estimates | Demographics and baseline inequality (Gini) |
| BEA Personal Income Distribution | Top-income-share treatment variable |
| BRFSS annual state estimates | Self-reported health and mental distress outcomes |
| BEA Regional Accounts | Real consumption, income, and price controls |
| FHFA HPI, Census Building Permits | Housing market controls |
| BLS LAUS, QCEW, union membership data | Labor market and institutional controls |
| CDC WONDER | Mortality outcomes for hard-outcome checks |

## What was tested

**Treatment:** Lagged top-10% disposable income share (standardized), constructed from BEA state personal income distribution data.

**Primary outcomes:**

- **Frequent mental distress rate** -- share of adults reporting 14 or more days of poor mental health in the past 30 days (BRFSS).
- **Fair or poor self-rated health rate** -- share of adults rating their own health as fair or poor (BRFSS).

**Controls:** All models include state fixed effects and Census-division-by-year fixed effects. The baseline control set adds lagged income, regional prices, unemployment, housing price growth, and housing supply. A richer control set adds minimum wage, union membership, and QCEW wage and industry-structure variables.

## Models

Two main model families carry the headline results:

1. **Dynamic fixed effects** -- state and division-by-year fixed effects, a lagged dependent variable, and lagged controls. This is the primary specification.
2. **Long-difference companion models** -- 2-year and 3-year differenced specifications that reduce sensitivity to slow-moving confounders at the cost of a smaller effective sample.

Statistical inference uses CR2 (bias-reduced linearization) standard errors with Satterthwaite degrees of freedom, which are more appropriate than ordinary clustered standard errors when the number of clusters is moderate.

<details>
<summary>Robustness and stress-test branches</summary>

The project runs several additional checks beyond the main models:

- **Correlated common effects (CCE) dynamic FE** -- adds latent common factors to guard against low-rank omitted trends.
- **Subgroup reverse-causality checks** -- restricts to adults 65+ and retirees, whose health is less likely to feed back into current earnings and measured inequality.
- **Measurement sensitivity** -- re-runs the main specification using age-standardized outcome rates and precision-weighted estimation.
- **Within-BRFSS falsification** -- tests whether the treatment also predicts frequent physical distress and mean bad physical health days (outcomes that should be less responsive to income distribution than mental distress).
- **Hard-outcome falsification** -- tests associations with state-level age-adjusted suicide and drug poisoning mortality rates (CDC WONDER).
- **Extension outcomes** -- examines employment-to-population ratio, price-adjusted median household income, and secondary wellbeing measures under the same framework.
- **Alternative inequality measures** -- Gini coefficient and mean-median income gap are tested alongside the primary top-share measure.

</details>

## Results

### Main findings

Under the strictest available inference (CR2 standard errors), the dynamic fixed-effects models show **no statistically significant association** between lagged top-10% income share and either frequent mental distress or fair/poor self-rated health. This holds for both the baseline and richer control sets.

The long-difference models produce a negative point estimate for mental distress (suggesting higher top-share concentration is associated with lower distress, directionally the opposite of the expected sign), but this estimate is imprecise and not significant under CR2 inference (p = 0.38 in the baseline specification, p = 0.44 in the richer specification). Fair/poor health is null in the long-difference models as well.

### Robustness summary

The robustness branches reinforce the null main read:

- **CCE models:** Null across both outcomes and both control sets.
- **Subgroup checks:** Null for both the 65+ and retiree subsamples.
- **Measurement sensitivity:** Age-standardized outcomes are null. Precision-weighted fair/poor health reaches marginal significance under ordinary clustered inference in some specifications, but this is a less conservative inference standard than the CR2 results reported in the main table.
- **Falsification outcomes:** The primary treatment does not significantly predict frequent physical distress or mean bad physical health days.
- **Hard outcomes:** The primary top-share treatment shows no significant association with suicide or drug poisoning mortality. Some associations appear under alternative inequality measures (Gini, mean-median gap) for suicide, but these are not the locked primary estimand.
- **Extension outcomes:** Mostly null for the primary treatment. Some associations between the mean-median income gap and price-adjusted median income reach significance, but these are a different treatment variable.

<details>
<summary>Key numbers from the main table</summary>

All estimates are for the effect of a one-standard-deviation increase in lagged top-10% disposable income share.

**Dynamic FE, baseline controls:**

| Outcome | Estimate | CR2 p-value |
|---|---|---|
| Frequent mental distress rate | +0.27 pp | 0.99 |
| Fair/poor health rate | +1.34 pp | 0.96 |

**Dynamic FE, rich controls:**

| Outcome | Estimate | CR2 p-value |
|---|---|---|
| Frequent mental distress rate | +0.19 pp | 0.99 |
| Fair/poor health rate | +0.61 pp | 0.99 |

**Long-difference (2-year), baseline controls:**

| Outcome | Estimate | CR2 p-value |
|---|---|---|
| Frequent mental distress rate | -1.32 pp | 0.38 |

**Long-difference (3-year), baseline controls:**

| Outcome | Estimate | CR2 p-value |
|---|---|---|
| Fair/poor health rate | +0.15 pp | 0.92 |

</details>

## What this does and does not show

**What it shows:**

- Apparent associations between state-level income inequality and self-reported wellbeing weaken substantially when the design includes state and division-by-year fixed effects, lagged controls, dynamic specifications, and small-sample-corrected inference.
- Null results under a defensible design are informative. They indicate that any population-level effect of top-income concentration on these self-reported health measures is either small, slow-acting, or operating through channels that are absorbed by the controls.
- The project provides a reproducible panel with extensive stress testing. The validation suite passes with one warning (coverage of lagged housing price growth).

**What it does not show:**

- This project does not prove that income inequality has no effect on wellbeing. The estimates are imprecise enough that moderate effects in either direction remain consistent with the data.
- It does not identify a causal effect. The design is observational. State fixed effects and time-varying controls reduce but do not eliminate confounding.
- It does not speak to individual-level or neighborhood-level inequality effects, which may differ from state-level patterns.
- It does not address mechanisms like relative deprivation, social trust, or public goods provision, which could operate on longer time horizons or through channels not captured here.

## Assumptions and limitations

The main models rest on a conditional parallel-trends assumption: after state fixed effects, division-by-year fixed effects, a lagged dependent variable, and lagged controls, there are no remaining state-level shocks that jointly drive both inequality and wellbeing strongly enough to explain the estimates.

<details>
<summary>Additional assumptions and known limitations</summary>

- **Reverse causality.** The lagged treatment variable and subgroup checks (65+, retirees) partially address concerns that poor health feeds back into earnings and measured inequality, but cannot rule this out entirely.
- **Measurement.** BRFSS state-year estimates are survey aggregates, not administrative records. Measurement error in both treatment and outcome variables will tend to attenuate estimates toward zero.
- **Sample size.** With roughly 50 states and 6-8 usable time periods (after lagging), the effective sample is small by panel-data standards. This limits statistical power and makes inference sensitive to the choice of standard error estimator, which is why the project uses CR2 / Satterthwaite corrections.
- **Treatment definition.** The BEA top-10% disposable income share is one of several possible inequality measures. Results using alternative measures (Gini, mean-median gap) sometimes differ, which suggests the choice of inequality metric matters.
- **Coverage.** The lagged FHFA housing price index has a coverage warning (459 of 612 panel observations non-missing vs. a threshold of 500). All other variables meet their coverage thresholds.
- **Omitted period.** Excluding 2020 improves internal comparability but means the analysis does not speak to pandemic-period inequality dynamics.

</details>

## Bottom line

This project started with the question of whether rising income inequality reduces population wellbeing. After building a state-year panel, applying dynamic fixed-effects and long-difference models, and running extensive robustness checks, the answer from the data is: the relationship is not detectable under a disciplined observational design. Earlier, less strict specifications produced suggestive associations, but those associations do not survive stronger controls and more conservative inference. The main contribution is showing that apparent inequality-wellbeing relationships weaken as the design becomes more defensible.

---

## Repo structure

```
R/                       Analysis scripts (numbered by stage)
  00_packages.R          Package loading
  01_helpers.R           Shared utility functions
  10-15_download_*.R     Data acquisition
  20_build_panel.R       Panel construction
  30_model_fe.R          Fixed-effects models
  31_model_dml.R         Double machine learning
  32_model_event_study.R Event study
  33_model_grf.R         Causal forest
  34_sensitivity.R       Sensitivity analysis
  35_causal_robustness.R Robustness branches (CCE, subgroup, measurement, falsification)
  36_extended_outcomes.R Extension outcomes
  37_final_tables.R      Locked final tables with CR2 inference
  98_validate_project.R  End-to-end validation
  99_run_all.R           Full pipeline orchestrator
scripts/                 Shell scripts (CDC WONDER fetch)
config/config.yml        Project configuration
data/
  raw/                   Original downloaded files
  intermediate/          Cleaned single-source intermediates
  final/                 Integrated panel (state_year_panel.rds)
output/
  tables/                All model output tables (CSV)
  logs/                  Identification notes, validation report, results memos
docs/                    Scope, data source, and design documentation
```

## Reproduction

<details>
<summary>Setup and build instructions</summary>

**Requirements:** R (tested on 4.x) with the packages listed in `R/00_packages.R`. Key dependencies include `fixest`, `clubSandwich`, `DoubleML`, `grf`, `survey`, and `tidycensus`.

**API keys** (set in `~/.Renviron`):

```
CENSUS_API_KEY=your_key
BEA_API_KEY=your_key
```

**Run the full pipeline:**

```bash
Rscript R/99_run_all.R
```

**Or run stage by stage:**

```bash
Rscript -e 'source("R/00_packages.R")'
Rscript -e 'source("R/10_download_acs.R"); download_acs_state_panel()'
Rscript -e 'source("R/12_download_brfss.R"); build_brfss_panel()'
Rscript -e 'source("R/20_build_panel.R"); build_state_year_panel()'
Rscript -e 'source("R/30_model_fe.R"); panel <- read_panel(); run_primary_fe_models(panel)'
Rscript R/98_validate_project.R
```

If the intermediate data files already exist under `data/intermediate/`, the panel can be rebuilt without live API calls.

**Validate:**

```bash
Rscript R/98_validate_project.R
```

Current validation: 77 pass, 1 warning (lagged HPI coverage), 0 failures.

</details>

## License

This project uses publicly available U.S. government data. See `docs/DATA_SOURCES.md` for source-by-source access details.
