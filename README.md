# wellgap

`wellgap` is a reproducible R pipeline that asks whether a rise in a state's top-10% share of disposable income, holding average income and prices roughly constant, lowers self-reported wellbeing. It builds a state-year panel for the 50 U.S. states and DC and estimates observational models on a fixed 2012–2019 sample. The model output tables are committed, so you can inspect every result without rebuilding the data.

Under dynamic fixed-effects and long-difference models with small-sample-corrected standard errors, the estimates for frequent mental distress and fair or poor self-rated health are statistically null with both the baseline and the richer control set. Associations seen in simpler fixed-effects specifications do not survive these models. The intervals are wide, so the results do not rule out small or slow-acting effects, and they are not causal estimates.

## Data and sample

The main sample is 2012–2019. The year 2020 is excluded because standard ACS 1-year data were not released in their usual form for that year and because pandemic disruptions break the annual series. The wider panel (2012–2019 and 2021–2024) is used only for extension and exploratory models.

| Source | Role |
|---|---|
| American Community Survey (ACS) 1-year estimates | Demographics, unemployment, Gini coefficient |
| Bureau of Economic Analysis (BEA) distribution of personal income | Top-10% disposable income share (treatment) |
| Behavioral Risk Factor Surveillance System (BRFSS) | Self-reported health and mental distress (outcomes) |
| BEA regional accounts | Real consumption, income, and regional price controls |
| FHFA House Price Index, Census building permits | Housing controls |
| BLS QCEW, BLS union membership, DOL state minimum wage history | Labor-market and institutional controls |
| CDC WONDER | Mortality outcomes for falsification checks |

## Method

The treatment is the lagged, standardized top-10% disposable income share. The two primary outcomes, both from BRFSS, are the frequent mental distress rate (share of adults reporting 14 or more poor mental health days in the past 30) and the fair or poor self-rated health rate.

The primary model is a dynamic fixed-effects (FE) regression with state and Census-division-by-year fixed effects, a lagged outcome, and lagged controls. Companion 2- and 3-year long-difference models reduce sensitivity to slow-moving confounders at the cost of sample size. Baseline controls are lagged income, regional prices, unemployment, house price growth, and housing supply; the rich set adds minimum wage, union membership, and QCEW wage and industry structure. Inference uses CR2 (bias-reduced linearization) standard errors with Satterthwaite degrees of freedom, which suit about 50 clusters better than ordinary clustered errors; the final tables keep the conventional values in `cluster_*` columns.

## Results

Effect of a one-standard-deviation increase in the lagged top-10% share, in percentage points, with the CR2 p-value in parentheses (from `output/tables/final_main_wellbeing_table.csv`):

| Model | Frequent mental distress | Fair/poor health |
|---|---|---|
| Dynamic FE, baseline controls | +0.27 (0.99) | +1.34 (0.96) |
| Dynamic FE, rich controls | +0.19 (0.99) | +0.61 (0.99) |
| 2-year long difference, baseline | −0.55 (0.67) | +0.98 (0.60) |
| 3-year long difference, baseline | −0.57 (0.75) | +0.74 (0.72) |

The robustness checks, collected in `output/tables/final_robustness_wellbeing_table.csv`, point the same way:

| Check | Result for the primary treatment |
|---|---|
| Minimal, core, and rich control sets | Null |
| State linear trends instead of a lagged outcome | Null |
| 3-year moving-average treatment; distributed lags | Null |
| Wild-cluster bootstrap | Null |
| Collapsed difference GMM (system GMM fails overidentification tests) | Not significant |
| Correlated common effects (latent factors) | Null |
| Adults 65+ and retirees only (reverse causality) | Null |
| Age-standardized outcomes | Null |
| Precision-weighted outcomes (conventional clustered errors; CR2 unstable) | Fair/poor health p = 0.034; mental distress not significant |
| Physical distress outcomes (falsification) | Null |
| Suicide and drug poisoning mortality (falsification) | Null; Gini and mean-median measures show some suicide associations |
| Employment, real median household income | Null; Gini and mean-median measures show some income associations |

Event-study, double machine learning, causal forest, and local projection modules are exploratory and not part of the headline result.

## Limitations

- The design is observational. Fixed effects, lagged outcomes, and controls reduce confounding but do not remove it; the main models assume no remaining state-level shocks drive both inequality and wellbeing.
- With about 50 states and 6–8 usable years after lagging, power is low and results depend on the standard-error method. Moderate effects in either direction remain consistent with the data.
- BRFSS state-year rates are survey estimates, and measurement error in treatment or outcome biases estimates toward zero.
- The top-10% share is one inequality measure among several. Gini and mean-median gap results sometimes differ.
- Lagged FHFA house price growth is non-missing for 459 of 612 panel rows, below the 500-row coverage threshold; this is the one validation warning.
- The results say nothing about individual or neighborhood inequality, long-run mechanisms such as social trust or public goods, or the pandemic period.

## Reproduce

Requires R 4.x. Install the packages listed in `R/00_packages.R`:

```bash
Rscript -e 'source("R/00_packages.R"); setup_project_packages("full")'
```

On a fresh clone, with no data, check the configuration, script syntax, and committed output tables:

```bash
Rscript R/98_validate_project.R --mode=clone
```

Rebuilding from source needs `CENSUS_API_KEY` and `BEA_API_KEY` in `~/.Renviron`. The DOL minimum-wage table and the CDC WONDER mortality exports are fetched through a browser with a Playwright CLI wrapper: set `PLAYWRIGHT_WRAPPER` to its executable path. The CDC WONDER script also needs `npx` and `jq`; without its exports the mortality checks are skipped.

```bash
scripts/fetch_cdc_wonder_mortality.sh   # optional: mortality exports into data/raw/cdc_wonder/
Rscript R/99_run_all.R                  # download, build the panel, run all models and final tables
Rscript R/98_validate_project.R         # full validation of data, panel, and outputs
```

## Repository layout

```
R/                  Numbered pipeline scripts: 10–15 download, 20 panel, 30–37 models and final tables,
                    98 validation, 99 full run
scripts/            CDC WONDER export script
config/config.yml   Years, sources, outcomes, and control sets
output/tables/      Committed model output tables (CSV); output/README.md describes each file
data/               Raw, intermediate, and final panel data (created by the pipeline, not committed)
```

## License

MIT. See [LICENSE](LICENSE). The input data are public U.S. government data.
