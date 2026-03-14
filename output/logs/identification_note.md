# Identification Note

Generated: 2026-03-14 09:20 EDT

## Locked causal core
- Main sample: `2012-2019`
- Primary treatment: `l1_disp_top10_share_z`
- Primary outcomes:
  - `frequent_mental_distress_rate`
  - `fair_poor_health_rate`
- Main estimators:
  - dynamic FE with lagged outcome, state FE, and division x year FE
  - long-difference companion models
- Core controls: lagged income, prices, unemployment, housing pressure/supply, and demographic composition
- Rich controls: add labor-institution and QCEW structure variables as robustness only

## What the core assumptions are
- After state FE, division x year FE, lagged outcomes, and lagged core controls, there are no remaining state-level shocks that jointly move inequality and wellbeing strongly enough to explain the estimates.
- Lagged inequality is informative about subsequent wellbeing rather than mostly reflecting reverse causality from worsening health into earnings and measured inequality.
- Long differences reduce slow-moving confounding rather than merely adding noise.
- The BRFSS state-year aggregates are measured well enough for panel inference.

## What each robustness branch is testing
- `wellbeing_cce_dynamic_fe.csv`: latent common trends / low-rank omitted factors.
- `wellbeing_subgroup_dynamic_fe.csv`: reverse-causality stress test using `age_65_plus` and `retired`.
- `wellbeing_measurement_sensitivity.csv`: whether BRFSS weighting and fixed age composition materially change the result.
- `wellbeing_falsification_dynamic_fe.csv`: whether the preferred treatment also predicts nearby non-headline BRFSS outcomes.
- `wellbeing_hard_outcome_falsification.csv`: whether harder external outcomes move in a way that supports the same wellbeing story.
- `extended_outcomes_dynamic_fe.csv`: secondary wellbeing and material-status extensions, not the causal core.

## Stronger inference read
- The locked final table is `output/tables/final_main_wellbeing_table.csv`.
- It adds CR2 / Satterthwaite small-cluster-robust inference on top of the standard clustered FE output.
- Under that stronger inference:
  - dynamic FE is null throughout
  - the 2-year long-difference mental-distress result remains negative, but `cr2_p = 0.379` in baseline and `0.435` in rich
  - fair/poor-health is null throughout
- This pushes the backend further toward a disciplined observational null/mixed read rather than a causal positive finding.

## What should not be claimed
- Do not claim the project identifies the causal effect of inequality on wellbeing.
- Do not claim DML, event study, causal forest, or local projections provide the main causal evidence.
- Do not claim the mixed suicide associations validate the preferred top-share treatment; they appear in ACS Gini / mean-median-gap models, not the locked top-share estimand.
- Do not present measurement-sensitive fair/poor-health results as solving reverse causality.

## What can be claimed
- The repo provides a reproducible state-year observational design with substantial timing, measurement, subgroup, latent-factor, and hard-outcome stress tests.
- Apparent inequality-wellbeing associations weaken materially as the design becomes more defensible.
- The preferred top-share wellbeing estimand is null to mixed under the strictest backend read.
