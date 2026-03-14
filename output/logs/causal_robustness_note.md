# Causal Robustness Note

Generated: 2026-03-14 07:55 EDT

## What changed
- Time logic was hardened so lags, leads, differences, local-projection horizons, and derived growth rates require exact calendar adjacency.
- Because 2020 is omitted from the main panel, 2021 no longer inherits a fake one-year lag from 2019.
- A wellbeing-focused causal module now uses:
  - main causal sample `2012-2019`
  - dynamic FE with lagged outcome and `state + division x year` fixed effects
  - 2-year long differences for `frequent_mental_distress_rate`
  - 3-year long differences for `fair_poor_health_rate`
  - reverse-causality subgroup checks for `age_65_plus` and `retired`
- BRFSS state-year outcomes now also carry:
  - survey-design standard errors
  - inverse-variance precision weights
  - fixed 3-band age-standardized wellbeing outcomes
- Additional backend sensitivity branches now include:
  - common-correlated-effects latent-factor sensitivity
  - BRFSS measurement sensitivity using precision weighting and age standardization
  - a small within-BRFSS falsification layer using physical-distress outcomes
  - external hard-outcome checks using CDC WONDER suicide and drug-poisoning mortality
- Primary wellbeing treatments now prioritize `l1_disp_top10_share_z`, with `l1_acs_gini_z` secondary and `l1_disp_mean_median_gap_z` retained as a stress-test appendix treatment.

## Main read
- The stricter dynamic FE models are null across all wellbeing outcomes and treatments.
- The only near-threshold signal is in the 2-year long-difference models for `frequent_mental_distress_rate` using the BEA top-share treatment:
  - baseline: estimate about `-1.43`, `p = 0.053`, `n = 203`
  - rich: estimate about `-1.38`, `p = 0.096`, `n = 202`
- The 3-year fair/poor-health long-difference results are null.
- The 65+/retired subgroup checks are null, which weakens the case for a strong causal wellbeing claim and does not provide supportive evidence against reverse causality.
- The common-correlated-effects / latent-factor sensitivity is null across the main wellbeing outcomes.
- The within-BRFSS falsification outcomes are null to weak.
- The external hard-outcome layer is mixed:
  - baseline dynamic FE shows positive suicide-rate associations for `l1_acs_gini_z` (`p = 0.034`) and `l1_disp_mean_median_gap_z` (`p = 0.034`)
  - those suicide associations attenuate in the rich spec
  - `l1_disp_top10_share_z` is null for both suicide and drug-poisoning mortality
  - drug-poisoning mortality is null throughout
- The only newly non-null branch is the BRFSS measurement-sensitivity layer:
  - precision-weighted dynamic FE gives positive `fair_poor_health_rate` estimates for `l1_disp_mean_median_gap_z` and `l1_disp_top10_share_z` in the baseline spec
  - those same signals persist in the age-standardized precision-weighted variant
  - those results sharpen measurement, but they do not resolve reverse causality or omitted-variable bias by themselves

## Defensibility implication
- The repo now supports a much narrower claim:
  - disciplined observational evidence with conservative timing and reverse-causality stress tests
- The repo does not currently support a strong causal claim for mental distress or fair/poor health.
- The hard-outcome extension does not materially strengthen the preferred top-share wellbeing design because its signal appears in ACS Gini / mean-median-gap suicide models rather than in the preferred top-share treatment.
- Event study, causal forest, local projections, and DML should be treated as secondary diagnostics or descriptive robustness, not as the causal core.
- Precision weighting and age standardization improve measurement quality, not identification.
