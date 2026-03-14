# Post-QCEW Interpretation Memo

Generated: 2026-03-13 17:58 EDT

## Executive read

- The project is now technically complete for the current state-year design: validation passes 44/44 checks and the panel is 612 rows x 119 columns.
- The most consistent signal remains the BEA disposable-income mean-median gap, not ACS Gini.
- The strongest FE result is a negative association between `l1_disp_mean_median_gap_z` and `log_real_pce_pc`, especially in the weighted FE spec.
- Wellbeing results are weaker and more model-dependent. DML finds a negative association between the BEA gap and frequent mental distress, but FE does not reproduce a strong wellbeing effect.
- The event-study branch is not strong causal confirmation at this stage because it shows meaningful pre-trends.

## Baseline decision

- Keep QCEW variables in `rich`, not `baseline`.
- Reason 1: QCEW industry mix and wages are conceptually close to labor-market structure that may partly mediate inequality rather than purely confound it.
- Reason 2: the narrow baseline remains easier to explain and compare across models.
- Reason 3: the rich spec is still valuable because it asks whether the main inequality signal survives after conditioning on labor-market composition.

## Main findings

### FE

- Strongest FE result: weighted `log_real_pce_pc` on `l1_disp_mean_median_gap_z`
  - estimate about `-0.0087`
  - `p = 0.004`
  - `n = 510`
- The same BEA-gap result weakens in the richer QCEW spec:
  - rich `log_real_pce_pc` estimate about `-0.0053`
  - `p = 0.089`
  - `n = 509`
- Interpretation: part of the cross-state association may run through labor-market composition, but the sign remains negative.
- ACS Gini FE results remain mostly null in the contemporaneous TWFE table.

### Local projections

- LP results show scattered significance rather than a clean pattern.
- Notable examples:
  - weighted `fair_poor_health_rate` on `l1_acs_gini_z` at horizon 2: estimate about `-0.0043`, `p = 0.021`
  - rich `log_real_pce_pc` on `l1_acs_gini_z` at horizon 1: estimate about `0.0066`, `p = 0.048`
- Interpretation: dynamic results are not internally consistent enough to treat as the core finding.

### DML

- Strongest DML result: baseline `frequent_mental_distress_rate` on `l1_disp_mean_median_gap_z` with `xgboost`
  - theta about `-0.0025`
  - `p = 0.032`
  - `n = 505`
- The prior ACS Gini wellbeing signal is weaker after the QCEW refresh:
  - baseline `fair_poor_health_rate` on `l1_acs_gini_z` with `glmnet`: `p = 0.046`
  - rich version: `p = 0.051`
- Interpretation: the BEA disposable-gap treatment currently looks more robust than ACS Gini, especially for consumption and possibly for mental distress.

### Sensitivity and event-study

- Placebo lead test for the primary ACS-Gini wellbeing setup is null (`p = 0.601`).
- Leave-one-state-out is stable; no single state flips the primary baseline result into significance.
- Event-study coefficients show significant pre-period effects, so the shock-timing design should be treated as descriptive rather than confirmatory for now.

### GRF

- Average treatment effect for `large_disp_gap_shock` on frequent mental distress is small and not significant:
  - ATE about `0.0015`
  - SE about `0.0019`
- Heterogeneity is more informative than the average effect.
- Top GRF heterogeneity features include:
  - `l1_college_share`
  - `l1_unemployment_rate`
  - `l1_hpi_yoy`
  - `l1_qcew_manufacturing_share`

## Bottom line

- The project supports a cautious consumption-side conclusion: larger BEA disposable-income gaps are associated with lower real consumption per capita.
- The wellbeing-side conclusion is weaker: there is suggestive DML evidence for mental-distress effects, but it is not yet matched by equally strong FE or event-study support.
- QCEW improves defensibility as a robustness layer and should stay in `rich`, but it does not justify replacing the narrower baseline.
