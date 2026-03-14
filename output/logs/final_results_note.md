# Final Results Note

Generated: 2026-03-13 18:02 EDT

## Project status

- Validation passes `44/44` checks.
- The final integrated panel is `612` rows x `119` columns.
- The main sample is `2012-2019` and `2021-2024`.
- The project uses two active treatment families:
  - `l1_acs_gini_z`
  - `l1_disp_mean_median_gap_z`

## Preferred specification framing

- `baseline` is the main specification.
- `rich` is the demanding robustness specification, adding minimum wage, union membership, and QCEW labor-structure controls.
- `weighted` is the population-weighted FE robustness specification.

QCEW should remain in `rich`, not `baseline`.

## Main empirical read

### 1. Consumption results are the clearest part of the project

The strongest fixed-effects result is the weighted FE association between the BEA disposable-income mean-median gap and real consumption per capita:

- outcome: `log_real_pce_pc`
- treatment: `l1_disp_mean_median_gap_z`
- estimate: about `-0.0087`
- `p = 0.004`
- `n = 510`

This result weakens but does not reverse in the richer QCEW robustness specification:

- rich estimate: about `-0.0053`
- `p = 0.089`

Interpretation: larger disposable-income gaps are associated with lower real consumption per capita, and part of that association may overlap with labor-market structure, but the negative sign survives the richer specification.

### 2. Wellbeing results are suggestive, not definitive

The strongest DML result is:

- outcome: `frequent_mental_distress_rate`
- treatment: `l1_disp_mean_median_gap_z`
- learner: `xgboost`
- theta: about `-0.0025`
- `p = 0.032`
- `n = 505`

ACS Gini still shows a weaker wellbeing signal in DML:

- baseline `fair_poor_health_rate` on `l1_acs_gini_z` with `glmnet`
- theta: about `0.0033`
- `p = 0.046`

But those wellbeing patterns are not matched by equally strong FE evidence.

### 3. Event-study evidence is not strong confirmation

The event-study branch shows significant pre-period coefficients. That means it should be used as descriptive timing evidence, not as a clean causal confirmation.

### 4. Heterogeneity matters more than average shock effects

The causal forest average treatment effect for `large_disp_gap_shock` is small and not significant, but heterogeneity is meaningful. The more important moderators include:

- `l1_college_share`
- `l1_unemployment_rate`
- `l1_hpi_yoy`
- `l1_qcew_manufacturing_share`

## Defensible conclusion

The most defensible conclusion from the current project is:

> Higher state-level disposable-income inequality is associated with lower real consumption per capita, with the strongest evidence coming from the BEA disposable mean-median gap treatment. Evidence for wellbeing effects is suggestive, especially in DML for mental distress, but weaker and less consistent than the consumption-side result.

## Scope decision

The main panel should remain frozen at `2012-2019` and `2021-2024`. The omission of `2020` improves comparability and identification clarity and should be treated as a design choice, not as an unfinished data gap.

See also:

- `output/logs/post_qcew_interpretation_memo.md`
- `docs/SCOPE_AND_SAMPLE.md`
