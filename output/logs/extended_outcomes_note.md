# Extended Outcomes Note

Generated: 2026-03-14 09:10 EDT

## What was added
- Secondary wellbeing extension outcomes were run in dynamic FE:
  - `frequent_physical_distress_rate`
  - `mean_bad_physical_days`
- Material-wellbeing extension outcomes were derived from the existing ACS/BEA panel and run in dynamic FE:
  - `log_rpp_adj_median_hh_income`
  - `employment_to_population_ratio`

## Main read
- The secondary wellbeing extensions are mostly null.
- `frequent_physical_distress_rate` is only weakly positive in the baseline spec:
  - `l1_disp_top10_share_z`: `p = 0.107`
  - `l1_disp_mean_median_gap_z`: `p = 0.137`
  - `l1_acs_gini_z`: `p = 0.166`
- `mean_bad_physical_days` is null throughout.
- `employment_to_population_ratio` is null throughout.
- `log_rpp_adj_median_hh_income` is the only material extension with non-null signals, but the signs are not coherent across treatments:
  - rich spec `l1_acs_gini_z`: negative, `p = 0.028`
  - rich spec `l1_disp_mean_median_gap_z`: positive, `p = 0.037`

## Interpretation
- The added physical-distress outcomes do not materially strengthen the wellbeing case.
- The material-wellbeing extension suggests inequality may be related to the typical household-income position, but the cross-treatment sign pattern is too mixed to carry a clean headline.
- These extension outcomes are best treated as secondary evidence rather than a new core result.

## Implementation note
- Hooks were added for broader mortality extensions like all-cause and cardiovascular mortality, but those larger CDC WONDER queries were not stable enough to keep in the default automated fetch path on this machine.
