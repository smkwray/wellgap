# Expected outputs

When the pipeline is working, you should expect these output types.

## Tables
- `output/tables/baseline_fe_coefficients.csv`
- `output/tables/baseline_fe_table.txt`
- `output/tables/local_projection_coefficients.csv`
- `output/tables/dml_results.csv`
- `output/tables/placebo_lead_test.csv`
- `output/tables/leave_one_state_out.csv`
- `output/tables/causal_forest_ate.csv`
- `output/tables/causal_forest_importance.csv`

## Final analysis data
- `data/final/state_year_panel.csv`
- `data/final/state_year_panel.rds`

## Main interpretation targets
1. Does lagged inequality predict worse state-level wellbeing?
2. Does lagged inequality predict lower real consumption per capita?
3. Are the results robust to flexible DML adjustment?
4. Are the results driven by a handful of states?
