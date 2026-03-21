# Expected outputs

When the pipeline completes, you should expect these outputs.

## Final tables (locked, public-facing)
- `output/tables/final_main_wellbeing_table.csv` — Primary treatment × primary outcomes, CR2/Satterthwaite inference
- `output/tables/final_robustness_wellbeing_table.csv` — CCE, subgroup, measurement, falsification branches (primary treatment only, labeled inference standard)
- `output/tables/final_hard_outcome_table.csv` — Mortality falsification (with `treatment_role`)
- `output/tables/final_extension_outcomes_table.csv` — Secondary wellbeing and material outcomes (with `treatment_role`)

## Model outputs (intermediate)
- `output/tables/baseline_fe_coefficients.csv` — Baseline FE coefficients
- `output/tables/fe_spec_coefficients.csv` — Full spec-grid FE coefficients (with `treatment_role`)
- `output/tables/baseline_fe_table.txt` — Baseline FE formatted table
- `output/tables/fe_spec_table.txt` — Full spec-grid formatted table
- `output/tables/local_projection_coefficients.csv` — Local projection impulse responses (with `treatment_role`)
- `output/tables/dml_results.csv` — Double machine learning results (with `treatment_role`)
- `output/tables/wellbeing_dynamic_fe.csv` — Dynamic FE for wellbeing outcomes
- `output/tables/wellbeing_longdiff_coefficients.csv` — Long-difference wellbeing models
- `output/tables/wellbeing_cce_dynamic_fe.csv` — Correlated common effects models
- `output/tables/wellbeing_measurement_sensitivity.csv` — Age-standardized and precision-weighted variants
- `output/tables/wellbeing_falsification_dynamic_fe.csv` — Within-BRFSS falsification (physical distress)
- `output/tables/wellbeing_hard_outcome_falsification.csv` — Mortality falsification (pre-aggregation)
- `output/tables/wellbeing_subgroup_dynamic_fe.csv` — Subgroup reverse-causality checks
- `output/tables/extended_outcomes_dynamic_fe.csv` — Secondary/material wellbeing extensions

## Sensitivity outputs
- `output/tables/placebo_lead_test.csv` — Lead placebo test
- `output/tables/leave_one_state_out.csv` — Leave-one-state-out jackknife
- `output/tables/sample_split_fe.csv` — Sample-split validation
- `output/tables/sensemakr_frequent_mental_distress_rate_l1_disp_top10_share_z.txt` — Omitted-variable sensitivity (primary)
- `output/tables/sensemakr_frequent_mental_distress_rate_l1_acs_gini_z.txt` — Omitted-variable sensitivity (alternative)

## Exploratory outputs
- `output/tables/event_study_coefficients.csv` — Event-study scaffold (requires defensible treatment timing)
- `output/tables/causal_forest_ate.csv` — Generalized random forest ATE
- `output/tables/causal_forest_importance.csv` — GRF variable importance

## Logs and notes
- `output/logs/validation_report.md` — Validation summary
- `output/logs/validation_checks.csv` — Validation check details
- `output/logs/final_results_note.md` — Compact results summary
- `output/logs/identification_note.md` — Identification strategy memo
- `output/logs/causal_robustness_note.md` — Robustness test summary
- `output/logs/extended_outcomes_note.md` — Hard outcomes interpretation
- `output/logs/post_qcew_interpretation_memo.md` — Labor market controls memo

## Final analysis data
- `data/final/state_year_panel.rds`
