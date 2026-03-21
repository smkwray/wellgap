# Validation Report

- Generated: 2026-03-21 07:12:03 EDT
- PASS: 84
- WARN: 1
- FAIL: 0

| Check | Status | Detail |
|---|---|---|
| config_primary_treatment_defined | PASS | primary=l1_disp_top10_share_z |
| config_primary_not_in_alternatives | PASS | primary=l1_disp_top10_share_z alternatives=l1_acs_gini_z,l1_disp_mean_median_gap_z |
| file_exists data/intermediate/bea_state_core.csv | PASS | data/intermediate/bea_state_core.csv |
| file_exists data/intermediate/bea_distribution_state_year.csv | PASS | data/intermediate/bea_distribution_state_year.csv |
| file_exists data/intermediate/acs_state_year.csv | PASS | data/intermediate/acs_state_year.csv |
| file_exists data/intermediate/brfss_state_year.csv | PASS | data/intermediate/brfss_state_year.csv |
| file_exists data/intermediate/brfss_state_year_subgroups.csv | PASS | data/intermediate/brfss_state_year_subgroups.csv |
| file_exists data/intermediate/brfss_age_reference_weights.csv | PASS | data/intermediate/brfss_age_reference_weights.csv |
| file_exists data/intermediate/cdc_wonder_state_year.csv | PASS | data/intermediate/cdc_wonder_state_year.csv |
| file_exists data/intermediate/fhfa_state_hpi.csv | PASS | data/intermediate/fhfa_state_hpi.csv |
| file_exists data/intermediate/building_permits_state_year.csv | PASS | data/intermediate/building_permits_state_year.csv |
| file_exists data/intermediate/state_minimum_wage.csv | PASS | data/intermediate/state_minimum_wage.csv |
| file_exists data/intermediate/union_membership_state_year.csv | PASS | data/intermediate/union_membership_state_year.csv |
| file_exists data/intermediate/qcew_state_year.csv | PASS | data/intermediate/qcew_state_year.csv |
| file_exists data/final/state_year_panel.rds | PASS | data/final/state_year_panel.rds |
| file_exists output/tables/baseline_fe_coefficients.csv | PASS | output/tables/baseline_fe_coefficients.csv |
| file_exists output/tables/fe_spec_coefficients.csv | PASS | output/tables/fe_spec_coefficients.csv |
| file_exists output/tables/local_projection_coefficients.csv | PASS | output/tables/local_projection_coefficients.csv |
| file_exists output/tables/placebo_lead_test.csv | PASS | output/tables/placebo_lead_test.csv |
| file_exists output/tables/leave_one_state_out.csv | PASS | output/tables/leave_one_state_out.csv |
| file_exists output/tables/sample_split_fe.csv | PASS | output/tables/sample_split_fe.csv |
| file_exists output/tables/wellbeing_dynamic_fe.csv | PASS | output/tables/wellbeing_dynamic_fe.csv |
| file_exists output/tables/wellbeing_longdiff_coefficients.csv | PASS | output/tables/wellbeing_longdiff_coefficients.csv |
| file_exists output/tables/wellbeing_cce_dynamic_fe.csv | PASS | output/tables/wellbeing_cce_dynamic_fe.csv |
| file_exists output/tables/wellbeing_measurement_sensitivity.csv | PASS | output/tables/wellbeing_measurement_sensitivity.csv |
| file_exists output/tables/wellbeing_falsification_dynamic_fe.csv | PASS | output/tables/wellbeing_falsification_dynamic_fe.csv |
| file_exists output/tables/wellbeing_hard_outcome_falsification.csv | PASS | output/tables/wellbeing_hard_outcome_falsification.csv |
| file_exists output/tables/extended_outcomes_dynamic_fe.csv | PASS | output/tables/extended_outcomes_dynamic_fe.csv |
| file_exists output/tables/final_main_wellbeing_table.csv | PASS | output/tables/final_main_wellbeing_table.csv |
| file_exists output/tables/final_robustness_wellbeing_table.csv | PASS | output/tables/final_robustness_wellbeing_table.csv |
| file_exists output/tables/final_hard_outcome_table.csv | PASS | output/tables/final_hard_outcome_table.csv |
| file_exists output/tables/final_extension_outcomes_table.csv | PASS | output/tables/final_extension_outcomes_table.csv |
| file_exists output/tables/wellbeing_subgroup_dynamic_fe.csv | PASS | output/tables/wellbeing_subgroup_dynamic_fe.csv |
| file_exists output/tables/dml_results.csv | PASS | output/tables/dml_results.csv |
| file_exists output/tables/event_study_coefficients.csv | PASS | output/tables/event_study_coefficients.csv |
| file_exists output/tables/causal_forest_ate.csv | PASS | output/tables/causal_forest_ate.csv |
| file_exists output/tables/causal_forest_importance.csv | PASS | output/tables/causal_forest_importance.csv |
| panel_row_count | PASS | rows=612 expected=612 |
| panel_state_count | PASS | states=51 expected=51 |
| panel_years_match | PASS | years=2012,2013,2014,2015,2016,2017,2018,2019,2021,2022,2023,2024 |
| panel_no_duplicates | PASS | duplicates=0 |
| panel_2020_omitted | PASS | has_2020=FALSE |
| panel_2021_lags_missing | PASS | 2021_non_missing_l1_acs_gini_z=0 2021_non_missing_l1_disp_mean_median_gap_z=0 |
| coverage acs_gini | PASS | non_missing=612 threshold=600 |
| coverage frequent_mental_distress_rate | PASS | non_missing=607 threshold=600 |
| coverage fair_poor_health_rate | PASS | non_missing=607 threshold=600 |
| coverage log_real_pce_pc | PASS | non_missing=612 threshold=600 |
| coverage disp_mean_median_gap_z | PASS | non_missing=561 threshold=550 |
| coverage disp_top10_share_z | PASS | non_missing=561 threshold=550 |
| coverage l1_acs_gini_z | PASS | non_missing=510 threshold=500 |
| coverage l1_disp_top10_share_z | PASS | non_missing=510 threshold=500 |
| coverage l1_disp_mean_median_gap_z | PASS | non_missing=510 threshold=500 |
| coverage l1_hpi_yoy | WARN | non_missing=459 threshold=500 |
| coverage l1_permits_per_1000 | PASS | non_missing=510 threshold=500 |
| coverage l1_state_min_wage_nominal | PASS | non_missing=510 threshold=500 |
| coverage l1_union_membership_rate | PASS | non_missing=510 threshold=500 |
| coverage qcew_private_avg_wkly_wage | PASS | non_missing=612 threshold=600 |
| coverage l1_qcew_private_avg_wkly_wage | PASS | non_missing=510 threshold=500 |
| coverage frequent_mental_distress_rate_age_std | PASS | non_missing=607 threshold=600 |
| coverage fair_poor_health_rate_age_std | PASS | non_missing=607 threshold=600 |
| coverage frequent_mental_distress_rate_precision_wt | PASS | non_missing=607 threshold=600 |
| coverage fair_poor_health_rate_precision_wt | PASS | non_missing=607 threshold=600 |
| coverage l1_qcew_manufacturing_share | PASS | non_missing=509 threshold=500 |
| coverage suicide_age_adjusted_rate | PASS | non_missing=408 threshold=400 |
| coverage drug_poisoning_age_adjusted_rate | PASS | non_missing=408 threshold=400 |
| coverage employment_to_population_ratio | PASS | non_missing=612 threshold=600 |
| coverage log_rpp_adj_median_hh_income | PASS | non_missing=612 threshold=600 |
| fe_specs_present | PASS | specs=baseline,rich,weighted |
| fe_primary_treatment_present | PASS | treatments=l1_acs_gini_z,l1_disp_mean_median_gap_z,l1_disp_top10_share_z |
| dml_specs_present | PASS | specs=baseline,rich |
| dml_primary_treatment_present | PASS | treatments=l1_acs_gini_z,l1_disp_mean_median_gap_z,l1_disp_top10_share_z |
| sample_split_rows | PASS | rows=10 |
| wellbeing_dynamic_primary_treatment_present | PASS | treatments=l1_acs_gini_z,l1_disp_mean_median_gap_z,l1_disp_top10_share_z |
| wellbeing_subgroups_present | PASS | subgroups=age_65_plus,retired |
| wellbeing_cce_primary_treatment_present | PASS | treatments=l1_acs_gini_z,l1_disp_mean_median_gap_z,l1_disp_top10_share_z |
| wellbeing_measurement_variants_present | PASS | model_types=dynamic_fe_age_std,dynamic_fe_age_std_precision_weighted,dynamic_fe_precision_weighted |
| wellbeing_falsification_outcomes_present | PASS | outcomes=frequent_physical_distress_rate,mean_bad_physical_days |
| wellbeing_hard_outcomes_present | PASS | outcomes=drug_poisoning_age_adjusted_rate,suicide_age_adjusted_rate |
| extended_outcome_families_present | PASS | families=material_wellbeing,secondary_wellbeing |
| final_main_table_primary_treatment_present | PASS | treatments=l1_disp_top10_share_z |
| final_main_table_inference_standard | PASS | has_col=TRUE values=CR2_Satterthwaite |
| final_robustness_table_inference_standard | PASS | has_col=TRUE |
| final_robustness_table_primary_treatment_only | PASS | treatments=l1_disp_top10_share_z |
| final_hard_outcome_table_inference_standard | PASS | has_col=TRUE |
| final_extension_table_inference_standard | PASS | has_col=TRUE |
