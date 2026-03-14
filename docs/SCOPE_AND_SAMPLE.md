# Scope and Sample Decision

Generated: 2026-03-13 18:02 EDT

## Main decision

The main project sample should remain:

- `2012-2019`
- `2021-2024`

This means the primary integrated panel intentionally omits `2020`.

## Why 2020 is omitted

### 1. Data comparability

- The standard ACS 1-year path used elsewhere in the project does not provide the same clean workflow for `2020`.
- Backfilling `2020` from a different source or alternate ACS pathway would create a non-uniform treatment series inside the main analysis window.

### 2. Design defensibility

- `2020` is not just another missing year. It is also a pandemic shock year with unusually large economic, behavioral, health, and survey disruptions.
- Treating it as part of the same smooth annual identifying design would require more justification than the rest of the panel.

### 3. Interpretation clarity

- The current main design is easy to explain: a harmonized annual state panel with one omitted break year.
- A mixed-source `2020` patch would improve apparent completeness but weaken the credibility of the core specification.

## What counts as the primary design

- Geography: states plus DC
- Panel years: `2012-2019, 2021-2024`
- Primary treatments:
  - `l1_acs_gini_z`
  - `l1_disp_mean_median_gap_z`
- Primary FE/DML spec family:
  - `baseline`
  - `rich`
  - `weighted`

## What counts as a robustness extension

`2020` can still be added later, but only as a clearly labeled robustness exercise if all of the following are true:

- the source is documented and reproducible
- the definition is harmonized with the rest of the ACS treatment series
- the project explicitly reports that `2020` uses a different construction path
- results are compared against the frozen main sample rather than replacing it

## Recommended language

Use this framing in writeups:

> The primary panel covers 2012-2019 and 2021-2024. We omit 2020 from the main specification because the standard ACS 1-year treatment path is not available in the same form for that year, and because 2020 is also an unusually disruptive pandemic-period break year. This choice favors comparability and identification clarity over nominal panel completeness.

## Bottom line

For this project, omitting `2020` in the main specification is the more defensible choice.
