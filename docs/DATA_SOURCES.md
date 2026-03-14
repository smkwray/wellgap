# Data sources and access guide
## Causal effect of inequality on wellbeing and real consumption (R-only)

This document lists the full public-data stack for the project, explains what each source contributes, and shows how to retrieve each source in **R**.

The project is built around a **state-year core panel** because that is where the overlap is strongest among:
- annual inequality
- annual real consumption
- annual price levels
- annual wellbeing outcomes

---

## 1) Core integrated panel sources

| Source | Main role | Geography | Recommended years | Access in R | Primary / optional |
|---|---|---|---|---|---|
| ACS 1-year | annual inequality + demographics | state | 2012–2024 | `tidycensus` / Census API | primary |
| BEA consumer spending by state | real consumption outcomes | state | 2012–2024 | BEA API via `httr2` / `jsonlite` | primary |
| BEA regional price parities | price controls | state | 2012–2024 | BEA API | primary |
| BEA distribution of personal/disposable income | preferred inequality treatment | state | 2012–2023 | BEA download page + scraper / manual workbook parse | primary |
| BRFSS | wellbeing outcomes | state-year | 2012–2024 | CDC annual microdata + `haven` + `survey` | primary |

---

## 2) Core extension sources

| Source | Main role | Geography | Recommended years | Access in R | Primary / optional |
|---|---|---|---|---|---|
| Household Pulse Survey / HTOPS | material hardship, short-run wellbeing | respondent/state-period | 2020–2025 | Census microdata downloads | extension |
| CEX PUMD | direct household expenditures | selected states / selected MSAs | chosen years | BLS public-use microdata | extension |
| CDC WONDER | mortality extension | county/state | 2012–2024 | download/query + read in R | extension |

---

## 3) Controls and mechanism sources

| Source | Main role | Geography | Recommended years | Access in R | Primary / optional |
|---|---|---|---|---|---|
| DOL state minimum wage | institutional control / moderator | state | historical + current | scrape page/resources in R | optional |
| BLS union membership | institutional control / moderator | state | annual | scrape or manually compile official tables | optional |
| FHFA HPI | housing cost / housing pressure | state, metro, county | 2012–2024 | download FHFA CSV/XLSX resources | recommended |
| Census Building Permits Survey | housing supply proxy | state/county/place | 2012–2024 | download XLSX/CSV resources | recommended |
| BLS LAUS | unemployment | state/county/metro | 2012–2024 | BLS API / flat files / manual import | recommended |
| BLS QCEW | industry structure / wage structure | state/county | 2012–2024 | downloadable CSVs | optional |
| BEA regional personal income | average-resource controls | state | 2012–2024 | BEA API | recommended |
| SAIPE | poverty/safety-net robustness | state/county/school district | annual | Census downloads/API | optional |

---

# 4) Source-by-source instructions

## 4.1 ACS 1-year (state inequality + demographics)

### What to use it for
Use ACS as the **baseline annual inequality source** and to build demographic controls.

### Recommended variables
Core state-year pull:
- `B19083_001` — Gini index
- `B19013_001` — median household income
- `B01003_001` — total population
- `B23025_003` — labor force
- `B23025_005` — unemployed
- `B25001_001` — housing units

Recommended control tables:
- `B15003` — educational attainment
- `B03002` — race/ethnicity
- optional `B01001` — age composition
- optional `B05002` / `B05006` — nativity / foreign-born share

### Recommended years
- primary state-year panel: `2012:2024`
- keep a robustness flag for 2020 because ACS pandemic-era collection issues complicate comparisons
- for this project's main validated panel, treat `2020` as omitted rather than patched from a mixed source; see `docs/SCOPE_AND_SAMPLE.md`

### Recommended access method in R
Use `tidycensus::get_acs()` with `survey = "acs1"` and a Census API key.

### Basic R pattern
```r
library(tidycensus)

acs_state <- get_acs(
  geography = "state",
  variables = c(gini = "B19083_001", pop = "B01003_001"),
  year = 2024,
  survey = "acs1",
  output = "wide"
)
```

### Notes
- Use **ACS 1-year** for the main state-year design.
- Use **ACS 5-year** only for small-area robustness because rolling windows blur treatment timing.
- ACS measures **money income**, so it is a weaker welfare concept than post-tax/disposable resources.

### Output file in this pack
- `data/intermediate/acs_state_year.rds`
- built by `R/10_download_acs.R`

---

## 4.2 BEA consumer spending by state (real consumption outcomes)

### What to use it for
This is the **main consumption lane**.

Recommended outcomes:
- real PCE
- real PCE per capita
- real PCE growth
- PCE / personal income ratio
- current-dollar PCE category shares (exploratory)

### Recommended years
- integrated panel: `2012:2024`
- official real state series are available for the modern window; use the overlapping years with your treatment

### Access method in R
Use the BEA API, preferably via:
- `httr2`
- `jsonlite`

This pack includes a metadata-discovery helper because BEA table names can change or be revised.

### Workflow
1. Set `BEA_API_KEY` in `.Renviron`
2. Run `discover_bea_datasets()`, `discover_bea_parameters()`, and then `search_bea_tables("consumer spending")`
3. Copy the relevant table name into `config/config.yml`
4. Run `download_configured_bea_aliases()`

### Why the BEA workflow is set up this way
BEA is excellent for state consumption data, but the exact API table names and line descriptions are not as convenient as ACS table IDs. It is better to:
- discover the current table catalog first
- then set the table names explicitly in config
- then extract the line descriptions you want

### Output files in this pack
- raw table catalogs in `data/raw/bea/`
- configured table pulls in `data/intermediate/`
- standardized merge target built later as `data/intermediate/bea_state_core.csv`

---

## 4.3 BEA regional price parities (RPP)

### What to use it for
Price controls are essential because the project is about **absolute wellbeing** and **real consumption**.

Recommended measures:
- all-items RPP
- housing / rent-sensitive RPP if available and stable in the chosen table

### Access method in R
Same BEA API workflow as above:
1. discover the relevant table
2. place the chosen table name in config
3. download with `R/11_download_bea.R`

### Recommended derived variables
- `rpp_all_items`
- `rpp_housing`
- real-income controls if the chosen BEA table includes them

### Output file
- merged into `bea_state_core.csv`

---

## 4.4 BEA distribution of personal and disposable income

### What to use it for
This is the **preferred inequality treatment family** for a welfare-oriented project.

Recommended treatment candidates:
- top 20% share
- top 10% share
- mean personal income
- median personal income
- mean disposable personal income
- median disposable personal income
- mean-median gap
- disposable mean-median gap

### Why this source matters
If your outcome is wellbeing or consumption, **disposable-resource inequality** is conceptually better than pretax money-income inequality.

### Access method in R
This source is best treated as a **download-page + workbook parse** workflow:
1. scrape the official BEA distribution page for downloadable workbooks (`.xlsx`, `.csv`, `.zip`)
2. save all raw files unchanged
3. create one tidy state-year dataset from the workbook(s)
4. standardize the resulting column names

### Why the code is more template-like here
The distribution tables are very useful, but workbook structure can be revised. The pack therefore:
- includes a scraper for the download page
- includes a template for reading workbooks
- expects you to create a clean intermediate file called `bea_distribution_state_year.csv`

### Standardized columns expected by the merge script
If you create a cleaned file, use these names where possible:
- `state_fips`
- `state`
- `year`
- `disp_top20_share`
- `disp_top10_share`
- `disp_mean`
- `disp_median`
- `pers_mean`
- `pers_median`

The merge script will then derive:
- `disp_mean_median_gap = log(disp_mean) - log(disp_median)`
- `pers_mean_median_gap = log(pers_mean) - log(pers_median)`

### Output file
- `data/intermediate/bea_distribution_state_year.csv`

---

## 4.5 BRFSS annual microdata (main wellbeing lane)

### What to use it for
BRFSS is the recommended **primary wellbeing source** for the main paper.

### Recommended outcomes
Construct annual weighted state-level measures of:
- fair/poor self-rated health
- frequent mental distress
- frequent physical distress
- mean bad mental-health days
- mean bad physical-health days

### Key raw variables
- `_STATE`
- `_LLCPWT`
- `_STSTR`
- `_PSU`
- `GENHLTH`
- `MENTHLTH`
- `PHYSHLTH`

### Access method in R
1. scrape the annual BRFSS page for each year
2. download the ZIP or XPT file
3. read the SAS transport file with `haven::read_xpt()`
4. aggregate using the `survey` package

### Important coding details
- `GENHLTH`:
  - fair/poor health = values 4 or 5
- `MENTHLTH`:
  - 88 means **0 days**, not missing
  - frequent mental distress = `MENTHLTH >= 14`
- `PHYSHLTH`:
  - 88 means **0 days**
  - frequent physical distress = `PHYSHLTH >= 14`

### Why BRFSS is the best primary lane
Compared with HPS/HTOPS, BRFSS is:
- annual
- state-centered
- large enough for robust state-year aggregates
- relatively stable across time

### Output files
- raw downloads in `data/raw/brfss/`
- aggregated outcomes in `data/intermediate/brfss_state_year.rds`

---

## 4.6 Household Pulse Survey / HTOPS (hardship extension)

### What to use it for
Use HPS / HTOPS as a **second-stage extension** for short-run hardship and material wellbeing.

### Best outcome types
- food insufficiency
- difficulty paying usual household expenses
- housing payment difficulty / delinquency
- anxiety / depression indicators
- household spending pressure

### Access method in R
1. scrape the Census PUF page for CSV / ZIP links
2. save all raw files
3. build a variable crosswalk
4. aggregate to state-period or state-year

### Why this is an extension rather than the first module
The design changed over time:
- the original cross-sectional HPS period ended in September 2024
- the survey later transitioned and relaunched as HTOPS
- variable names and structures need harmonization

### Practical recommendation
Use HPS/HTOPS only after:
- the BRFSS lane is working
- the state-year core panel is stable
- you have a clear variable crosswalk

### Output file
- `data/intermediate/hps_htops_state_period.csv` or `...state_year.csv`

---

## 4.7 CEX public-use microdata (direct expenditure robustness)

### What to use it for
Use CEX only as a **robustness / validation lane** for direct household expenditure measurement.

### Good uses
- replicate the direction of the BEA consumption result
- inspect whether inequality is associated with household spending mix
- validate category-specific expenditure patterns

### Geography limitation
Public CEX geography is limited to:
- national
- regions/divisions
- selected states
- selected MSAs

So it is **not** the right source for a nationwide county panel.

### Access method in R
1. scrape the BLS PUMD page for ZIP links
2. download the selected files
3. read the family/integration files
4. use the provided expenditure and state-weight variables where relevant

### Recommendation
Do **not** make CEX the core design.  
Use it as a targeted robustness exercise once the BEA state panel is working.

### Output file
- user-defined; template script writes to `data/intermediate/cex_selected_geo.csv`

---

## 4.8 BLS LAUS (unemployment control)

### What to use it for
Main labor-market control:
- unemployment rate
- unemployment level
- labor force
- employment

### Recommended role in the project
- primary labor-market control if you want official labor-market time series
- optional if you prefer to use ACS unemployment for the first pass

### Access method in R
Because BLS access patterns vary, this pack treats LAUS as:
- a documented source in the plan
- an optional manual/API module in code

If you need the fastest path, use:
- ACS unemployment rate for the first integrated panel
- then replace or validate with LAUS later

### Output file
- `data/intermediate/laus_state_year.csv` if you build it

---

## 4.9 FHFA House Price Index (housing pressure control)

### What to use it for
Housing-market mechanism / control:
- house-price growth
- rolling appreciation
- interaction with RPP housing costs

### Recommended measures
- year-over-year HPI growth
- 3-year cumulative growth
- optionally state relative-to-national growth

### Access method in R
1. scrape the FHFA HPI page for downloadable files
2. download the chosen state-level CSV/XLSX
3. compute annualized or end-of-year measures

### Recommendation
Use state-level annualized HPI as the default control.

### Output file
- `data/intermediate/fhfa_state_hpi.csv`

---

## 4.10 Census Building Permits Survey (housing supply control)

### What to use it for
Supply-side housing pressure proxy:
- permits per 1,000 population
- single-family permits per 1,000
- multifamily share

### Access method in R
1. scrape the Building Permits Survey page for annual XLSX/CSV files
2. download the state-level files
3. aggregate if needed
4. merge to population

### Recommendation
This is a very useful control for:
- high-housing-cost states
- housing mechanism tests
- distinguishing price pressure from inequality per se

### Output file
- `data/intermediate/building_permits_state_year.csv`

---

## 4.11 DOL state minimum wage

### What to use it for
Institutional robustness / heterogeneity:
- minimum wage level
- real minimum wage
- interactions with inequality or consumption sensitivity

### Recommended role
Use as:
- a control in robustness
- a subgroup/moderator variable
- not as the main instrument for inequality

### Access method in R
1. scrape the DOL minimum-wage page and linked resources
2. preserve the historical workbook if available
3. convert to state-year

### Output file
- `data/intermediate/state_minimum_wage.csv`

---

## 4.12 BLS union membership

### What to use it for
Institutional context / moderator.

### Recommended role
Use as:
- optional control
- moderator for heterogeneity
- background descriptive variable

### Important caution
State union estimates are informative, but not so precise that tiny year-to-year changes should drive the causal argument.

### Access method in R
1. preserve the official BLS release page
2. if you need a long panel, compile the annual official state tables into one CSV
3. then merge into the panel

### Output file
- `data/intermediate/union_membership_state_year.csv`

---

## 4.13 BLS QCEW

### What to use it for
Industry composition and wage-structure context:
- tradable vs non-tradable employment mix
- sector shares
- average weekly wages by sector

### Recommended role
Optional but valuable if you want richer mechanism controls.

### Access method in R
Use the downloadable CSV files from QCEW and aggregate to:
- state-year employment shares by industry
- or a few coarse sector groups

### Output file
- `data/intermediate/qcew_state_year.csv`

---

## 4.14 CDC WONDER (optional mortality extension)

### What to use it for
A harder endpoint:
- all-cause mortality
- overdose mortality
- suicide mortality
- cardiovascular mortality

### Recommended role
Treat as a **secondary paper or extension module**, not the first integrated result.

### Access method in R
The easiest path is often:
1. run the query manually on the WONDER site
2. export to CSV
3. read the export in R

If you later automate the query workflow, keep the manual export format stable.

### Output file
- `data/intermediate/cdc_wonder_state_year.csv` or county-year if you build a substate extension

---

# 5) Recommended first-paper stack

If you want the fastest strong start, use only:

1. ACS Gini
2. BEA real PCE
3. BEA RPP
4. BRFSS
5. ACS unemployment or LAUS
6. FHFA HPI
7. Building permits
8. ACS demographics

This is already enough for:
- one main wellbeing result
- one main real-consumption result
- one transparent fixed-effects table
- one DML robustness table

---

# 6) Standardized intermediate file names

To keep the project organized, use these filenames:

- `data/intermediate/acs_state_year.rds`
- `data/intermediate/brfss_state_year.rds`
- `data/intermediate/bea_pce_state.csv`
- `data/intermediate/bea_income_state.csv`
- `data/intermediate/bea_rpp_state.csv`
- `data/intermediate/bea_state_core.csv`
- `data/intermediate/bea_distribution_state_year.csv`
- `data/intermediate/fhfa_state_hpi.csv`
- `data/intermediate/building_permits_state_year.csv`
- `data/intermediate/state_minimum_wage.csv`
- `data/intermediate/union_membership_state_year.csv`
- `data/final/state_year_panel.rds`

---

# 7) Access order recommendation

Use this order:

1. ACS
2. BEA PCE / income / RPP
3. BRFSS
4. BEA distribution files
5. housing controls
6. institutional controls
7. HPS / HTOPS
8. CEX

That order gets you to a working core panel as quickly as possible.
