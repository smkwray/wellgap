#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RAW_DIR="$ROOT/data/raw/cdc_wonder"
ARTIFACT_DIR="$ROOT/output/playwright/cdc_wonder"
PWCLI="${PLAYWRIGHT_WRAPPER:-${PWCLI:-}}"

if ! command -v npx >/dev/null 2>&1; then
  echo "npx is required for the Playwright wrapper." >&2
  exit 1
fi

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required to convert Playwright JSON output into CSV." >&2
  exit 1
fi

if [[ -z "$PWCLI" || ! -x "$PWCLI" ]]; then
  echo "Set PLAYWRIGHT_WRAPPER to an executable playwright_cli.sh path." >&2
  exit 1
fi

mkdir -p "$RAW_DIR" "$ARTIFACT_DIR"

extract_eval_result() {
  awk '
    /^### Result$/ { capture = 1; next }
    /^### Ran Playwright code$/ { capture = 0 }
    capture { print }
  '
}

open_request_form() {
  local session="$1"
  PLAYWRIGHT_CLI_SESSION="$session" "$PWCLI" open "https://wonder.cdc.gov/ucd-icd10.html" >/dev/null

  # Accept the data use agreement if the browser lands on the splash page.
  PLAYWRIGHT_CLI_SESSION="$session" "$PWCLI" eval \
    "() => {
      const btn = Array.from(document.querySelectorAll('button, input[type=button], input[type=submit]'))
        .find(el => /i agree/i.test((el.value || el.textContent || '').trim()));
      if (btn) btn.click();
      return document.title;
    }" >/dev/null

  PLAYWRIGHT_CLI_SESSION="$session" "$PWCLI" eval \
    "() => {
      if (!document.forms['form']) return 'missing-form';
      return document.forms['form'].action;
    }" >/dev/null
}

configure_query() {
  local session="$1"
  local cause_mode="$2"
  local cause_json="$3"

  PLAYWRIGHT_CLI_SESSION="$session" "$PWCLI" eval \
    "() => {
      const causeMode = \"$cause_mode\";
      const selectedCauseValues = ${cause_json};
      const setSingle = (sel, value) => {
        sel.value = value;
        sel.dispatchEvent(new Event('change', { bubbles: true }));
      };
      const setMulti = (sel, values) => {
        Array.from(sel.options).forEach(opt => {
          opt.selected = values.includes(opt.value);
        });
        sel.dispatchEvent(new Event('change', { bubbles: true }));
      };

      setSingle(document.querySelector('select[name=\"B_1\"]'), 'D76.V9-level1');
      setSingle(document.querySelector('select[name=\"B_2\"]'), 'D76.V1-level1');
      setSingle(document.querySelector('select[name=\"B_3\"]'), '*None*');
      setSingle(document.querySelector('select[name=\"B_4\"]'), '*None*');
      setSingle(document.querySelector('select[name=\"B_5\"]'), '*None*');

      document.getElementById('CO_aar_enable').checked = true;
      document.getElementById('export-option').checked = false;
      document.querySelector('select[name=\"O_export-format\"]').value = 'csv';

      if (causeMode !== 'all') {
        Array.from(document.querySelectorAll('input[type=radio][name=\"O_ucd\"]')).forEach(el => {
          el.checked = el.value === causeMode;
        });

        const causeSelectName = causeMode === 'D76.V25' ? 'F_D76.V25' : 'V_D76.V4';
        setMulti(document.querySelector('select[name=\"' + causeSelectName + '\"]'), selectedCauseValues);
      }

      return {
        b1: document.querySelector('select[name=\"B_1\"]').value,
        b2: document.querySelector('select[name=\"B_2\"]').value,
        causeMode,
        causes: causeMode === 'all'
          ? '*All*'
          : Array.from(
              document.querySelector(
                'select[name=\"' + (causeMode === 'D76.V25' ? 'F_D76.V25' : 'V_D76.V4') + '\"]'
              ).selectedOptions
            ).map(o => o.value),
        aar: document.getElementById('CO_aar_enable').checked
      };
    }" >/dev/null
}

submit_query() {
  local session="$1"
  PLAYWRIGHT_CLI_SESSION="$session" "$PWCLI" eval \
    "() => {
      document.getElementById('submit-button1').click();
      return 'submitted';
    }" >/dev/null

  PLAYWRIGHT_CLI_SESSION="$session" "$PWCLI" eval \
    "() => document.title" | extract_eval_result | grep -q "Results Form"
}

extract_rows_json() {
  local session="$1"
  PLAYWRIGHT_CLI_SESSION="$session" "$PWCLI" eval \
    "() => Array.from(document.querySelectorAll('table.response-form tr'))
      .map(tr => Array.from(tr.querySelectorAll('th,td')).map(cell => cell.innerText.replace(/\\s+/g, ' ').trim()))
      .filter(cells => cells.length === 6 && /^\\d{4}$/.test(cells[1]))
      .map(cells => ({
        State: cells[0],
        Year: cells[1],
        Deaths: cells[2],
        Population: cells[3],
        Crude_Rate: cells[4],
        Age_Adjusted_Rate: cells[5]
      }))"
}

write_csv() {
  local json_file="$1"
  local csv_file="$2"
  jq -r '
    ["State","Year","Deaths","Population","Crude_Rate","Age_Adjusted_Rate"],
    (.[] | select((.Year | tonumber) >= 2012) | [.State, .Year, .Deaths, .Population, .Crude_Rate, .Age_Adjusted_Rate])
    | @csv
  ' "$json_file" > "$csv_file"
}

run_query() {
  local label="$1"
  local cause_mode="$2"
  local cause_json="$3"
  local session
  case "$label" in
    all_cause) session="wal" ;;
    cardiovascular) session="wcv" ;;
    suicide) session="wsu" ;;
    drug_poisoning) session="wdp" ;;
    *) session="wtmp" ;;
  esac
  local json_file="$ARTIFACT_DIR/${label}.json"
  local csv_file="$RAW_DIR/cdc_wonder_${label}_state_year.csv"

  echo "Running CDC WONDER query for ${label}..."
  open_request_form "$session"
  configure_query "$session" "$cause_mode" "$cause_json"
  submit_query "$session"
  extract_rows_json "$session" | extract_eval_result > "$json_file"
  write_csv "$json_file" "$csv_file"
  echo "Saved $csv_file"
}

run_query "suicide" "D76.V4" '["GR113-124"]'
run_query "drug_poisoning" "D76.V25" '["D1","D2","D3","D4"]'
