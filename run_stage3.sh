#!/usr/bin/env bash
# run_stage3.sh — Pipeline Stage 3: post-Stata R outputs
#
# Renders conceptual diagrams (Figs 2 & 3) and choropleth maps. ~1 minute,
# single-threaded. Safe on the login node.
#
# Requires Stage 2 (Stata) to have produced data/working/acs_county_sample.xlsx;
# fig_diagrams runs regardless, map_code.R skips with a warning if the handoff
# is missing.

set -euo pipefail

script_dir="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$script_dir"

if ! command -v Rscript >/dev/null 2>&1; then
    echo "ERROR: Rscript not found on PATH." >&2
    echo "       Load an R module first, e.g.: module load R/4.4.2" >&2
    exit 1
fi

handoff="data/working/acs_county_sample.xlsx"
if [[ ! -f "$handoff" ]]; then
    echo "WARNING: Stage-2 handoff missing: $handoff" >&2
    echo "         Diagrams will render; maps will be skipped." >&2
fi

echo "[Stage 3] Starting at $(date)"
Rscript 00_post_stata.R
echo "[Stage 3] Done at $(date)"
