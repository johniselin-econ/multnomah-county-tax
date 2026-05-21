#!/usr/bin/env bash
# run_stage1.sh — Pipeline Stage 1: R data downloads
#
# Pulls ACS microdata (IPUMS), NHGIS, BLS LAUS, DOL childcare, Census
# centroids. Network-I/O bound; safe on the login node. First pass: ~30 min
# depending on the IPUMS extract queue. Cached reruns: seconds.
#
# Prereqs:
#   - R 4.1+ on PATH (e.g. `module load R/4.4.2`)
#   - api_codes.txt in repo root with IPUMS and Census keys (README §3)
#   - R packages installed (README §1)

set -euo pipefail

script_dir="$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
cd "$script_dir"

if ! command -v Rscript >/dev/null 2>&1; then
    echo "ERROR: Rscript not found on PATH." >&2
    echo "       Load an R module first, e.g.: module load R/4.4.2" >&2
    exit 1
fi

if [[ ! -f "api_codes.txt" ]]; then
    echo "ERROR: api_codes.txt missing in $(pwd)" >&2
    echo "       See README §3 for setup." >&2
    exit 1
fi

echo "[Stage 1] $(Rscript --version 2>&1 | head -1)"
echo "[Stage 1] Starting downloads at $(date)"
Rscript 00_download_data.R
echo "[Stage 1] Done at $(date)"
