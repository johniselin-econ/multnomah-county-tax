#!/usr/bin/env bash
#SBATCH --job-name=multnomah-full
#SBATCH --partition=week
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=64
#SBATCH --mem-per-cpu=2G
#SBATCH --time=3-00:00:00
#SBATCH --output=slurm-%x-%j.out
#SBATCH --mail-type=END,FAIL
#
# The whole paper in one job: Stage 1 (R downloads, cached files skipped),
# Stage 2 (Stata: cleaning -> SDID -> 500-rep bootstrap -> tables), Stage 3
# (R diagrams + maps) and the paper-artifact audit.
#
# Submit FROM THE REPO ROOT:
#     sbatch --output=$HOME/slurm-logs/%x-%j.out full_run.sbatch
# (the --output override keeps the Slurm log out of the repo; without it the
# log lands at the repo root as slurm-<job>-<id>.out, which is gitignored).
#
# Switches (environment variables, read by both Stata and R):
#     PAPER_ONLY=1   mirror only resources/paper_manifest.csv artifacts to the
#                    Overleaf folder (user_settings.do: oth_path)
#     PAPER_ONLY=1 sbatch --output=... full_run.sbatch
# Run-control knobs (bootstrap reps, clean_results, resume, ...) live in the
# RUN-CONTROL FLAGS panel at the top of 00_multnomah.do.
#
# 64 cpus -> n_clusters = 4 (floor(64/16), MP/16 license per worker).
# Faster bootstrap: --cpus-per-task=128 --mem-per-cpu=1G -> n_clusters = 8.

set -euo pipefail

cd "$SLURM_SUBMIT_DIR"
test -f 00_multnomah.do || { echo "ERROR: submit from the repo root"; exit 1; }
start_epoch=$(date +%s)

echo "=== multnomah full run  $(date)"
echo "node: $(hostname)   nproc: $(nproc)   mem: ${SLURM_MEM_PER_CPU:-?}MB x ${SLURM_CPUS_PER_TASK:-?}"
echo "repo: $(pwd)"
echo "branch: $(git branch --show-current) @ $(git rev-parse --short HEAD)"
echo "PAPER_ONLY=${PAPER_ONLY:-0}"
if [ -n "$(git status --porcelain)" ]; then
    echo "WARNING: working tree not clean:"; git status --porcelain
fi

# ---- preflight --------------------------------------------------------------
for f in api_codes.txt user_settings.R "data/JII Covid data.dta"; do
    test -f "$f" || { echo "ERROR: missing input $f"; exit 1; }
done
test -f user_settings.do || echo "NOTE: no user_settings.do -> Overleaf mirror off"

# ---- Stage 1: R downloads (cached files are skipped) -----------------------
module load R/4.4.2-gfbf-2024a
echo "=== Stage 1  $(date)"
./run_stage1.sh

# ---- Stage 2: Stata ---------------------------------------------------------
module load Stata/19
echo "=== Stage 2  $(date)"
stata-mp -q -b -e 'di "processors_max = " c(processors_max)' || true
cat stata.log 2>/dev/null | grep processors_max || true
rm -f stata.log

stata_log=00_multnomah.log      # Stata batch writes it next to the do-file
rm -f "$stata_log"
stata-mp -b do 00_multnomah.do  # returns 0 even on error: inspect the log below
echo "=== Stata finished  $(date)   (exit $?)"

if grep -q -E '^r\([0-9]+\);' "$stata_log"; then
    echo "ERROR: Stata stopped with an error. Last error context from $stata_log:"
    grep -n -B12 -E '^r\([0-9]+\);' "$stata_log" | tail -60
    exit 1
fi

# Artifacts that a complete run must have (re)written after start
for f in results/run_manifest_latest.csv results/sdid/sdid_results.dta \
         results/sdid/sdid_event_results.dta results/bootstrap/bootstrap_draws.dta \
         data/working/revenue_parameters.dta data/working/acs_county_sample.xlsx; do
    if [ ! -f "$f" ]; then echo "ERROR: expected artifact missing: $f"; exit 1; fi
    if [ "$(stat -c %Y "$f")" -lt "$start_epoch" ]; then echo "ERROR: artifact not refreshed by this run: $f"; exit 1; fi
done
if grep -q '^FALLBACK: using taxsim_fallback_calc' "$stata_log"; then
    echo "WARNING: TAXSIM fell back to taxsim_fallback_calc (taxsim_fallback_used=1); avg_total_rate* are approximations"
fi
if ls code/stata/logs/sdid_failures_*.csv >/dev/null 2>&1; then
    echo "NOTE: SDID spec/table failures were logged: $(ls -t code/stata/logs/sdid_failures_*.csv | head -1)"
fi
echo "Stage 2 OK: $(grep -c . "$stata_log") log lines"
rm -f "$stata_log"              # the dated copy lives in code/stata/logs/

# ---- Stage 3: R post-Stata (needs tigris downloads) ------------------------
echo "=== Stage 3  $(date)"
if curl -s -o /dev/null -m 20 https://www2.census.gov; then
    ./run_stage3.sh
else
    echo "NOTE: no internet from this node; run on the login node:"
    echo "      module load R/4.4.2-gfbf-2024a && ./run_stage3.sh"
fi

# ---- Audit: every paper artifact exists (and is in the Overleaf mirror) -----
echo "=== Audit  $(date)"
Rscript code/R/audit_paper_artifacts.R
Rscript code/R/audit_paper_numbers.R

echo "=== SUCCESS  $(date)   elapsed $(( ($(date +%s) - start_epoch) / 60 )) min"
