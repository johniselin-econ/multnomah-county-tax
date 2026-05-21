#!/usr/bin/env bash
#SBATCH --job-name=multnomah-stage1
#SBATCH --partition=day
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=8G
#SBATCH --time=02:00:00
#SBATCH --output=slurm-stage1-%j.log
#SBATCH --mail-type=END,FAIL

# Stage 1 — R data downloads (ACS via IPUMS, NHGIS, BLS, DOL, Census).
# Single-threaded R; resources sized for network I/O plus modest CSV churn.
# First pass: ~30 min (IPUMS extract queue dominates). Cached reruns: seconds.

set -euo pipefail

module purge
module load R/4.4.1-foss-2022b

cd /nfs/roberts/project/pi_nrs36/ji252/repos/multnomah-county-tax

./run_stage1.sh
