# SLURM runbook — Multnomah pipeline

Three-stage workflow on the McCleary-style cluster:

1. **Stage 1** — R data downloads (network-I/O bound)
2. **Stage 2** — Stata analysis (parallel SDID + bootstrap)
3. **Stage 3** — R post-Stata outputs (diagrams + maps)

Stages 1 and 3 are light and can run on the login node or as small SLURM
jobs. Stage 2 is the heavy lifter and is run interactively in a salloc.

Cluster facts (verified 2026-05-21):
- R module:     `R/4.4.1-foss-2022b`
- Stata module: `Stata/19` (MP/16 license — `c(processors_max)=16`)
- Project root: `/nfs/roberts/project/pi_nrs36/ji252/repos/multnomah-county-tax`
- Curated outputs land in `results/overleaf_export/{figures,tables}/`
  (driven by `user_settings.do` → `${oth_path}`)

---

## One-time setup

1. **Install R packages** (one-time per cluster R install):

   ```bash
   module load R/4.4.1-foss-2022b
   Rscript -e 'install.packages(c("ipumsr","tidycensus","dplyr","tidyr","readr","stringr","sf","tigris","ggplot2","tidyverse","readxl","here","patchwork","cowplot","grid"), repos="https://cloud.r-project.org")'
   ```

2. **Install Stata packages** (one-time):

   ```stata
   ssc install reghdfe, replace
   ssc install ftools, replace
   ssc install ppmlhdfe, replace
   ssc install sdid, replace
   ssc install sdid_event, replace
   ssc install estout, replace
   ssc install coefplot, replace
   ssc install fre, replace
   ssc install distinct, replace
   ssc install blindschemes, replace
   net install taxsimlocal35, from("https://taxsim.nber.org/stata") replace
   net install parallel, from(https://raw.github.com/gvegayon/parallel/stable/) replace
   ```

3. **API keys**: `api_codes.txt` exists in repo root (IPUMS + Census keys).

4. **Fake Overleaf staging**: `user_settings.do` is in place; mirrors curated outputs
   to `results/overleaf_export/`. Edit `oth_path` in `user_settings.do` to relocate.

---

## Each pipeline run

### Stage 1 — R downloads

**Option A: SLURM batch job (preferred — clean log, unattended).**

The repo no longer ships a `.sbatch` (it was cluster/path-specific, and `*.sbatch`
is gitignored). Create one from this template on the cluster, then submit:

```bash
cd /nfs/roberts/project/pi_nrs36/ji252/repos/multnomah-county-tax
cat > stage1.sbatch <<'SBATCH'
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
set -euo pipefail
module purge
module load R/4.4.1-foss-2022b
cd "$SLURM_SUBMIT_DIR"
./run_stage1.sh
SBATCH
sbatch stage1.sbatch
# monitor:
squeue -u $USER
tail -f slurm-stage1-*.log
```

**Option B: login-node (if cached, runs in seconds).**

```bash
cd /nfs/roberts/project/pi_nrs36/ji252/repos/multnomah-county-tax
module load R/4.4.1-foss-2022b
./run_stage1.sh
```

First pass ~30 min (dominated by the IPUMS extract queue). Cached reruns:
seconds. Hand-off check: `ls data/acs/acs_2024.csv` should now exist.

### Stage 2 — Stata interactive

**Request the allocation:**

```bash
salloc --partition=day --cpus-per-task=64 --mem-per-cpu=2G --time=08:00:00
```

64 cpus gives `n_clusters=4` (floor(64/16)=4), saturating SDID's 4 data
blocks 1:1. For a 500-rep publication bootstrap you can bump to
`--cpus-per-task=128 --mem-per-cpu=1G` to get `n_clusters=8` and roughly
halve the bootstrap wall time. SDID itself doesn't get faster — still 4
data blocks.

**Inside the allocation:**

```bash
module load Stata/19
cd /nfs/roberts/project/pi_nrs36/ji252/repos/multnomah-county-tax

# sanity-check: should print 16 for MP/16, and match --cpus-per-task for nproc
stata-mp -q -b -e 'di c(processors_max)' && tail -2 *.log && rm -f *.log
nproc

# batch run (log → code/stata/logs/00_log_multnomah_<date>.log)
stata-mp -b do 00_multnomah.do

# OR interactive
stata-mp
# . do 00_multnomah.do
```

Expected wall time: 3–5 hr at 100-rep bootstrap. Exit with `exit` when
done — don't sit on idle cores.

### Stage 3 — R post-Stata

```bash
cd /nfs/roberts/project/pi_nrs36/ji252/repos/multnomah-county-tax
module load R/4.4.1-foss-2022b
./run_stage3.sh
```

~1 minute. Renders diagrams + maps, copying paper-ready versions to
`results/overleaf_export/`.

### Collect artifacts

From your laptop:

```bash
rsync -av <cluster>:/nfs/roberts/project/pi_nrs36/ji252/repos/multnomah-county-tax/results/overleaf_export/ ~/Dropbox/Overleaf-Multnomah/
```

---

## Pre-flight sanity checks

Before the multi-hour Stata run, inside the salloc:

| Check                                            | Expected         |
|--------------------------------------------------|------------------|
| `nproc`                                          | matches `--cpus-per-task` |
| `stata-mp -q -b -e 'di c(processors_max)'`       | `16` (MP/16 license)       |
| `cat user_settings.do`                                 | shows `oth_path` set       |
| `ls data/acs/acs_2024.csv`                       | exists (Stage 1 ran)       |
| `ls results/overleaf_export/{figures,tables}`    | both exist                 |

## Sizing rationale (MP/16)

`setup_parallel` in `code/utils/programs.do` reads `nproc` (respects
SLURM cgroup) and caps `n_clusters = floor(visible_cores / 16)`. Each
worker then runs Stata/MP at the full 16-core license cap.

- 64 cores → 4 workers × 16 cores. Saturates SDID's 4 data blocks 1:1;
  100-rep bootstrap = 25 reps/worker.
- 128 cores → 8 workers × 16 cores. SDID unchanged; bootstrap halves.
- 32 cores → forces `n_clusters=2`; leaves 2 of 4 SDID blocks waiting.
- <16 cores → forces `n_clusters=1` (no parallelism).

## Run-control knobs (`00_multnomah.do` PROJECT GLOBALS panel)

- `run_bootstrap` — `0` to skip the bootstrap stage entirely
- `bootstrap_reps` — `20` smoke / `100` stress / `500` publication
- `use_parallel` — `1` if `parallel` ado installed (auto-downgrades if not)
- `n_clusters` — worker count; auto-capped by `setup_parallel`
- `resume` — `1` to skip bootstrap reps whose draw .dta already exists
- `event_study_mode` — `"all"` / `"main"` / `"none"`
