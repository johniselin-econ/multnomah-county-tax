# SLURM runbook — Multnomah pipeline

The whole paper runs as one Slurm job from the tracked launcher
`full_run.sbatch`: Stage 1 (R downloads, cached files skipped), Stage 2
(Stata: cleaning → SDID → bootstrap → tables), Stage 3 (R diagrams + maps),
then the paper-artifact audit. Stages 1 and 3 are light; Stage 2 is the
heavy lifter (the 500-rep bootstrap dominates, roughly a day on 64 cores).

Running off the cluster (laptop, no shared drive): the same three stages
work, but `user_settings.R` must set `acs_source <- "local"` — see README §4.

Cluster facts (verified 2026-10-26):
- R module:     `R/4.4.2-gfbf-2024a` (pin the full string; a bare `module load R`
  gives 4.4.1 from the 2022b toolchain)
- Stata module: `Stata/19` (MP/16 license — `c(processors_max)=16`)
- Overleaf mirror: `results/overleaf_export/{figures,tables}/`, set once by
  `oth_path` in `user_settings.do` (Stata and R read the same line)

---

## One-time setup

1. **Install R packages** (one-time per cluster R install):

   ```bash
   module load R/4.4.2-gfbf-2024a
   Rscript -e 'install.packages(c("ipumsr","tidycensus","dplyr","tidyr","readr","stringr","sf","tigris","ggplot2","tidyverse","readxl","here","patchwork","cowplot","grid"), repos="https://cloud.r-project.org")'
   ```

2. **Install Stata packages** (one-time; `STATA_REQUIREMENTS.txt` has the list):

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

3. **API keys**: `api_codes.txt` in the repo root (IPUMS + Census keys).

4. **Local settings** (both gitignored; templates `user_settings.example.{do,R}`):
   - `user_settings.R`: `acs_source <- "shared"` and `acs_shared_root` (cluster) or
     `acs_source <- "local"` (laptop).
   - `user_settings.do`: `global oth_path "results/overleaf_export/"` turns on the
     Overleaf mirror into the staging folder. A relative path is taken from the
     repo root; an absolute Dropbox path works the same way on a laptop. No
     file, or no `oth_path`, means no mirror.

---

## Each pipeline run

### Submit

From the repo root:

```bash
sbatch --output=$HOME/slurm-logs/%x-%j.out full_run.sbatch
squeue -u $USER                               # ST=R means running
tail -f ~/slurm-logs/multnomah-full-<jobid>.out
```

The `--output` override keeps the Slurm log out of the repo (the launcher's
default, `slurm-<name>-<id>.out` at the repo root, is gitignored).
Defaults: partition `week`, 64 CPUs × 2 GB, 3-day limit → `n_clusters = 4`
(floor(64/16)). For a faster bootstrap add `--cpus-per-task=128 --mem-per-cpu=1G`
(8 workers).

**Paper-only mirror.** `PAPER_ONLY=1 sbatch ... full_run.sbatch` restricts the
Overleaf mirror to the artifacts registered in `resources/paper_manifest.csv`,
in both Stata and R, so `results/overleaf_export/` holds exactly the paper's
figures and tables and nothing else.

**Fresh start.** Set `clean_results = 1` in the RUN-CONTROL FLAGS panel of
`00_multnomah.do` to wipe the regenerable `results/` subdirectories (including
the staging folder) before the run, so nothing from earlier code survives.
It refuses to combine with `resume = 1`.

### What the launcher checks

- Stata's batch log for any `r(#)` error (Stata itself returns 0 on error).
- Key artifacts exist and were rewritten after the job started.
- SDID spec or table failures logged under `code/stata/logs/sdid_failures_*.csv`.
- Stage 3 only if the node can reach the Census servers (tigris downloads);
  otherwise run it on the login node: `module load R/4.4.2-gfbf-2024a && ./run_stage3.sh`.
- The audit (`code/R/audit_paper_artifacts.R`): every manifest artifact must
  exist in `results/` and, when a mirror is configured, in the mirror. On the
  cluster the paper's `.tex` is unreachable, so the audit runs in manifest-only
  mode; with the Overleaf folder on a laptop it also reconciles against the
  `.tex` (DEAD / DRIFT reports). Reports land in `results/_runs/`.

A "completed" job is not a successful one: look for `=== SUCCESS` at the end
of the Slurm log.

### Resuming after a failure

Every `02_*.do` sources `code/utils/globals.do` itself, so a stage can be rerun
standalone from the repo root once upstream outputs exist:

```bash
module load Stata/19
stata-mp -b do code/stata/02_otherout_sdid.do     # log: 02_otherout_sdid.log
```

Set any run flags (`use_parallel`, `n_clusters`, ...) as globals in a small
driver do-file; `globals.do` only fills in what is unset. `resume = 1` skips
bootstrap reps whose draw file already exists.

### Collect artifacts

From your laptop (the Overleaf project is a Dropbox folder there):

```bash
rsync -av <cluster>:/path/to/repos/multnomah-county-tax/results/overleaf_export/ ~/Dropbox/Apps/Overleaf/Multnomah\ County/
```

---

## Pre-flight sanity checks

| Check                                            | Expected                   |
|--------------------------------------------------|----------------------------|
| `stata-mp -q -b -e 'di c(processors_max)'`       | `16` (MP/16 license)       |
| `cat user_settings.do`                           | shows `oth_path` set       |
| `cat user_settings.R`                            | `acs_source` / `acs_shared_root` set |
| `ls api_codes.txt "data/JII Covid data.dta"`     | both exist                 |
| `git status --short`                             | clean (the log records the commit) |

## Sizing rationale (MP/16)

`setup_parallel` in `code/utils/programs.do` reads `nproc` (respects the
SLURM cgroup) and caps `n_clusters = floor(visible_cores / 16)`. Each worker
then runs Stata/MP at the full 16-core license cap.

- 64 cores → 4 workers × 16 cores. Saturates SDID's 4 data blocks 1:1.
- 128 cores → 8 workers × 16 cores. SDID unchanged; bootstrap halves.
- 32 cores → forces `n_clusters=2`; leaves 2 of 4 SDID blocks waiting.
- <16 cores → forces `n_clusters=1` (no parallelism).

Never run two Stata-MP instances at once (license contention + CPU
oversubscription). Interactive/login sessions are capped at ~5 GB RAM; the
analysis stages belong in a batch job.

## Run-control knobs (`00_multnomah.do` RUN-CONTROL FLAGS panel)

- `run_cleaning` — `0` to skip the data-cleaning stage and reuse `data/working/*.dta`
- `run_indiv_analysis` — `0` to skip the individual-level descriptive margins
- `run_bootstrap` — `0` to skip the bootstrap stage entirely
- `bootstrap_reps` — `20` smoke / `100` stress / `500` publication
- `use_parallel` — `1` if `parallel` ado installed (auto-downgrades if not)
- `n_clusters` — worker count; auto-capped by `setup_parallel`
- `resume` — `1` to skip bootstrap reps whose draw .dta already exists
- `event_study_mode` — `"all"` / `"main"` / `"none"`
- `clean_results` — `1` to wipe the regenerable `results/` subdirectories first
- `paper_only` — `1` to mirror only manifest artifacts; `""` takes `PAPER_ONLY`
  from the environment (the launcher's switch)
