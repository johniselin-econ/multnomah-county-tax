# Server re-run: frozen donor pools + per-spec placebo seeds

Temporary handoff (delete once the run is committed).

## Why a full re-run

Run 31548600 dropped Haskell County, KS, and the three k-means donor pools
were redrawn (IRS pools: Covid 85 -> 87, demographic 117 -> 144, stringency
72 -> 179; ACS stringency 35 -> 83, although Haskell was never in the ACS).
Each pool came from one seeded `cluster kmeans` run. Its random start picks
rows by position, so dropping any row can move it to a different local
optimum. Stringency match is a preferred specification, so the Table 2
elasticities and the revenue ranges moved with it.

What changed in the code:

1. **Pools 3-5 are best-of-100 k-means** (`project_kmeans_best`, programs.do):
   100 seeded starts, keep the lowest within-cluster sum of squares.
2. **Pools are frozen** in `resources/donor_pools.csv` (tracked, one row per
   county in the 2020 panel; flags `sample_urban75_covid`, `sample_demog`,
   `sample_stringency`). `02_sdid_analysis.do` writes the file when it is
   missing (or when `rebuild_donor_pools = 1`). Otherwise it reads the file and
   logs how many counties today's k-means would assign differently.
3. **`02_otherout_sdid.do` reads the same file** instead of running its own
   k-means. It had used a different seed and no narrow_only exclusion, so its
   pools did not match the main ones. Expect otherout results to move.
4. **Per-spec placebo seeds** (`spec_seed_push`/`spec_seed_pop`,
   02_spec_engine.do): each SDID spec's placebo inference (`sdid`
   `vce(placebo)` and `sdid_event` `brep()`) is seeded from master_seed and the
   spec's identity, then the caller's RNG state is restored. Before this, a
   spec's SE depended on how many draws earlier specs on the same worker used,
   so on `n_clusters` and grid order. **All placebo SEs will move** by Monte
   Carlo noise (reps = 100). Bootstrap draws are unaffected (push/pop restores
   the stream).

The run's config signature changes on purpose (new globals `donor_pool_file`,
`rebuild_donor_pools`, `kmeans_starts`).

## Steps

1. `git pull` on `October-revised`. Confirm `resources/donor_pools.csv` does
   **not** exist yet, so this run builds it.
2. Full run as usual (`sbatch full_run.sbatch`, 500-rep bootstrap). Everything
   downstream of the pools changes: SDID tables and spec curves, Table 1 /
   A1, elasticities, revenue, bootstrap CIs, otherout.
3. Check the `02_sdid_analysis` log:
   - three lines `project_kmeans_best: k=..., 100 starts, best seed ...,
     reached by N starts`. **Record N for each pool.** N = 1 means the best
     partition was found only once in 100 starts, so it is fragile; flag it
     rather than shipping.
   - `Donor pools written to .../resources/donor_pools.csv`
   - `tab1` of the three pools (2020): record the member counts.
   - the Multnomah `assert` passed (all three pools contain it).
4. Check the `02_otherout_sdid` log: no rc 601 (file found), assert passed.
5. Commit `resources/donor_pools.csv` together with `results/overleaf_export/`
   and `results/paper_numbers.csv` as usual.
6. Drift check, free on the next run: once the file exists, the
   `02_sdid_analysis` log prints `N counties differ` per pool. On unchanged
   data it should be 0.

## Report back

- Pool sizes (IRS panel and ACS panel, Table 1) and N per pool from step 3.
- Run ID, success sentinel, exit code.

## On the laptop afterwards

Sync to Overleaf, rerun `audit_paper_numbers.R`, then settle the Haskell
footnote (`handoff/haskell_exclusion_note.md`). Its "does not change our point
estimates materially" clause can be tested only now that the pools are stable:
compare against a run with Haskell restored, using the same frozen pools.
The abstract's hand-typed ranges (\$9-\$19M PFA, \$45-\$81M state) also need
updating or converting to `\pn{}`.

## Randomness review: other sites checked (no change needed)

- Bootstrap donor resample: per-rep seed (02_bootstrap.do:236). OK.
- 02_flow_analysis.do: debug sample (seeded) and parallel load-balancing
  shuffle (job order only; PPML is deterministic). OK.
- 02_revenue_microsim.do:493 itemizer draw: seeded at script top; fixed for
  fixed ACS data, but any change to the ACS rows reshuffles which filers
  itemize. Same class of fragility as the pools, small effect. Left as is.
- Tie-sensitive `keep if _n == 1` / `duplicates drop, force` sites: benign.
- R code: no stochastic calls.
