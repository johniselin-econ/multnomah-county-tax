# Note for the paper: Haskell County, KS dropped from the IRS data

Temporary handoff (delete once the footnote is in). Applies from the first run
after commit "Drop Haskell County, KS from the IRS county data".

## What changed in the code

- `globals.do` sets `excluded_fips "20081"` (Haskell County, KS).
- `01f_irs_migration.do` and `01g_irs_agi.do` drop it from every IRS county
  file: the gross in/out/net panel, the county-to-county flow file (as origin
  and as destination), and the AGI-by-bracket file. So it is absent from all
  IRS-based work: the SDID panels, the PPML flows, the descriptives, and the
  other-outcome SDID.
- It was never in the ACS panels (not identified in the ACS microdata), and it
  remains inside the Kansas state totals and in other counties' aggregate in-
  and out-migration counts. We drop it as a unit, not its movers.

## Why

In 2016 Haskell's AGI base (the AGI of non-movers plus all movers, the
denominator of every AGI migration rate) is negative, about -$2.0 million, so
its 2016 AGI out-rate is -302% and its in-rate -175%. Every other county-year
in the panel has a positive base (the next-smallest relative to its own county
median is about half). No reported table or figure showed the problem: the
descriptives use 2018 onward, and Figure B3 trims changes beyond ±200%. It
entered only through the SDID pre-period (2016–2020). When the placebo
inference drew Haskell as the placebo-treated county, its placebo effect was
-361 pp, and the standard error for one cell (IRS, all counties, covariates,
2020 included, AGI out-rate) rose from 0.55 to 36.0.

## Proposed footnote (data section, where the IRS county data are introduced)

> We exclude Haskell County, Kansas, from all IRS county analyses. In 2016 the
> total AGI of its filers is negative, so its AGI-based migration rates for
> that year have a negative denominator and are not interpretable. Dropping it
> removes one of roughly 2,900 donor counties and does not change our point
> estimates materially.

The numbers in this note are for you, not for the paper. The footnote quotes
none of them so that nothing new needs a `\pn{}` key. If you want to quote the
-302% or the -$2.0 million, tell me and I will register them. The pipeline no
longer loads that county-year, so this would mean logging the values in `01f`
before the drop.

## Check after the new run lands

- Any all-county IRS count written by hand in the .tex drops by one: the
  SDID tables' "Number of Counties" goes 2,916 → 2,915 (incl. Multnomah),
  i.e. 2,915 → 2,914 donors. Table rows and `\pn{}` keys
  update on their own.
- The "does not change our point estimates materially" clause depends on the
  new run. Compare Table 2 / the preferred SDID estimates against run
  30912814 before you keep it.
