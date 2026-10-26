**To:** Karen Conway, Jon Rork
**Subject:** Multnomah: updated results from the full rerun, and what changed in the draft

Karen, Jon,

The full pipeline finished on the Yale cluster this week, including the 500-draw bootstrap, and I have pushed the new tables and figures to Overleaf. Along the way I found and fixed a set of bugs in the code, some of which move the numbers. This note explains what changed, why, and what is left. The headline findings hold. The main SDID point estimates are essentially unchanged, and the story is the same: large, significant increases in out-migration concentrated among college-educated and high-income households, with weaker evidence on in-migration.

What moved, and why

The biggest change is in the stock elasticities. The event-study routine was mapping post-treatment years in reverse order, so for the ACS specifications the 2-year stock elasticity was summing the 2023 and 2024 effects instead of 2021 and 2022. With that fixed, the Table 2 range is now 1.0 to 2.6 rather than 1.5 to 2.8. The ACS College all-counties stock elasticity falls from 1.94 to 1.72, and the IRS narrow-pool estimate from 1.50 to 1.04. The semi-elasticities barely move (0.7 to 2.0).

All of the bootstrap intervals changed. Each draw now resamples donors within the specification's own pool, with Multnomah fixed, which is what Appendix B already said we did. Before, draws came from all 2,800 counties and were then restricted to the pool, so the pool size itself was random. The narrow and stringency intervals move the most.

The PPML flow models now include the zero flows that the tsfill step was supposed to add. A mover flag was built before the zero rows existed, so those rows were dropped from every regression. Observations rise from 282,836 to 401,718. The destination effect strengthens a little (from -0.203 to -0.227), and the origin effect in the ACS-county columns is now significant at 10 percent. The event-study figures look almost identical.

The DiD by age is the one place the narrative changes. The age-specific regressions were absorbing only year and the demographic categories, so the College x Age and Post x Age terms were loading onto the triple interaction. With those absorbed, the 45-64 college-educated group is now the one with the clearest and most persistent increase in out-migration (4 to 5 points a year from 2022 on). The 25-44 group is positive but imprecise, and the 65+ group jumps in 2021-2022 and then fades. On in-migration, the 65+ effect is no longer negative, and nothing by age is significant once pooled. The main College x Post coefficient is unchanged, so the DiD-implied semi-elasticities (-0.65 and -0.46) stand. I rewrote the relevant paragraph in Section 5.3; please read it with the new Figure 8 panels B and D.

Smaller items. The COVID case and death series were being double-cumulated, which is why the urban COVID-match pool grows from 77 to 85 counties and the corresponding rows of Tables 1 and A1 move. The revenue loss is now signed (a net inflow is a gain, not a loss), and the CPI factor is corrected, so the PFA baseline is $192 million rather than $189 million and the headline loss is $17.6 million (9.4 percent) rather than $17.7 million (9.5 percent). The Oregon figure of $76.7 million is unchanged. Table A3 now excludes 2020 and counts county movers the same way the DiD does, so the descriptive migration rates there are lower than before. A property-tax bracket map was also off by one bracket, which slightly shifts the ACS specifications with covariates (the stringency-pool estimate goes from -5.5 to -5.8).

What I changed in the draft

Numbers in the abstract, the elasticity paragraph in 5.1.1, the DiD-by-age paragraphs in 5.3, the revenue section, and the conclusion. I also added a clause in Appendix B noting that the bootstrap holds the pool size fixed. Two sentences still carry old numbers and are flagged with TODO comments in the tex: the PPML placebo shares (5.1 percent and so on) and the center and interquartile range of the revenue specification curve. Both need a quick recomputation on the server, which I will do this week.

Two things I would like your eyes on. First, the new by-age out-migration figure shows a positive 2017 coefficient for the 45-64 group, so the pre-trend there is less clean than before. Second, several of the appendix tables and all of the DiD and PPML figures were regenerated, so a scan for anything that reads oddly would be welcome.

I will send the recompiled PDF once the two remaining numbers are in.

John
