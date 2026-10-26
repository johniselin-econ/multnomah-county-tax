** user_settings.example.do — template for local, machine-specific settings.
**
** Copy this file to user_settings.do (gitignored) and edit. It is sourced by
** code/utils/globals.do if present, so its globals are available to every
** script (orchestrated or standalone).
**
** Overleaf sync: set oth_path to your local Overleaf folder to mirror figures
** and tables there. globals.do then sets ${overleaf}=1 and derives ${ol_fig} /
** ${ol_tab}; the R stages read the same line (code/R/utils.R), so this is the
** one place to set it. A relative path is taken from the repo root (the
** cluster staging convention is "results/overleaf_export/"). Leave
** user_settings.do absent (or oth_path unset) to disable sync.
** PAPER_ONLY=1 in the environment restricts the mirror to the artifacts in
** resources/paper_manifest.csv (Stata and R alike).

* global oth_path "C:/Users/<you>/Dropbox/Apps/Overleaf/Multnomah County/"
* global oth_path "results/overleaf_export/"
