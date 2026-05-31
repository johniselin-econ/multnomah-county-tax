** user_settings.example.do — template for local, machine-specific settings.
**
** Copy this file to user_settings.do (gitignored) and edit. It is sourced by
** code/utils/globals.do if present, so its globals are available to every
** script (orchestrated or standalone).
**
** Overleaf sync: set oth_path to your local Overleaf folder to mirror figures
** and tables there. globals.do then sets ${overleaf}=1 and derives ${ol_fig} /
** ${ol_tab}. Leave user_settings.do absent (or oth_path unset) to disable sync.

* global oth_path "C:/Users/<you>/Dropbox/Apps/Overleaf/Multnomah County/"
