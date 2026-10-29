# =============================================================================
# utils.R
# Shared utility functions for the multnomah-county-tax R code
#
# Functions:
#   read_api_key(api_codes_path, label) — Parse API key from CSV file
#   load_user_settings(project_root)    — user_settings.R into its own env
#   resolve_oth_path(settings, root)    — Overleaf folder (R or Stata settings)
#   audit_tex_paths(oth_path)           — paper + slides .tex the audits read
#   resolve_paper_only()                — PAPER_ONLY environment switch
#   load_paper_manifest(project_root)   — basenames in resources/paper_manifest.csv
#   overleaf_copy(cfg, files)           — the one Overleaf mirror for R outputs
# =============================================================================

#' Read an API key from a CSV file
#'
#' Searches column 1 for a row matching `label` (case-insensitive),
#' then returns the value in column 2 (stripped of quotes and whitespace).
#'
#' @param api_codes_path Path to the CSV file containing API keys
#' @param label Label to search for in column 1 (e.g., "ipums", "census")
#' @return Character string with the API key
read_api_key <- function(api_codes_path, label) {
  if (!file.exists(api_codes_path)) {
    stop("API codes file not found at: ", api_codes_path, call. = FALSE)
  }

  api_codes <- tryCatch(
    read.delim(api_codes_path, sep = ",", header = TRUE, stringsAsFactors = FALSE),
    error = function(e) {
      read.delim(api_codes_path, sep = ",", header = FALSE, stringsAsFactors = FALSE)
    }
  )

  key <- NA_character_
  if (ncol(api_codes) >= 2) {
    col1 <- tolower(trimws(as.character(api_codes[[1]])))
    idx  <- which(col1 == tolower(label))
    if (length(idx) >= 1) {
      key <- as.character(api_codes[idx[1], 2])
    }
  }

  # Strip quotes and trim whitespace
  key <- trimws(gsub('"', '', key))

  if (is.na(key) || key == "") {
    stop("Could not parse a '", label, "' API key from: ", api_codes_path, call. = FALSE)
  }

  key
}

#' Load user_settings.R (gitignored, optional) into its own environment
#'
#' One loader for every R entry point (multnomah_r_common.R and
#' audit_paper_artifacts.R), so the settings file is never sourced into a
#' script's own frame where its assignments could overwrite local objects.
#'
#' @param project_root Repo root
#' @return An environment holding whatever user_settings.R assigned (empty
#'   if the file is absent)
load_user_settings <- function(project_root) {
  settings <- new.env(parent = globalenv())
  path <- file.path(project_root, "user_settings.R")
  if (file.exists(path)) sys.source(path, envir = settings)
  settings
}

#' Resolve the Overleaf folder used for figure/table sync
#'
#' Precedence: the OVERLEAF_PATH environment variable, then `oth_path` from
#' user_settings.R, then `global oth_path "..."` in user_settings.do (so one
#' Stata-side setting serves both languages), else "" (sync off). A relative
#' path is taken from the repo root, matching globals.do.
#'
#' @param settings Environment from load_user_settings()
#' @param project_root Repo root (for relative paths and user_settings.do)
#' @return Character path (with trailing slash) or ""
resolve_oth_path <- function(settings, project_root = getwd()) {
  p <- Sys.getenv("OVERLEAF_PATH", "")
  if (!nzchar(p) && exists("oth_path", envir = settings, inherits = FALSE)) {
    q <- get("oth_path", envir = settings)
    if (is.character(q) && length(q) == 1 && nzchar(q)) p <- q
  }
  if (!nzchar(p)) {
    do_path <- file.path(project_root, "user_settings.do")
    if (file.exists(do_path)) {
      hit <- regmatches(readLines(do_path, warn = FALSE),
                        regexpr('^\\s*global\\s+oth_path\\s+"[^"]+"', readLines(do_path, warn = FALSE)))
      if (length(hit) >= 1) p <- sub('^\\s*global\\s+oth_path\\s+"([^"]+)"', "\\1", hit[1])
    }
  }
  if (!nzchar(p)) return("")
  p <- gsub("\\\\", "/", p)
  if (!grepl("^(/|[A-Za-z]:)", p)) p <- file.path(project_root, p)
  sub("/*$", "/", p)
}

#' Manuscript and slide-deck .tex files the audits read
#'
#' OVERLEAF_TEX / OVERLEAF_SLIDES_TEX win; otherwise both sit at the root of
#' the Overleaf folder. An entry is "" when its file is not reachable (the
#' cluster case, where oth_path is the results/overleaf_export/ staging folder).
#'
#' @param oth_path Overleaf folder from resolve_oth_path()
#' @return Named character vector: paper, slides
audit_tex_paths <- function(oth_path) {
  default <- function(f) if (nzchar(oth_path)) paste0(oth_path, f) else ""
  paths <- c(
    paper  = Sys.getenv("OVERLEAF_TEX",        unset = default("Conway_Iselin_Rork_2026.tex")),
    slides = Sys.getenv("OVERLEAF_SLIDES_TEX", unset = default("Conway_Iselin_Rork_2026_slides.tex"))
  )
  paths[!(nzchar(paths) & file.exists(paths))] <- ""
  paths
}

#' PAPER_ONLY switch: mirror only the paper's registered artifacts
#'
#' The same environment variable drives Stata (globals.do) and R, so one
#' setting on the launcher reaches both. "1" turns it on; anything else is off.
resolve_paper_only <- function() identical(Sys.getenv("PAPER_ONLY", ""), "1")

#' Basenames registered in resources/paper_manifest.csv
load_paper_manifest <- function(project_root) {
  path <- file.path(project_root, "resources", "paper_manifest.csv")
  if (!file.exists(path)) stop("Paper manifest not found: ", path, call. = FALSE)
  m <- read.csv(path, stringsAsFactors = FALSE)
  if (!"artifact_basename" %in% names(m)) stop("paper_manifest.csv needs an artifact_basename column", call. = FALSE)
  unique(m$artifact_basename)
}

#' Mirror R outputs to Overleaf (figures/ for images, tables/ for .tex)
#'
#' The R counterpart of Stata's project_save_overleaf: a no-op unless
#' cfg$overleaf is TRUE; with cfg$paper_only only manifest basenames are
#' copied; a missing source file is an error.
#'
#' @param cfg List from multnomah_r_init() (overleaf, dir_ol_fig, dir_ol_tab,
#'   paper_only, paper_artifacts)
#' @param files Character vector of source paths
#' @return Invisibly, the basenames copied
overleaf_copy <- function(cfg, files) {
  if (!isTRUE(cfg$overleaf)) return(invisible(character()))
  missing_src <- files[!file.exists(files)]
  if (length(missing_src) > 0) stop("Overleaf copy: source not found: ", paste(missing_src, collapse = ", "), call. = FALSE)
  if (isTRUE(cfg$paper_only)) files <- files[basename(files) %in% cfg$paper_artifacts]
  for (f in files) {
    dest_dir <- if (grepl("\\.tex$", f)) cfg$dir_ol_tab else cfg$dir_ol_fig
    file.copy(f, file.path(dest_dir, basename(f)), overwrite = TRUE)
  }
  invisible(basename(files))
}
