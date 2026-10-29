# =============================================================================
# audit_paper_artifacts.R
#
# Cross-check the paper-artifact registry against three sources:
#   1. resources/paper_manifest.csv     (what the paper SHOULD include)
#   2. The paper and slide-deck .tex     (what they ACTUALLY cite; rows with
#                                         location = slides are checked
#                                         against the deck, the rest against
#                                         the paper)
#   3. results/                          (what the pipeline produced)
#
# Reports three statuses per artifact:
#   - OK         in manifest, on disk, cited by .tex
#   - MISSING    in manifest but not on disk (build break)
#   - DEAD       on disk but not in manifest and not cited by .tex
#   - DRIFT      cited by .tex but not in manifest (or vice versa)
#
# Writes dated CSV reports under results/_runs/ and prints a summary.
# Run standalone:  Rscript code/R/audit_paper_artifacts.R
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
})

# ---- Paths ------------------------------------------------------------------
# Resolve repo root from script location or fall back to getwd().
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, mustWork = TRUE)),
  error = function(e) NULL
)
repo_root <- if (!is.null(script_dir)) normalizePath(file.path(script_dir, "..", "..")) else getwd()

manifest_path <- file.path(repo_root, "resources", "paper_manifest.csv")
results_dir   <- file.path(repo_root, "results")

# Overleaf .tex files live outside the repo: OVERLEAF_TEX / OVERLEAF_SLIDES_TEX
# win; otherwise <Overleaf folder>/Conway_Iselin_Rork_2026{,_slides}.tex, with
# the folder taken from the pipeline's own resolver (code/R/utils.R:
# audit_tex_paths, resolve_oth_path), so the two cannot disagree.
#
# Without a reachable .tex (the cluster case, where oth_path is the
# results/overleaf_export/ staging folder) the audit runs in MANIFEST-ONLY
# mode: it checks manifest -> results/ and manifest -> the Overleaf mirror,
# and skips the disk -> manifest DEAD / DRIFT reports, which need the .tex to
# be trustworthy. AUDIT_REQUIRE_TEX=1 makes a missing .tex a hard stop.
source(file.path(repo_root, "code", "R", "utils.R"))
oth_path    <- resolve_oth_path(load_user_settings(repo_root), repo_root)
tex_paths   <- audit_tex_paths(oth_path)
tex_path    <- tex_paths[["paper"]]
tex_available <- nzchar(tex_path)
if (!tex_available && identical(Sys.getenv("AUDIT_REQUIRE_TEX", ""), "1")) {
  stop("Overleaf .tex not found. Set OVERLEAF_TEX, OVERLEAF_PATH, or oth_path ",
       "in user_settings.do / user_settings.R.", call. = FALSE)
}

stopifnot(file.exists(manifest_path))
stopifnot(dir.exists(results_dir))

today <- format(Sys.Date(), "%Y-%m-%d")

# ---- 1. Manifest ------------------------------------------------------------
manifest <- read_csv(manifest_path, show_col_types = FALSE)
stopifnot(all(c("artifact_basename", "paper_label", "paper_number",
                "location", "source_script") %in% names(manifest)))

# ---- 2. Disk walk -----------------------------------------------------------
disk_files <- list.files(
  results_dir,
  pattern = "\\.(pdf|png|tex)$",
  recursive = TRUE,
  full.names = TRUE
)
disk <- tibble(
  disk_path = disk_files,
  disk_basename = basename(disk_files),
  disk_kind = ifelse(grepl("\\.tex$", disk_files), "table", "figure")
)

# ---- 3. .tex parse ----------------------------------------------------------
parse_tex_includes <- function(tex_path) {
  if (!file.exists(tex_path)) return(tibble(tex_basename = character(),
                                             tex_line = integer()))
  lines <- readr::read_lines(tex_path)
  # Match \includegraphics[...]{path} OR \input{path}. Tolerate either
  # extension or no extension. Strip directory; keep basename.
  graphics_re <- "\\\\includegraphics\\s*(?:\\[[^\\]]*\\])?\\s*\\{([^}]+)\\}"
  input_re    <- "\\\\input\\s*\\{([^}]+)\\}"

  pull_matches <- function(re, kind_default_ext) {
    hits <- str_match_all(lines, re)
    rows <- imap_dfr(hits, function(m, ln) {
      if (nrow(m) == 0 || is.na(m[1, 2])) return(NULL)
      paths <- m[, 2]
      tibble(tex_line = ln,
             tex_basename = vapply(paths, function(p) {
               bn <- basename(p)
               # \input rarely carries .tex; add it for consistency
               if (kind_default_ext == ".tex" && !grepl("\\.", bn)) {
                 bn <- paste0(bn, kind_default_ext)
               }
               bn
             }, character(1)))
    })
    rows
  }

  bind_rows(
    pull_matches(graphics_re, ""),
    pull_matches(input_re, ".tex")
  )
}

# One row per reference, tagged with the document (paper / slides) it is in
tex_refs <- imap_dfr(tex_paths[nzchar(tex_paths)],
                     function(p, doc) parse_tex_includes(p) |> mutate(tex_doc = doc))
if (nrow(tex_refs) == 0) tex_refs <- tibble(tex_basename = character(), tex_line = integer(),
                                            tex_doc = character())
tex_available <- tex_available && any(tex_refs$tex_doc == "paper")
slides_available <- any(tex_refs$tex_doc == "slides")

# ---- 4. Reconcile -----------------------------------------------------------
# Manifest -> disk, and -> the Overleaf mirror when one is configured
mirror_files <- if (nzchar(oth_path)) basename(list.files(oth_path, recursive = TRUE)) else character()
paper_refs  <- tex_refs$tex_basename[tex_refs$tex_doc == "paper"]
slides_refs <- tex_refs$tex_basename[tex_refs$tex_doc == "slides"]
manifest_status <- manifest |>
  mutate(
    on_disk   = artifact_basename %in% disk$disk_basename,
    in_mirror = if (nzchar(oth_path)) artifact_basename %in% mirror_files else NA,
    # Checked against the document the row belongs to; NA when it is unreachable
    in_tex    = case_when(
      location == "slides" & slides_available ~ artifact_basename %in% slides_refs,
      location != "slides" & tex_available    ~ artifact_basename %in% paper_refs,
      TRUE                                    ~ NA
    ),
    status = case_when(
      !on_disk                            ~ "MISSING",
      nzchar(oth_path) & !in_mirror       ~ "MISSING_IN_OVERLEAF",
      !is.na(in_tex) & !in_tex            ~ "DRIFT_NOT_IN_TEX",
      TRUE                                ~ "OK"
    )
  )

# Disk -> manifest (find dead artifacts)
disk_status <- disk |>
  mutate(
    in_manifest = disk_basename %in% manifest$artifact_basename,
    in_tex      = if (tex_available) disk_basename %in% tex_refs$tex_basename
                  else NA,
    status = case_when(
      in_manifest                                                   ~ "REGISTERED",
      tex_available & in_tex & !in_manifest                         ~ "DRIFT_MISSING_REGISTRATION",
      TRUE                                                          ~ "DEAD"
    )
  )

# .tex -> manifest (find references with no registry entry)
tex_status <- if (tex_available) {
  tex_refs |>
    distinct(tex_doc, tex_basename, .keep_all = TRUE) |>
    mutate(
      in_manifest = tex_basename %in% manifest$artifact_basename,
      on_disk     = tex_basename %in% disk$disk_basename,
      status = case_when(
        in_manifest & on_disk      ~ "OK",
        in_manifest & !on_disk     ~ "MISSING_ON_DISK",
        !in_manifest & on_disk     ~ "DRIFT_MISSING_REGISTRATION",
        TRUE                       ~ "UNRESOLVED"
      )
    )
} else NULL

# ---- 5. Write reports -------------------------------------------------------
report_dir   <- file.path(results_dir, "_runs")
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
manifest_out <- file.path(report_dir,
                          sprintf("paper_manifest_audit_%s_manifest.csv", today))
disk_out     <- file.path(report_dir,
                          sprintf("paper_manifest_audit_%s_disk.csv", today))

write_csv(manifest_status, manifest_out)
if (tex_available) write_csv(disk_status, disk_out)
if (tex_available) {
  tex_out <- file.path(report_dir,
                       sprintf("paper_manifest_audit_%s_tex.csv", today))
  write_csv(tex_status, tex_out)
}

# ---- 6. Summary -------------------------------------------------------------
summarize_status <- function(df, col = "status") {
  df |> count(.data[[col]]) |> arrange(desc(n))
}

cat("\n==================================================\n")
cat("Paper-artifact audit\n")
cat("==================================================\n\n")

cat("Manifest -> disk:\n")
print(summarize_status(manifest_status), row.names = FALSE)
cat("\n")

if (tex_available) {
  cat("Disk -> manifest:\n")
  print(summarize_status(disk_status), row.names = FALSE)
  cat("\n")
}
if (nzchar(oth_path)) cat("Overleaf mirror checked: ", oth_path, "\n\n", sep = "")

if (tex_available) {
  cat("Tex -> manifest:\n")
  print(summarize_status(tex_status), row.names = FALSE)
  cat("\n")
} else {
  cat("Tex audit SKIPPED (paper .tex unreachable)\n\n")
}
if (tex_available && !slides_available) {
  cat("Slides .tex unreachable: location = slides rows not checked against the deck.\n\n")
}

cat("Reports written:\n")
cat("  ", manifest_out, "\n", sep = "")
if (tex_available) cat("  ", disk_out, "\n", sep = "")
if (tex_available) cat("  ", tex_out, "\n", sep = "")

# Hard fail if anything in the manifest is missing on disk or from the
# Overleaf mirror — that's a real build break. Drift warnings are exit 0.
n_missing <- sum(manifest_status$status %in% c("MISSING", "MISSING_IN_OVERLEAF"))
if (n_missing > 0) {
  cat("\nFAIL: ", n_missing,
      " manifest row(s) missing from results/ or the Overleaf mirror:\n", sep = "")
  print(manifest_status |> filter(status %in% c("MISSING", "MISSING_IN_OVERLEAF")) |>
          select(artifact_basename, status, source_script), n = Inf)
  quit(status = 1)
}

invisible(NULL)
