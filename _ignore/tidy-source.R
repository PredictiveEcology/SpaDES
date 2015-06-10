###
### use this script as part of a pre-commit hook to tidy up R source files
### e.g., add `Rscript -e 'source("~/Documents/GitHub/SpaDES/SAMPLE/tidy-source.R")'`
###       to `~/Documents/GitHub/SpaDES/.git/hooks/pre-commit`
###

options("formatR.arrow" = TRUE,
        "formatR.blank" = TRUE,
        "formatR.brace.newline" = FALSE,
        "formatR.comment" = TRUE,
        "formatR.indent" = 2)

path <- if (.os == "windows") { "~/GitHub/SpaDES/R" } else { "~/Documents/GitHub/SpaDES/R" }
files <- dir(path, pattern="[.][r|R]", full.names=TRUE)
lapply(files, function(f) {
  tidy_source(f, arrow=TRUE, file=f, indent=2, width=80)
})
