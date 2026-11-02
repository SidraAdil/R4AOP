# Precompute the vignette.
#
# The vignette queries the AOP-Wiki API. Running those queries during the
# Bioconductor build would make the package depend on the API being reachable,
# so the output is generated here instead and the finished vignette is shipped.
#
# Run this from the package root, after devtools::load_all(), whenever the
# vignette output should be refreshed. It needs an internet connection.
#
#   devtools::load_all()
#   source("vignettes/precompute.R")
#
# Commit the generated liver-fibrosis.Rmd together with vignettes/figures/.
#
# The working directory is changed explicitly rather than with on.exit(),
# because under source() on.exit() restores it before knit() runs.

local({
  root <- getwd()
  dir <- if (basename(root) == "vignettes") {
    root
  } else {
    file.path(root, "vignettes")
  }
  src <- file.path(dir, "liver-fibrosis.Rmd.orig")

  if (!file.exists(src)) {
    stop("Cannot find ", src,
         ". Run this from the package root, where DESCRIPTION lives.",
         call. = FALSE)
  }

  setwd(dir)
  knitr::knit("liver-fibrosis.Rmd.orig", output = "liver-fibrosis.Rmd")
  setwd(root)
})
