.onLoad <- function(libname, pkgname) {
  aop_request_cached <<- memoise::memoise(aop_request_impl)
}

# Column names used inside ggplot2::aes(), which R CMD check cannot see
utils::globalVariables(c("ke_title", "n_genes", "detected_by", "symbol"))
