.onLoad <- function(libname, pkgname) {
  aop_request_cached <<- memoise::memoise(aop_request_impl)
}
