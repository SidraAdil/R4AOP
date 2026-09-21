#' @keywords internal
"_PACKAGE"
#' @section Options:
#' \describe{
#'   \item{`R4AOP.base_url`}{Base URL of the AOP-Wiki API. Defaults to
#'     `"https://aopwiki.rdf.bigcat-bioinformatics.org/api/v1"`. Change it to
#'     point the package at a different server or API version, e.g.
#'     `options(R4AOP.base_url = "https://example.org/api/v1")`.}
#' }
## usethis namespace: start
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL

#'   \item{`R4AOP.cache`}{If `TRUE` (the default), API responses are cached for
#'     the current session. Set to `FALSE` to always fetch fresh data, or call
#'     [aop_cache_clear()] to empty the cache.}
