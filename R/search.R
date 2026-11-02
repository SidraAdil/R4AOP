#' Search across AOP-Wiki entities
#'
#' Free-text search over AOPs, Key Events, KERs, stressors and chemicals
#' at once.
#'
#' @param q Search term (1 to 200 characters).
#' @param types Restrict to certain entity types, e.g. "aop" or "key_event".
#' @param limit Maximum number of results.
#' @return A tibble of matches, with a `type` column and a relevance `score`.
#' @export
#' @examples
#' aop_search("liver")
aop_search <- function(q, types = NULL, limit = 100) {
  check_string(q, "q")
  fetch_table("search", list(q = q, types = types, limit = limit))
}

#' Dataset information
#'
#' Version, license, data dumps and entity counts of the AOP-Wiki RDF dataset.
#'
#' @return A tibble with one row.
#' @export
#' @examples
#' aop_dataset_info()
aop_dataset_info <- function() {
  fetch_row("dataset")
}

#' Data version of a result
#'
#' Reports which version of the AOP-Wiki dataset a result came from. Use it
#' when reporting results, so an analysis can be reproduced later.
#'
#' @param x A tibble returned by an R4AOP function. If missing, the current
#'   version is fetched from the API.
#' @return A version string, e.g. "2026.09.12".
#' @export
#' @examples
#' aops <- aop_list(q = "liver")
#' aop_data_version(aops)
#' aop_data_version()
aop_data_version <- function(x = NULL) {
  if (is.null(x)) return(aop_dataset_info()$version)
  v <- attr(x, "dataset_version")
  if (is.null(v)) {
    cli::cli_warn("No dataset version recorded for this result.")
    return(NA_character_)
  }
  v
}
