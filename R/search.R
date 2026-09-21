#' Search across AOP-Wiki entities
#'
#' Free-text search over AOPs, Key Events, KERs, stressors and chemicals at once.
#'
#' @param q Search term (1 to 200 characters).
#' @param types Restrict to certain entity types, e.g. "aop" or "key_event".
#' @param limit Maximum number of results.
#' @return A tibble of matches, with a `type` column and a relevance `score`.
#' @export
#' @examples
#' \dontrun{
#' aop_search("liver")
#' }
aop_search <- function(q, types = NULL, limit = 100) {
  check_string(q, "q")
  res <- aop_request("search", list(q = q, types = types, limit = limit))
  as_table(res$data)
}

#' Dataset information
#'
#' Version, license, data dumps and entity counts of the AOP-Wiki RDF dataset.
#'
#' @return A tibble with one row.
#' @export
#' @examples
#' \dontrun{
#' aop_dataset_info()
#' }
aop_dataset_info <- function() {
  as_row(aop_request("dataset")$data)
}
