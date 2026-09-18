#' Base URL of the AOP-Wiki API
#'
#' @keywords internal
#' @noRd
aop_base_url <- function() {
  getOption("R4AOP.base_url",
            "https://aopwiki.rdf.bigcat-bioinformatics.org/api/v1")
}

#' Perform a single request to the AOP-Wiki API
#'
#' @param path Endpoint path, e.g. "aops" or "aops/1/key-events".
#' @param query Named list of query parameters. NULL values are dropped.
#' @return The parsed JSON response as a list.
#' @keywords internal
#' @noRd
aop_get <- function(path, query = list()) {
  query <- Filter(Negate(is.null), query)

  httr2::request(aop_base_url()) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("R4AOP (https://github.com/SidraAdil/R4AOP)") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)
}
