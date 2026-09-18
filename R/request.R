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

  req <- httr2::request(aop_base_url()) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent("R4AOP (https://github.com/SidraAdil/R4AOP)") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort(
        c("Could not reach the AOP-Wiki API.",
          "i" = "Check your internet connection or try again later.",
          "x" = conditionMessage(e)),
        call = NULL
      )
    }
  )

  status <- httr2::resp_status(resp)

  if (status == 404) {
    cli::cli_abort("Not found: {.val {path}} does not exist in the AOP-Wiki.",
                   call = NULL)
  }
  if (status >= 500) {
    cli::cli_abort(
      c("The AOP-Wiki API is currently unavailable (HTTP {status}).",
        "i" = "This is a problem on the server side. Try again later."),
      call = NULL
    )
  }
  if (status >= 400) {
    cli::cli_abort("Request failed (HTTP {status}) for {.val {path}}.",
                   call = NULL)
  }

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}


######### TR-02 (pagination)

#' Fetch all pages of a list endpoint
#'
#' @param path Endpoint path, e.g. "key-events".
#' @param query Named list of filters.
#' @param limit Records per request (max 1000).
#' @return A tibble with all records.
#' @keywords internal
#' @noRd
aop_get_all <- function(path, query = list(), limit = 1000) {
  first <- aop_get(path, c(query, list(limit = limit, offset = 0)))

  total <- first$meta$total
  if (is.null(total) || total == 0) return(tibble::tibble())
  pages <- list(first$data)

  offset <- limit
  while (offset < total) {
    page <- aop_get(path, c(query, list(limit = limit, offset = offset)))
    pages[[length(pages) + 1]] <- page$data
    offset <- offset + limit
  }

  tibble::as_tibble(dplyr::bind_rows(pages))
}
