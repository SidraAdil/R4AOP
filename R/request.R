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
aop_request_impl <- function(path, query = list()) {
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

# Memoised request function, filled in by .onLoad()
.aop_memo <- new.env(parent = emptyenv())

#' Perform a request, using the session cache when enabled
#'
#' @keywords internal
#' @noRd
aop_request <- function(path, query = list()) {
  if (isTRUE(getOption("R4AOP.cache", TRUE)) && !is.null(.aop_memo$request)) {
    .aop_memo$request(path, query)
  } else {
    aop_request_impl(path, query)
  }
}

#' Clear the R4AOP response cache
#'
#' Responses are cached for the current R session. Clear the cache to force
#' fresh requests, for example after the AOP-Wiki data has been updated.
#'
#' @return Invisibly TRUE.
#' @export
#' @examples
#' aop_cache_clear()
aop_cache_clear <- function() {
  if (!is.null(.aop_memo$request)) memoise::forget(.aop_memo$request)
  invisible(TRUE)
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
aop_request_all <- function(path, query = list(), limit = 1000) {
  first <- aop_request(path, c(query, list(limit = limit, offset = 0)))

  total <- first$meta$total
  if (is.null(total) || total == 0) {
    out <- empty_like(path)
    attr(out, "dataset_version") <- first$meta$dataset_version
    return(out)
  }
  pages <- list(first$data)

  offset <- limit
  while (offset < total) {
    page <- aop_request(path, c(query, list(limit = limit, offset = offset)))
    pages[[length(pages) + 1]] <- page$data
    offset <- offset + limit
  }

  out <- tibble::as_tibble(vctrs::vec_rbind(!!!pages))
  attr(out, "dataset_version") <- first$meta$dataset_version
  out
}

#' Convert one API record into a one-row tibble
#'
#' @keywords internal
#' @noRd
as_row <- function(x) {
  cols <- lapply(x, function(v) {
    if (is.null(v)) NA
    else if (length(v) == 1 && !is.list(v)) v
    else list(v)          # keep arrays as a list-column
  })
  tibble::as_tibble(cols)
}

#' Convert an API "data" array into a tibble
#'
#' @keywords internal
#' @noRd
as_table <- function(x) {
  if (is.null(x) || length(x) == 0) return(tibble::tibble())
  tibble::as_tibble(x)
}

#' Build an empty tibble with the columns an endpoint returns
#'
#' @keywords internal
#' @noRd
empty_like <- function(path) {
  res <- try(aop_request(path, list(limit = 1)), silent = TRUE)
  failed <- inherits(res, "try-error")
  if (failed || is.null(res$data) || length(res$data) == 0) {
    return(tibble::tibble())
  }
  tibble::as_tibble(res$data)[0, ]
}

#' Request an endpoint and return a tibble, keeping columns when empty
#'
#' The version of the dataset is taken from the response and attached to the
#' result, so that every table reports the data it came from (TR-07).
#'
#' @keywords internal
#' @noRd
fetch_table <- function(path, query = list()) {
  res <- aop_request(path, query)
  out <- as_table(res$data)
  if (ncol(out) == 0) {
    proto <- recall_proto(path)
    out <- if (!is.null(proto)) proto else empty_like(path)
  } else {
    remember_proto(path, out)
  }
  attr(out, "dataset_version") <- res$meta$dataset_version
  out
}

#' Request a single record and return a one-row tibble with its version
#'
#' @keywords internal
#' @noRd
fetch_row <- function(path, query = list()) {
  res <- aop_request(path, query)
  out <- as_row(res$data)
  attr(out, "dataset_version") <- res$meta$dataset_version
  out
}

# Remembered column prototypes, keyed by endpoint shape
.aop_protos <- new.env(parent = emptyenv())

path_template <- function(path) gsub("[0-9]+", "{id}", path)

remember_proto <- function(path, tbl) {
  if (ncol(tbl) > 0) assign(path_template(path), tbl[0, ], envir = .aop_protos)
  invisible(NULL)
}

recall_proto <- function(path) {
  key <- path_template(path)
  if (exists(key, envir = .aop_protos, inherits = FALSE)) {
    get(key, envir = .aop_protos)
  } else {
    NULL
  }
}
