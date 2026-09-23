#' Key Event Components of a Key Event
#'
#' Key Event Components describe a Key Event in ontology terms: a biological
#' process, an optional biological object, and the action that links them. The
#' API returns them nested inside a single Key Event record, and this function
#' returns them as one row per component.
#'
#' @param ke_id Key Event identifier.
#' @return A tibble with the columns `ke_id`, `action`, `process`, `process_id`,
#'   `object` and `object_id`. The identifiers are ontology terms, for example
#'   `"GO:0008219"`. Components that name no object have `NA` in `object` and
#'   `object_id`.
#' @export
#' @examples
#' \dontrun{
#' ke_components(55)
#' }
ke_components <- function(ke_id) {
  check_id(ke_id, "ke_id")

  empty <- tibble::tibble(
    ke_id = integer(), action = character(),
    process = character(), process_id = character(),
    object = character(), object_id = character()
  )

  res <- aop_request(paste0("key-events/", ke_id))
  be <- res$data$biological_events
  if (is.null(be) || length(be) == 0) return(empty)

  # The response arrives either as a data frame with nested columns or as a
  # plain list, depending on the record. Normalise both to a list of rows.
  rows <- if (is.data.frame(be)) {
    lapply(seq_len(nrow(be)), function(i) {
      lapply(be, function(col) {
        if (is.data.frame(col)) as.list(col[i, , drop = FALSE]) else col[[i]]
      })
    })
  } else {
    be
  }

  pick <- function(row, part, key) {
    v <- row[[part]]
    if (is.null(v) || length(v) == 0 || !is.list(v)) return(NA_character_)
    val <- v[[key]]
    if (is.null(val) || length(val) == 0) return(NA_character_)
    as.character(val[[1]])
  }

  out <- lapply(rows, function(row) {
    action <- row[["action"]]
    tibble::tibble(
      ke_id      = as.integer(ke_id),
      action     = if (is.null(action) || length(action) == 0) NA_character_
                   else as.character(action[[1]]),
      process    = pick(row, "process", "title"),
      process_id = pick(row, "process", "id"),
      object     = pick(row, "object", "title"),
      object_id  = pick(row, "object", "id")
    )
  })

  tibble::as_tibble(vctrs::vec_rbind(!!!out))
}

#' Key Event Components of every Key Event in an AOP
#'
#' Retrieves the Key Event Components of each Key Event of an AOP and returns
#' them in a single table. One request is made per Key Event, so the first call
#' for a large AOP takes a few seconds. Responses are cached for the session.
#'
#' @param aop_id AOP identifier.
#' @return A tibble as returned by [ke_components()], with the columns
#'   `ke_title` and `role` added.
#' @export
#' @examples
#' \dontrun{
#' aop_components(38)
#' }
aop_components <- function(aop_id) {
  check_id(aop_id, "aop_id")

  kes <- aop_key_events(aop_id)
  if (nrow(kes) == 0) return(ke_components(1)[0, ])

  parts <- lapply(kes$id, ke_components)
  out <- tibble::as_tibble(vctrs::vec_rbind(!!!parts))
  if (nrow(out) == 0) return(out)

  merge(
    out,
    data.frame(ke_id = kes$id, ke_title = kes$title, role = kes$role,
               stringsAsFactors = FALSE),
    by = "ke_id"
  ) |>
    tibble::as_tibble()
}
