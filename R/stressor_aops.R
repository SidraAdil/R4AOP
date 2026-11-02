#' Resolve a stressor given as an identifier or a name
#'
#' @keywords internal
#' @noRd
resolve_stressor <- function(stressor) {
  if (is.character(stressor)) {
    hits <- stressor_list(q = stressor)
    if (nrow(hits) == 0) {
      cli::cli_abort("No stressor matched {.val {stressor}}.", call = NULL)
    }
    exact <- which(tolower(hits$title) == tolower(stressor))
    i <- if (length(exact) > 0) exact[1] else 1L
    if (nrow(hits) > 1 && length(exact) == 0) {
      cli::cli_inform(c(
        "i" = "{nrow(hits)} stressors matched {.val {stressor}}.",
        " " = "Using {.val {hits$title[i]}} (id {hits$id[i]}).",
        " " = "Call {.fn stressor_list} to see the others."
      ))
    }
    return(hits[i, , drop = FALSE])
  }

  check_id(stressor, "stressor")
  stressor_get(stressor)
}

#' AOPs linked to a stressor, with their adverse outcomes
#'
#' Lists every AOP in which a stressor is used, together with the adverse
#' outcomes those AOPs end in. The AOP identifiers come from the `aop_ids`
#' field of the stressor record, and the titles and adverse outcomes are taken
#' from the AOP list, which is cached for the session. The stressor may be
#' given as an identifier or as a name, in which case the name is looked up
#' with [stressor_list()].
#'
#' @param stressor Stressor identifier, or a stressor name.
#' @return A tibble with the columns `stressor_id`, `stressor_title`, `aop_id`,
#'   `aop_title`, `ao_id` and `ao_title`, with one row per AOP and adverse
#'   outcome. An AOP with no adverse outcome recorded keeps one row, with `NA`
#'   in the two `ao` columns. A stressor used in no AOP gives zero rows.
#' @export
#' @examples
#' stressor_aops(150)
#' stressor_aops("hexachlorobenzene")
stressor_aops <- function(stressor) {
  info <- resolve_stressor(stressor)

  empty <- tibble::tibble(
    stressor_id = integer(), stressor_title = character(),
    aop_id = integer(), aop_title = character(),
    ao_id = integer(), ao_title = character()
  )

  ids <- if ("aop_ids" %in% names(info)) unlist(info$aop_ids[1]) else integer(0)
  if (length(ids) == 0) {
    cli::cli_inform(
      "{.val {info$title[1]}} (id {info$id[1]}) is used in no AOP."
    )
    return(empty)
  }

  aops <- aop_list(resolve_titles = TRUE)
  aops <- aops[aops$id %in% ids, , drop = FALSE]
  if (nrow(aops) == 0) return(empty)

  if (!"ao_ids" %in% names(aops)) {
    aops$ao_ids <- replicate(nrow(aops), integer(0), simplify = FALSE)
    aops$ao_titles <- replicate(nrow(aops), character(0), simplify = FALSE)
  }

  # One row per AOP and adverse outcome, keeping AOPs that have none
  times <- pmax(lengths(aops$ao_ids), 1L)
  idx <- rep(seq_len(nrow(aops)), times)

  or_na <- function(x, empty_value) if (length(x) == 0) empty_value else x
  ao_id <- unlist(lapply(aops$ao_ids, or_na, NA_integer_), use.names = FALSE)
  ao_title <- unlist(lapply(aops$ao_titles, or_na, NA_character_),
                     use.names = FALSE)

  out <- tibble::tibble(
    stressor_id = info$id[1],
    stressor_title = info$title[1],
    aop_id = aops$id[idx],
    aop_title = aops$title[idx],
    ao_id = ao_id,
    ao_title = ao_title
  )

  attr(out, "dataset_version") <- attr(aops, "dataset_version")
  out[order(out$aop_id), ]
}
