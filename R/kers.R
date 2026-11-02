#' List Key Event Relationships
#'
#' @param aop Only KERs belonging to this AOP id.
#' @param upstream Only KERs starting at this Key Event id.
#' @param downstream Only KERs ending at this Key Event id.
#' @param ke Only KERs involving this Key Event id, in either position.
#' @param gene Only KERs linked to this gene.
#' @return A tibble with one row per KER.
#' @export
#' @examples
#' ker_list(aop = 1)
#' ker_list(ke = 334)
ker_list <- function(aop = NULL, upstream = NULL, downstream = NULL,
                     ke = NULL, gene = NULL) {
  aop_request_all(
    "kers",
    list(aop = aop, upstream = upstream, downstream = downstream,
         ke = ke, gene = gene)
  )
}

#' Get a single Key Event Relationship
#'
#' @param ker_id KER identifier.
#' @return A tibble with one row.
#' @export
#' @examples
#' ker_get(158)
ker_get <- function(ker_id) {
  check_id(ker_id, "ker_id")
  fetch_row(paste0("kers/", ker_id))
}
