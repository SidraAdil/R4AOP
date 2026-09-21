#' List Key Events
#'
#' @param q Free-text search term.
#' @param role Optional filter: one of "mie", "ke" or "ao".
#' @param level Biological level of organisation.
#' @param aop Only Key Events belonging to this AOP id.
#' @param taxon,organ,cell_type Biological context filters.
#' @param gene Only Key Events linked to this gene.
#' @return A tibble with one row per Key Event.
#' @export
#' @examples
#' \dontrun{
#' ke_list(q = "apoptosis")
#' ke_list(aop = 1)
#' }
ke_list <- function(q = NULL, role = NULL, level = NULL, aop = NULL,
                    taxon = NULL, organ = NULL, cell_type = NULL,
                    gene = NULL) {
  check_choice(role, c("mie", "ke", "ao"), "role")
  aop_request_all(
    "key-events",
    list(q = q, role = role, level = level, aop = aop, taxon = taxon,
         organ = organ, cell_type = cell_type, gene = gene)
  )
}

#' List Molecular Initiating Events
#'
#' @inheritParams ke_list
#' @return A tibble with one row per MIE.
#' @export
#' @examples
#' \dontrun{
#' mie_list(q = "receptor")
#' }
mie_list <- function(q = NULL, level = NULL, aop = NULL, taxon = NULL,
                     organ = NULL, cell_type = NULL, gene = NULL) {
  aop_request_all(
    "mies",
    list(q = q, level = level, aop = aop, taxon = taxon,
         organ = organ, cell_type = cell_type, gene = gene)
  )
}

#' List Adverse Outcomes
#'
#' @inheritParams ke_list
#' @return A tibble with one row per Adverse Outcome.
#' @export
#' @examples
#' \dontrun{
#' ao_list(q = "carcinoma")
#' }
ao_list <- function(q = NULL, level = NULL, aop = NULL, taxon = NULL,
                    organ = NULL, cell_type = NULL, gene = NULL) {
  aop_request_all(
    "aos",
    list(q = q, level = level, aop = aop, taxon = taxon,
         organ = organ, cell_type = cell_type, gene = gene)
  )
}

#' Get a single Key Event
#'
#' @param ke_id Key Event identifier.
#' @return A tibble with one row.
#' @export
#' @examples
#' \dontrun{
#' ke_get(334)
#' }
ke_get <- function(ke_id) {
  check_id(ke_id, "ke_id")
  as_row(aop_request(paste0("key-events/", ke_id))$data)
}
