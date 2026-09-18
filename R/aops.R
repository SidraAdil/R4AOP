#' List Adverse Outcome Pathways
#'
#' Retrieve AOPs from the AOP-Wiki, optionally filtered. All filters are
#' optional; unused ones are not sent to the API.
#'
#' @param q Free-text search term, e.g. "liver".
#' @param status OECD status of the AOP.
#' @param taxon Taxon filter.
#' @param ke,mie,ao Identifier of a Key Event, Molecular Initiating Event
#'   or Adverse Outcome that the AOP must contain.
#' @param stressor Stressor identifier.
#' @param chemical Chemical identifier (CAS number).
#' @param modified_since Only AOPs modified after this date ("YYYY-MM-DD").
#' @return A tibble with one row per AOP.
#' @export
#' @examples
#' \dontrun{
#' aop_list(q = "liver")
#' }
aop_list <- function(q = NULL, status = NULL, taxon = NULL, ke = NULL,
                     mie = NULL, ao = NULL, stressor = NULL,
                     chemical = NULL, modified_since = NULL) {
  aop_get_all(
    "aops",
    list(q = q, status = status, taxon = taxon, ke = ke, mie = mie,
         ao = ao, stressor = stressor, chemical = chemical,
         modified_since = modified_since)
  )
}
