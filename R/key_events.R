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


#' AOPs containing a Key Event
#'
#' @param ke_id Key Event identifier.
#' @param role Optional filter: one of "mie", "ke" or "ao".
#' @return A tibble of AOPs.
#' @export
#' @examples
#' \dontrun{
#' ke_aops(334)
#' }
ke_aops <- function(ke_id, role = NULL) {
  check_id(ke_id, "ke_id")
  check_choice(role, c("mie", "ke", "ao"), "role")
  fetch_table(paste0("key-events/", ke_id, "/aops"),
                       list(role = role))
}

#' Key Events downstream of a Key Event
#'
#' Follows Key Event Relationships forwards.
#'
#' @param ke_id Key Event identifier.
#' @param depth How many relationships to follow (1 to 3, default 1).
#' @return A tibble of Key Events.
#' @export
#' @examples
#' \dontrun{
#' ke_downstream(334, depth = 2)
#' }
ke_downstream <- function(ke_id, depth = 1) {
  check_id(ke_id, "ke_id")
  check_range(depth, 1, 3, "depth")
  fetch_table(paste0("key-events/", ke_id, "/downstream"),
                       list(depth = depth))
}

#' Key Events upstream of a Key Event
#'
#' Follows Key Event Relationships backwards.
#'
#' @inheritParams ke_downstream
#' @return A tibble of Key Events.
#' @export
#' @examples
#' \dontrun{
#' ke_upstream(334, depth = 2)
#' }
ke_upstream <- function(ke_id, depth = 1) {
  check_id(ke_id, "ke_id")
  check_range(depth, 1, 3, "depth")
  fetch_table(paste0("key-events/", ke_id, "/upstream"),
                       list(depth = depth))
}

#' Adverse Outcomes linked to a Key Event
#'
#' @param ke_id Key Event identifier.
#' @param as Which AOPs to consider: "mie" (only AOPs where this KE is the
#'   MIE) or "any" (every AOP containing it).
#' @return A tibble of Adverse Outcomes.
#' @export
#' @examples
#' \dontrun{
#' ke_adverse_outcomes(334)
#' }
ke_adverse_outcomes <- function(ke_id, as = NULL) {
  check_id(ke_id, "ke_id")
  check_choice(as, c("mie", "any"), "as")
  fetch_table(paste0("key-events/", ke_id, "/adverse-outcomes"),
                       list(as = as))
}

#' Molecular Initiating Events linked to a Key Event
#'
#' @param ke_id Key Event identifier.
#' @param as Which AOPs to consider: "ao" (only AOPs where this KE is the AO)
#'   or "any" (every AOP containing it).
#' @return A tibble of MIEs.
#' @export
#' @examples
#' \dontrun{
#' ke_mies(334)
#' }
ke_mies <- function(ke_id, as = NULL) {
  check_id(ke_id, "ke_id")
  check_choice(as, c("ao", "any"), "as")
  fetch_table(paste0("key-events/", ke_id,
                              "/molecular-initiating-events"),
                       list(as = as))
}

#' Stressors of a Key Event
#'
#' @param ke_id Key Event identifier.
#' @return A tibble of stressors.
#' @export
#' @examples
#' \dontrun{
#' ke_stressors(334)
#' }
ke_stressors <- function(ke_id) {
  check_id(ke_id, "ke_id")
  fetch_table(paste0("key-events/", ke_id, "/stressors"))
}

#' Chemicals of a Key Event
#'
#' @param ke_id Key Event identifier.
#' @return A tibble of chemicals.
#' @export
#' @examples
#' \dontrun{
#' ke_chemicals(334)
#' }
ke_chemicals <- function(ke_id) {
  check_id(ke_id, "ke_id")
  fetch_table(paste0("key-events/", ke_id, "/chemicals"))
}
