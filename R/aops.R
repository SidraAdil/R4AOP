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
  aop_request_all(
    "aops",
    list(q = q, status = status, taxon = taxon, ke = ke, mie = mie,
         ao = ao, stressor = stressor, chemical = chemical,
         modified_since = modified_since)
  )
}

#' Get a single Adverse Outcome Pathway
#'
#' @param aop_id AOP identifier, e.g. `1`.
#' @param include Optional extra content to request from the API.
#' @return A tibble with one row.
#' @export
#' @examples
#' \dontrun{
#' aop_get(1)
#' }
aop_get <- function(aop_id, include = NULL) {
  check_id(aop_id, "aop_id")
  res <- aop_request(paste0("aops/", aop_id), list(include = include))
  as_row(res$data)
}

#' Key Events of an AOP
#'
#' @param aop_id AOP identifier.
#' @param role Optional filter: one of "mie", "ke" or "ao".
#' @return A tibble of Key Events.
#' @export
#' @examples
#' \dontrun{
#' aop_key_events(1)
#' aop_key_events(1, role = "mie")
#' }
aop_key_events <- function(aop_id, role = NULL) {
  check_id(aop_id, "aop_id")
  check_choice(role, c("mie", "ke", "ao"), "role")
  res <- aop_request(paste0("aops/", aop_id, "/key-events"), list(role = role))
  as_table(res$data)
}

#' Key Event Relationships of an AOP
#'
#' @param aop_id AOP identifier.
#' @return A tibble of KERs.
#' @export
#' @examples
#' \dontrun{
#' aop_kers(1)
#' }
aop_kers <- function(aop_id) {
  check_id(aop_id, "aop_id")
  as_table(aop_request(paste0("aops/", aop_id, "/kers"))$data)
}

#' Stressors of an AOP
#'
#' @param aop_id AOP identifier.
#' @return A tibble of stressors.
#' @export
#' @examples
#' \dontrun{
#' aop_stressors(1)
#' }
aop_stressors <- function(aop_id) {
  check_id(aop_id, "aop_id")
  as_table(aop_request(paste0("aops/", aop_id, "/stressors"))$data)
}

#' Chemicals of an AOP
#'
#' @param aop_id AOP identifier.
#' @return A tibble of chemicals.
#' @export
#' @examples
#' \dontrun{
#' aop_chemicals(1)
#' }
aop_chemicals <- function(aop_id) {
  check_id(aop_id, "aop_id")
  as_table(aop_request(paste0("aops/", aop_id, "/chemicals"))$data)
}

#' Genes linked to an AOP
#'
#' @param aop_id AOP identifier.
#' @param method How genes were linked: one of "any", "regex", "ner" or "both".
#' @return A tibble of genes.
#' @export
#' @examples
#' \dontrun{
#' aop_genes(1)
#' }
aop_genes <- function(aop_id, method = NULL) {
  check_id(aop_id, "aop_id")
  check_choice(method, c("any", "regex", "ner", "both"), "method")
  res <- aop_request(paste0("aops/", aop_id, "/genes"), list(method = method))
  as_table(res$data)
}
