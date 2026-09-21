#' List or search chemicals
#'
#' @param q Free-text search term.
#' @param aop Only chemicals used in this AOP id.
#' @param stressor Only chemicals linked to this stressor id.
#' @param inchikey,chebi,pubchem,chemspider,wikidata,chembl,drugbank,kegg,hmdb,lipidmaps
#'   Look the chemical up by an identifier in that database.
#' @return A tibble with one row per chemical.
#' @export
#' @examples
#' \dontrun{
#' chemical_list(q = "benzene")
#' }
chemical_list <- function(q = NULL, aop = NULL, stressor = NULL,
                          inchikey = NULL, chebi = NULL, pubchem = NULL,
                          chemspider = NULL, wikidata = NULL, chembl = NULL,
                          drugbank = NULL, kegg = NULL, hmdb = NULL,
                          lipidmaps = NULL) {
  aop_request_all(
    "chemicals",
    list(q = q, aop = aop, stressor = stressor, inchikey = inchikey,
         chebi = chebi, pubchem = pubchem, chemspider = chemspider,
         wikidata = wikidata, chembl = chembl, drugbank = drugbank,
         kegg = kegg, hmdb = hmdb, lipidmaps = lipidmaps)
  )
}

#' Get a single chemical
#'
#' @param cas CAS registry number, e.g. "71-43-2".
#' @return A tibble with one row.
#' @export
#' @examples
#' \dontrun{
#' chemical_get("71-43-2")
#' }
chemical_get <- function(cas) {
  check_string(cas, "cas")
  as_row(aop_request(paste0("chemicals/", cas))$data)
}

#' AOPs linked to a chemical
#'
#' @inheritParams chemical_get
#' @return A tibble of AOPs.
#' @export
#' @examples
#' \dontrun{
#' chemical_aops("71-43-2")
#' }
chemical_aops <- function(cas) {
  check_string(cas, "cas")
  fetch_table(paste0("chemicals/", cas, "/aops"))
}

#' Key Events linked to a chemical
#'
#' @inheritParams chemical_get
#' @param role Optional filter: one of "mie", "ke" or "ao".
#' @return A tibble of Key Events.
#' @export
#' @examples
#' \dontrun{
#' chemical_key_events("71-43-2")
#' }
chemical_key_events <- function(cas, role = NULL) {
  check_string(cas, "cas")
  check_choice(role, c("mie", "ke", "ao"), "role")
  fetch_table(paste0("chemicals/", cas, "/key-events"),
                       list(role = role))
}

#' WikiPathways containing a chemical
#'
#' @inheritParams chemical_get
#' @return A tibble of pathways.
#' @export
#' @examples
#' \dontrun{
#' chemical_pathways("71-43-2")
#' }
chemical_pathways <- function(cas) {
  check_string(cas, "cas")
  fetch_table(paste0("chemicals/", cas, "/pathways"))
}
