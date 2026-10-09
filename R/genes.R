#' List genes
#'
#' @param q Free-text search term, e.g. a gene symbol.
#' @param aop Only genes linked to this AOP id.
#' @param ke Only genes linked to this Key Event id.
#' @param method How genes were linked: "any", "regex", "ner" or "both".
#' @return A tibble with one row per gene.
#' @export
#' @examples
#' \dontrun{
#' gene_list(q = "TP53")
#' gene_list(aop = 37)
#' }
gene_list <- function(q = NULL, aop = NULL, ke = NULL, method = NULL) {
  check_choice(method, c("any", "regex", "ner", "both"), "method")
  aop_request_all("genes", list(q = q, aop = aop, ke = ke, method = method))
}

#' Get a single gene
#'
#' @param gene Gene symbol or HGNC identifier, e.g. "TP53".
#' @return A tibble with one row.
#' @export
#' @examples
#' \dontrun{
#' gene_get("TP53")
#' }
gene_get <- function(gene) {
  check_string(gene, "gene")
  fetch_row(paste0("genes/", gene))
}
