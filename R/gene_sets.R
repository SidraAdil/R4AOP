#' Key Event gene sets of an AOP as a GeneSetCollection
#'
#' Returns the genes linked to each Key Event of an AOP as a
#' [GSEABase::GeneSetCollection], with one gene set per Key Event. This is the
#' form that Bioconductor enrichment tools expect, so the result can be passed
#' straight to functions such as `GSVA::gsva()` or `limma::camera()` without
#' reshaping.
#'
#' Each gene set is named `KE_<id>`, carries the Key Event title as its short
#' description, and records the Key Event role and the version of the AOP-Wiki
#' data in its long description. Key Events without gene links are omitted,
#' since an empty gene set is not useful for enrichment. Use
#' [aop_gene_summary()] to see which Key Events those are.
#'
#' @param aop_id AOP identifier.
#' @param method How genes were linked: one of "any", "regex", "ner" or "both".
#' @return A `GeneSetCollection` with one `GeneSet` per Key Event that has
#'   linked genes.
#' @seealso [aop_genes()] for the same information as a tibble.
#' @export
#' @examples
#' gsc <- ke_gene_sets(38)
#' names(gsc)
ke_gene_sets <- function(aop_id, method = NULL) {
  check_id(aop_id, "aop_id")
  check_choice(method, c("any", "regex", "ner", "both"), "method")

  genes <- aop_genes(aop_id, method = method)
  version <- attr(genes, "dataset_version")

  if (nrow(genes) > 0 && "entity_type" %in% names(genes)) {
    genes <- genes[genes$entity_type == "key_event", , drop = FALSE]
  }
  if (nrow(genes) == 0) {
    cli::cli_abort("No genes are linked to the Key Events of AOP {aop_id}.",
                   call = NULL)
  }

  kes <- aop_key_events(aop_id)
  by_ke <- split(genes$symbol, genes$entity_id)

  sets <- lapply(names(by_ke), function(id) {
    i <- match(as.integer(id), kes$id)
    title <- if (!is.na(i)) kes$title[i] else NA_character_
    role <- if (!is.na(i)) kes$role[i] else NA_character_

    GSEABase::GeneSet(
      unique(by_ke[[id]]),
      setName = paste0("KE_", id),
      geneIdType = GSEABase::SymbolIdentifier(),
      shortDescription = title,
      longDescription = paste0(
        "Key Event ", id, " (role: ", role, ") of AOP ", aop_id,
        ". AOP-Wiki dataset version: ", version, "."
      ),
      urls = paste0("https://aopwiki.org/events/", id)
    )
  })

  GSEABase::GeneSetCollection(sets)
}
