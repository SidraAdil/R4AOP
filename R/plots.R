#' Summarise the genes of an AOP per Key Event
#'
#' Counts the distinct genes linked to each Key Event of an AOP, separately for
#' each detection method (`regex`, `ner` or `both`).
#'
#' @param aop_id AOP identifier.
#' @return A tibble with the columns `ke_id`, `ke_title`, `role`,
#'   `detected_by` and `n_genes`.
#' @export
#' @examples
#' \dontrun{
#' aop_gene_summary(38)
#' }
aop_gene_summary <- function(aop_id) {
  check_id(aop_id, "aop_id")

  empty <- tibble::tibble(
    ke_id = integer(), ke_title = character(), role = character(),
    detected_by = character(), n_genes = integer()
  )

  genes <- aop_genes(aop_id)
  if (nrow(genes) == 0) return(empty)

  kes <- aop_key_events(aop_id)
  if (nrow(kes) == 0) return(empty)

  linked <- merge(
    genes[genes$entity_type == "key_event", , drop = FALSE],
    data.frame(entity_id = kes$id, ke_title = kes$title, role = kes$role,
               stringsAsFactors = FALSE),
    by = "entity_id"
  )
  if (nrow(linked) == 0) return(empty)

  out <- stats::aggregate(
    symbol ~ entity_id + ke_title + role + detected_by,
    data = linked,
    FUN = function(x) length(unique(x))
  )
  names(out)[names(out) == "symbol"] <- "n_genes"
  names(out)[names(out) == "entity_id"] <- "ke_id"

  tibble::as_tibble(out[order(out$ke_id, out$detected_by), ])
}

#' Plot the genes of an AOP per Key Event
#'
#' Draws a bar chart of the distinct genes linked to each Key Event of an AOP,
#' coloured by detection method. Requires the ggplot2 package.
#'
#' @param aop_id AOP identifier.
#' @return A ggplot object, which can be further modified in the usual way.
#' @export
#' @examples
#' \dontrun{
#' aop_plot_genes(38)
#' }
aop_plot_genes <- function(aop_id) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      c("The {.pkg ggplot2} package is needed for {.fn aop_plot_genes}.",
        "i" = 'Install it with install.packages("ggplot2").'),
      call = NULL
    )
  }

  dat <- aop_gene_summary(aop_id)
  if (nrow(dat) == 0) {
    cli::cli_abort("No genes are linked to the Key Events of AOP {aop_id}.",
                   call = NULL)
  }

  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = stats::reorder(ke_title, n_genes), y = n_genes,
                 fill = detected_by)
  ) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL, y = "Distinct genes", fill = "Detected by",
      title = paste0("Genes per Key Event, AOP ", aop_id)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}
