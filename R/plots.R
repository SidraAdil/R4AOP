# Colour used for the title of a Key Event, by its role in the AOP
role_colour <- function(role) {
  ifelse(role == "mie", "#1baf7a",
         ifelse(role == "ao", "#d1495b", "#3d3d3d"))
}

# Wrap a label in a coloured HTML span, for ggtext::element_markdown().
# The colour travels with the label, so it cannot be matched to the wrong axis
# break, and no vectorised element_text() is needed.
md_label <- function(text, colour) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("\n", "<br>", text, fixed = TRUE)
  paste0("<span style='color:", colour, ";'>", text, "</span>")
}

has_ggtext <- function() requireNamespace("ggtext", quietly = TRUE)

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
#' aop_gene_summary(38)
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

#' Add the Key Events of an AOP that carry no gene links
#'
#' They are given a count of zero, so that the gap stays visible in the plot
#' instead of the Key Event disappearing from it.
#'
#' @keywords internal
#' @noRd
add_gene_free_kes <- function(dat, aop_id) {
  kes <- aop_key_events(aop_id)
  gap <- kes[!kes$id %in% dat$ke_id, , drop = FALSE]
  if (nrow(gap) == 0) return(dat)

  vctrs::vec_rbind(
    dat,
    tibble::tibble(
      ke_id = gap$id, ke_title = gap$title, role = gap$role,
      detected_by = dat$detected_by[1], n_genes = 0L
    )
  )
}

#' Plot the genes of an AOP per Key Event
#'
#' Draws a bar chart of the distinct genes linked to each Key Event of an AOP,
#' coloured by detection method. Every Key Event of the AOP is shown, so a Key
#' Event without gene links appears with an empty bar. Key Event titles are
#' printed in green for the molecular initiating event and in red for the
#' adverse outcome. Requires the ggplot2 package.
#'
#' @param aop_id AOP identifier.
#' @return A ggplot object, which can be further modified in the usual way.
#' @export
#' @examples
#' aop_plot_genes(38)
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

  dat <- add_gene_free_kes(dat, aop_id)

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(x = stats::reorder(ke_title, n_genes), y = n_genes,
                 fill = detected_by)
  ) +
    ggplot2::geom_col(width = 0.65, colour = "grey45", linewidth = 0.2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(both = "#a8c9ef", ner = "#f6bda4", regex = "#9adcc3")
    ) +
    ggplot2::labs(
      x = NULL, y = "Distinct genes", fill = "Detected by",
      title = paste0("Genes per Key Event, AOP ", aop_id)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  # Colour the Key Event titles by role, if ggtext is installed
  if (has_ggtext()) {
    p <- p +
      ggplot2::scale_x_discrete(labels = function(x) {
        md_label(x, role_colour(dat$role[match(x, dat$ke_title)]))
      }) +
      ggplot2::theme(axis.text.y = ggtext::element_markdown())
  }

  p
}
