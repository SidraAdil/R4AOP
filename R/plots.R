# Colour used for the title of a Key Event, by its role in the AOP
role_colour <- function(role) {
  ifelse(role == "mie", "#1baf7a",
         ifelse(role == "ao", "#d1495b", NA_character_))
}

has_marquee <- function() requireNamespace("marquee", quietly = TRUE)

# Mark the molecular initiating event and the adverse outcome in a title.
#
# The role is written into the label, so the figure stays informative without
# any extra package.
role_label <- function(title, role) {
  suffix <- ifelse(role == "mie", " (MIE)",
                   ifelse(role == "ao", " (AO)", ""))
  paste0(title, suffix)
}

# Colour a label by role, using marquee's {#rrggbb text} span syntax.
#
# Each line is wrapped separately, because marquee does not parse a span that
# is broken across a line break, and the labels may already be wrapped.
colour_by_role <- function(label, role) {
  if (!has_marquee()) return(label)
  col <- role_colour(role)

  span <- function(text, colour) {
    if (is.na(colour)) return(text)
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    paste0("{", colour, " ", lines, "}", collapse = "\n")
  }
  vapply(seq_along(label), function(i) span(label[i], col[i]),
         character(1), USE.NAMES = FALSE)
}

# Axis text element that renders those spans, when marquee is available
role_axis_text <- function(...) {
  if (has_marquee()) {
    marquee::element_marquee(...)
  } else {
    ggplot2::element_text(...)
  }
}

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
#' Event without gene links appears with an empty bar. The molecular
#' initiating event and the adverse outcome are marked in the Key Event
#' titles, and coloured green and red when the marquee package is installed.
#' Requires the ggplot2 package.
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
  dat$ke_title <- colour_by_role(
    role_label(dat$ke_title, dat$role), dat$role
  )

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
    # theme_bw() rather than theme_minimal(), because a marquee axis element
    # is not honoured on top of theme_minimal()
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.border = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = role_axis_text(),
      axis.text.y = role_axis_text()
    )

  p
}
