#' Plot the Key Event Components of an AOP
#'
#' Shows, for every Key Event of an AOP, the biological process, the biological
#' object and the action that describe it, together with their ontology
#' identifiers. Key Events are ordered from the molecular initiating event to
#' the adverse outcome, and their titles are printed in green for the molecular
#' initiating event and in red for the adverse outcome. Components that AOP-Wiki
#' does not record are left blank.
#'
#' @param aop_id AOP identifier.
#' @param wrap Width in characters at which long terms are wrapped.
#' @return A ggplot object.
#' @export
#' @examples
#' \dontrun{
#' aop_plot_components(38)
#' }
aop_plot_components <- function(aop_id, wrap = 26) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      c("The {.pkg ggplot2} package is needed for {.fn aop_plot_components}.",
        "i" = 'Install it with install.packages("ggplot2").'),
      call = NULL
    )
  }

  wrap_text <- function(x, width) {
    vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
           character(1), USE.NAMES = FALSE)
  }

  dat <- aop_components(aop_id)
  if (nrow(dat) == 0) {
    cli::cli_abort("No Key Event Components are recorded for AOP {aop_id}.",
                   call = NULL)
  }

  # A Key Event can have several components, so each gets its own row
  dat <- dat[order(match(dat$role, c("mie", "ke", "ao")), dat$ke_id), ]
  dat$comp <- stats::ave(seq_len(nrow(dat)), dat$ke_id, FUN = seq_along)

  part_of <- function(term, term_id, part) {
    data.frame(
      ke_id = dat$ke_id, ke_title = dat$ke_title, role = dat$role,
      comp = dat$comp, part = part, term = term, term_id = term_id,
      stringsAsFactors = FALSE
    )
  }

  long <- vctrs::vec_rbind(
    part_of(dat$process, dat$process_id, "Process"),
    part_of(dat$object,  dat$object_id,  "Object"),
    part_of(dat$action,  NA_character_,  "Action")
  )
  long <- long[!is.na(long$term), , drop = FALSE]

  long$label <- ifelse(
    is.na(long$term_id),
    wrap_text(long$term, wrap),
    paste0(wrap_text(long$term, wrap), "\n", long$term_id)
  )

  # Molecular initiating event at the top, adverse outcome at the bottom.
  # One row per component, with the Key Event named on its first row only.
  long$row_key <- paste(long$ke_id, long$comp, sep = "-")
  key <- order(match(long$role, c("ao", "ke", "mie")), -long$ke_id, -long$comp)
  row_levels <- unique(long$row_key[key])

  row_label <- stats::setNames(rep("", length(row_levels)), row_levels)
  named <- long[!duplicated(long$row_key), ]
  row_label[named$row_key] <- ifelse(
    named$comp == 1,
    paste0(wrap_text(named$ke_title, wrap + 8), " (KE ", named$ke_id, ")"),
    ""
  )

  # Colour the Key Event titles by role, if ggtext is installed
  if (has_ggtext()) {
    nm <- names(row_label)
    cols <- role_colour(long$role[match(nm, long$row_key)])
    row_label <- ifelse(nzchar(row_label), md_label(row_label, cols), "")
    names(row_label) <- nm
  }

  long$ke_lab <- factor(long$row_key, levels = row_levels)
  long$part <- factor(long$part, levels = c("Process", "Object", "Action"))

  ggplot2::ggplot(long, ggplot2::aes(x = part, y = ke_lab, fill = part)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.2,
                       width = 0.96, height = 0.92, alpha = 0.18) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 2.7,
                       lineheight = 0.95, colour = "#0b0b0b") +
    ggplot2::scale_fill_manual(
      values = c(Process = "#2a78d6", Object = "#eb6834", Action = "#1baf7a")
    ) +
    ggplot2::scale_x_discrete(position = "top", drop = FALSE) +
    ggplot2::scale_y_discrete(labels = row_label) +
    ggplot2::labs(
      x = NULL, y = NULL, fill = NULL,
      title = paste0("Key Event Components of AOP ", aop_id)
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(face = "bold"),
      axis.text.y = if (has_ggtext()) {
        ggtext::element_markdown(hjust = 1)
      } else {
        ggplot2::element_text(hjust = 1)
      }
    )
}
