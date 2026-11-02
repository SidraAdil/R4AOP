#' Order the Key Events of an AOP causally
#'
#' Topological sort of the Key Event Relationships, so that every upstream
#' event comes before the events it leads to. Ties are broken by role, with the
#' molecular initiating event first and the adverse outcome last, and then by
#' identifier, so the result is reproducible. Returns the row order, and the
#' attribute `branching` is `TRUE` when the pathway is not a single chain.
#'
#' @keywords internal
#' @noRd
order_key_events <- function(kes, kers) {
  ids <- kes$id
  rank <- match(kes$role, c("mie", "ke", "ao"))
  rank[is.na(rank)] <- 2L

  from <- integer(0)
  to <- integer(0)
  if (nrow(kers) > 0 && !is.null(kers$upstream) && !is.null(kers$downstream)) {
    from <- as.integer(kers$upstream$id)
    to <- as.integer(kers$downstream$id)
    keep <- from %in% ids & to %in% ids
    from <- from[keep]
    to <- to[keep]
  }

  indeg <- vapply(ids, function(i) sum(to == i), integer(1))
  remaining <- ids
  ordered <- integer(0)
  branching <- FALSE

  while (length(remaining) > 0) {
    ready <- remaining[indeg[match(remaining, ids)] == 0]
    if (length(ready) == 0) {
      # A cycle, which should not occur, but must not hang the function
      branching <- TRUE
      ready <- remaining
    }
    if (length(ready) > 1) branching <- TRUE

    i <- match(ready, ids)
    take <- ready[order(rank[i], ready)][1]

    ordered <- c(ordered, take)
    remaining <- remaining[remaining != take]

    drop <- from == take
    for (t in to[drop]) {
      j <- match(t, ids)
      indeg[j] <- indeg[j] - 1L
    }
    from <- from[!drop]
    to <- to[!drop]
  }

  out <- match(ordered, ids)
  attr(out, "branching") <- branching
  out
}

#' Readable labels for a taxon table
#'
#' @keywords internal
#' @noRd
taxon_labels <- function(taxa) {
  if (is.null(taxa) || !is.data.frame(taxa) || nrow(taxa) == 0) {
    return(character(0))
  }
  nm <- vapply(taxa$names, function(n) {
    n <- unlist(n)
    if (length(n) == 0) return(NA_character_)
    binomial <- grep("^[A-Z][a-z]+ [a-z]+$", n, value = TRUE)
    if (length(binomial) > 0) binomial[1] else n[1]
  }, character(1))
  paste0(nm, " (", taxa$id, ")")
}

#' Overview of an AOP
#'
#' Returns the Key Events of an AOP in causal order, from the molecular
#' initiating event to the adverse outcome, with the biological level of
#' organisation and the role of each event. The title of the AOP, its OECD
#' status, its taxonomic applicability, its stressors and the version of the
#' data are attached as attributes and shown when the result is printed.
#'
#' The order is computed from the Key Event Relationships, since the Key Events
#' themselves are not stored in sequence. An AOP that branches has no single
#' chain, so the order is then one valid sequence among several, and the
#' printed output says so.
#'
#' @param aop_id AOP identifier.
#' @return A tibble with the columns `step`, `ke_id`, `ke_title`, `level` and
#'   `role`, of class `aop_summary`.
#' @export
#' @examples
#' aop_summary(38)
aop_summary <- function(aop_id) {
  check_id(aop_id, "aop_id")

  kes <- aop_key_events(aop_id)
  if (nrow(kes) == 0) {
    cli::cli_abort("AOP {aop_id} has no Key Events.", call = NULL)
  }
  kers <- aop_kers(aop_id)
  info <- aop_get(aop_id)

  ord <- order_key_events(kes, kers)

  out <- tibble::tibble(
    step = seq_along(ord),
    ke_id = kes$id[ord],
    ke_title = kes$title[ord],
    level = if ("level" %in% names(kes)) kes$level[ord] else NA_character_,
    role = kes$role[ord]
  )

  attr(out, "aop_id") <- aop_id
  attr(out, "aop_title") <- info$title[1]
  attr(out, "oecd_status") <- info$oecd_status[1]
  attr(out, "taxa") <- taxon_labels(info$taxa[[1]])
  attr(out, "taxa_text") <- unlist(info$taxa_text[1])
  attr(out, "stressors") <- aop_stressors(aop_id)
  attr(out, "branching") <- isTRUE(attr(ord, "branching"))
  attr(out, "dataset_version") <- attr(kes, "dataset_version")

  class(out) <- c("aop_summary", class(out))
  out
}

#' @export
print.aop_summary <- function(x, ...) {
  stressors <- attr(x, "stressors")
  taxa <- attr(x, "taxa")

  line <- function(label, value) {
    if (length(value) == 0 || all(is.na(value))) value <- "not recorded"
    cat(format(paste0(label, ":"), width = 14), paste(value, collapse = "; "),
        "\n", sep = "")
  }

  cat("AOP ", attr(x, "aop_id"), ": ", attr(x, "aop_title"), "\n", sep = "")
  line("OECD status", attr(x, "oecd_status"))
  line("Taxa", taxa)
  taxa_text <- attr(x, "taxa_text")
  if (length(taxa_text) > 0 && !all(is.na(taxa_text))) {
    line("Taxa (text)", taxa_text)
  }
  line("Stressors", if (!is.null(stressors) && nrow(stressors) > 0) {
    stressors$title
  } else {
    character(0)
  })
  line("Data version", attr(x, "dataset_version"))
  if (isTRUE(attr(x, "branching"))) {
    cat("\nThe pathway branches, so this is one valid order among several.\n")
  }
  cat("\n")

  y <- x
  class(y) <- setdiff(class(y), "aop_summary")
  print(y, ...)
  invisible(x)
}
