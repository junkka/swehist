#' Plot the administrative hierarchy
#'
#' Visualise the six administrative branches of Swedish historical
#' administrative units as a directed graph. Parish sits at the top as the
#' atomic unit; arrows flow downward toward larger containing units. Dotted
#' lines mark temporal transitions (1862 municipality reform, 1971 court
#' reform). Edge labels identify each branch.
#'
#' The six branches are:
#' \itemize{
#'   \item \strong{Civil}: Parish \eqn{\to} Municipality \eqn{\to} County
#'     (from 1862)
#'   \item \strong{Tax / Fiscal}: Parish \eqn{\to} Hundred \eqn{\to}
#'     Bailiwick \eqn{\to} County
#'   \item \strong{Judicial}: Parish \eqn{\to} Hundred \eqn{\to}
#'     Magistrates Court \eqn{\to} Court of Appeal
#'   \item \strong{Judicial (modern)}: Municipality \eqn{\to} District Court
#'     \eqn{\to} Court of Appeal (from 1971)
#'   \item \strong{Ecclesiastical}: Parish \eqn{\to} Pastorship \eqn{\to}
#'     Contract \eqn{\to} Diocese
#'   \item \strong{Military}: Parish \eqn{\to} Regiment (no polygon geometry)
#' }
#'
#' @param highlight character vector of type name(s) to highlight. Use the
#'   same names as the \code{type} argument of \code{\link{get_boundaries}},
#'   plus \code{"regiment"}. Default \code{NULL} means no highlighting.
#' @param label_size numeric, font size for node labels (default \code{3.5}).
#' @param title optional plot title (default \code{NULL} = no title).
#'
#' @return A \code{ggplot2} object. Add further ggplot2 layers or themes as
#'   needed.
#'
#' Internal: used by the vignettes via \code{swehist:::plot_hierarchy()}.
#' @noRd
plot_hierarchy <- function(highlight = NULL,
                           label_size = 3.5,
                           title = NULL){

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 is required for plot_hierarchy(). ",
         "Install it with install.packages('ggplot2').")

  valid_types <- c(
    "county", "municipality", "bailiwick", "hundred",
    "magistrates_court", "district_court", "court_of_appeal",
    "diocese", "contract", "pastorship", "parish", "regiment"
  )

  if (!is.null(highlight)) {
    bad <- setdiff(highlight, valid_types)
    if (length(bad))
      stop("Unknown type(s): ", paste(bad, collapse = ", "),
           ". Valid: ", paste(valid_types, collapse = ", "))
  }

  # node positions, parish at top and the larger containers below:
  # 14 parish, 12 municipality/hundred/pastorship/regiment, 10 bailiwick/court/contract,
  # 8 county/district court/diocese, 6 court of appeal
  nodes <- data.frame(
    id = c(
      "parish",
      "municipality", "hundred", "pastorship", "regiment",
      "bailiwick", "magistrates_court", "contract",
      "county", "district_court", "diocese",
      "court_of_appeal"
    ),
    label = c(
      "Parish\n(Socken / F\u00f6rsamling)",
      "Municipality\n(Kommun)",
      "Hundred\n(H\u00e4rad)",
      "Pastorship\n(Pastorat)",
      "Regiment*",
      "Bailiwick\n(F\u00f6gderi)",
      "Magistrates\nCourt\n(Domsaga)",
      "Contract\n(Kontrakt)",
      "County\n(L\u00e4n)",
      "District\nCourt\n(Tingsr\u00e4tt)",
      "Diocese\n(Stift)",
      "Court of\nAppeal\n(Hovr\u00e4tt)"
    ),
    x = c(
      5.0,
      1.5, 5.0, 8.5, 11.5,
      3.5, 6.5, 8.5,
      1.5, 4.0, 8.5,
      5.0
    ),
    y = c(
      14,
      12, 12, 12, 12,
      10, 10, 10,
      8, 8, 8,
      6
    ),
    stringsAsFactors = FALSE
  )

  nodes$highlighted <- nodes$id %in% highlight

  # edges, child to containing parent
  edges <- data.frame(
    from = c(
      "parish", "parish", "parish", "parish",
      "hundred", "hundred",
      "pastorship",
      "municipality", "bailiwick",
      "contract",
      "magistrates_court",
      "municipality", "district_court"
    ),
    to = c(
      "municipality", "hundred", "pastorship", "regiment",
      "bailiwick", "magistrates_court",
      "contract",
      "county", "county",
      "diocese",
      "court_of_appeal",
      "district_court", "court_of_appeal"
    ),
    ltype = c(
      "dotted", "solid", "solid", "dashed",
      "solid", "solid",
      "solid",
      "solid", "solid",
      "solid",
      "solid",
      "dotted", "solid"
    ),
    stringsAsFactors = FALSE
  )

  # Join node coordinates onto edges
  edges <- merge(edges, nodes[, c("id", "x", "y")],
                 by.x = "from", by.y = "id", all.x = TRUE)
  names(edges)[names(edges) == "x"] <- "x1"
  names(edges)[names(edges) == "y"] <- "y1"
  edges <- merge(edges, nodes[, c("id", "x", "y")],
                 by.x = "to", by.y = "id", all.x = TRUE)
  names(edges)[names(edges) == "x"] <- "x2"
  names(edges)[names(edges) == "y"] <- "y2"

  # Shorten each edge so arrows land outside labels
  off <- 0.65
  dx  <- edges$x2 - edges$x1
  dy  <- edges$y2 - edges$y1
  len <- sqrt(dx^2 + dy^2)
  edges$x1s <- edges$x1 + off * dx / len
  edges$y1s <- edges$y1 + off * dy / len
  edges$x2s <- edges$x2 - off * dx / len
  edges$y2s <- edges$y2 - off * dy / len

  # edge labels, positioned by hand
  edge_labels <- data.frame(
    x     = c(2.8,  9.5,  9.5,  3.8,  6.3,  0.5,  2.2),
    y     = c(13.3, 11.0, 13.3, 11.2, 11.2, 10.0, 10.6),
    label = c(
      "1862", "Ecclesiastical", "Military",
      "Tax",  "Judicial",       "Civil", "1971"
    ),
    stringsAsFactors = FALSE
  )

  # build the plot
  arr <- ggplot2::arrow(length = ggplot2::unit(0.18, "cm"), type = "closed")

  p <- ggplot2::ggplot()

  # --- Draw edges by line type ---
  for (lt in c("solid", "dotted", "dashed")) {
    sub <- edges[edges$ltype == lt, ]
    if (nrow(sub) > 0) {
      p <- p + ggplot2::geom_segment(
        data    = sub,
        ggplot2::aes(x = x1s, y = y1s, xend = x2s, yend = y2s),
        linewidth = 0.45,
        linetype  = lt,
        arrow   = arr,
        lineend = "round",
        colour  = "black"
      )
    }
  }

  # --- Edge labels (italic, slightly muted) ---
  p <- p + ggplot2::geom_text(
    data = edge_labels,
    ggplot2::aes(x = x, y = y, label = label),
    size     = label_size * 0.75,
    fontface = "italic",
    colour   = "grey30"
  )

  # --- Node labels ---
  regular <- nodes[!nodes$highlighted, ]
  if (nrow(regular) > 0) {
    p <- p + ggplot2::geom_label(
      data = regular,
      ggplot2::aes(x = x, y = y, label = label),
      fill          = "white",
      colour        = "black",
      linewidth     = 0.3,
      size          = label_size,
      label.padding = ggplot2::unit(0.35, "lines"),
      label.r       = ggplot2::unit(0, "lines")
    )
  }

  hilite <- nodes[nodes$highlighted, ]
  if (nrow(hilite) > 0) {
    p <- p + ggplot2::geom_label(
      data = hilite,
      ggplot2::aes(x = x, y = y, label = label),
      fill          = "#e0e0e0",
      colour        = "black",
      linewidth     = 0.8,
      fontface      = "bold",
      size          = label_size,
      label.padding = ggplot2::unit(0.35, "lines"),
      label.r       = ggplot2::unit(0, "lines")
    )
  }

  # --- Footnote for Regiment ---
  p <- p + ggplot2::annotate(
    "text", x = 0, y = 5.3,
    label = "* Regiment has no polygon geometry in the dataset",
    size = 2.5, hjust = 0, colour = "grey40", fontface = "italic"
  )

  p <- p +
    ggplot2::coord_cartesian(
      xlim = c(-0.5, 13),
      ylim = c(5, 15)
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(8, 8, 8, 8),
      plot.title  = ggplot2::element_text(
        face = "bold", size = 12, hjust = 0.5,
        margin = ggplot2::margin(b = 6)
      )
    )

  if (!is.null(title))
    p <- p + ggplot2::labs(title = title)

  p
}
