#' Generate Univariate Bar Plot
#'
#' This function generates a univariate bar plot, displaying the count
#' and percentage of each category of a variable within a dataframe.
#'
#' @param .base A raw dataframe or one processed by tablas_univariadas.
#' @param .var Optional variable to plot (if .base lacks pct column).
#' @param .palette Optional fallback color palette (vector of hex codes).
#' @param .category_colors Optional named character vector mapping levels to hex colors,
#'   or a list mapping hex colors to vectors of levels (for grouping multiple levels).
#' @param .digits Number of decimal digits for percent labels.
#' @param .plot_text_size Size of text displayed on bars.
#' @param .plot_text_color Color of text on bars.
#' @param .legend_text_size Size of text in legend.
#' @param .gray_category Optional categories to be filled with gray color.
#' @param .wrap_length Optional max width before wrapping category labels.
#' @param .text_threshold Percent threshold to display text labels.
#' @param .omit_label Categories to omit from label display.
#' @param .horizontal If TRUE, plot is horizontal (flipped axes).
#'
#' @return A ggplot2 bar plot with counts and percentages.
#' @export
plot_bar_univariadas <- function(
    .base,
    .var = NULL,
    .palette = NULL,
    .category_colors = NULL,
    .digits = 2,
    .plot_text_size = 6,
    .plot_text_color = "#252525",
    .legend_text_size = 16,
    .gray_category = NULL,
    .wrap_length = NULL,
    .text_threshold = 0.05,
    .omit_label = NULL,
    .horizontal = FALSE
) {
  # -- Type checks
  assertthat::assert_that(is.data.frame(.base), msg = ".base must be a data.frame.")
  assertthat::assert_that(is.numeric(.digits) && .digits >= 0 && .digits %% 1 == 0,
                          msg = ".digits must be a non-negative integer.")
  assertthat::assert_that(is.numeric(.plot_text_size) && .plot_text_size > 0,
                          msg = ".plot_text_size must be positive.")
  assertthat::assert_that(is.numeric(.legend_text_size) && .legend_text_size > 0,
                          msg = ".legend_text_size must be positive.")

  # -- Prepare data
  if (!"pct" %in% colnames(.base)) {
    if (rlang::quo_is_missing(rlang::enquo(.var))) {
      stop("If .base lacks pct, you must supply .var.")
    }
    .base <- tablas_univariadas(.base, {{ .var }})
    var_label <- rlang::as_name(rlang::enquo(.var))
  } else {
    var_label <- colnames(.base)[1]
  }

  # -- Wrap labels
  if (!is.null(.wrap_length)) {
    lvls <- levels(as.factor(.base[[var_label]]))
    wrapped <- stringr::str_wrap(lvls, width = .wrap_length)
    .base[[var_label]] <- factor(
      stringr::str_wrap(as.character(.base[[var_label]]), width = .wrap_length),
      levels = wrapped
    )
  }

  # -- Determine palette fallback
  levels_vec <- unique(.base[[var_label]])
  n_lvls <- length(levels_vec)
  if (is.null(.palette)) {
    .palette <- if (n_lvls == 2) c("#AC0000","#08305B")
    else RColorBrewer::brewer.pal(min(n_lvls,11), "RdBu")
  }

  # -- Build category_colors mapping with warnings for unmapped
  if (!is.null(.category_colors)) {
    if (is.list(.category_colors)) {
      flat <- unlist(
        lapply(names(.category_colors), function(col) {
          lvls_in <- .category_colors[[col]]
          setNames(rep(col, length(lvls_in)), lvls_in)
        })
      )
      bad <- setdiff(names(flat), levels_vec)
      if (length(bad)) warning("These levels in .category_colors not in data: ", paste(bad, collapse=", "))
      category_colors <- setNames(rep(NA_character_, n_lvls), levels_vec)
      overlap <- intersect(names(flat), levels_vec)
      category_colors[overlap] <- flat[overlap]
      unmapped <- levels_vec[is.na(category_colors)]
      if (length(unmapped)) warning("Unmapped levels, using fallback: ", paste(unmapped, collapse=", "))
      fb <- setNames(.palette, levels_vec)
      category_colors[is.na(category_colors)] <- fb[is.na(category_colors)]
    } else {
      assertthat::assert_that(is.character(.category_colors) && !is.null(names(.category_colors)),
                              msg = ".category_colors must be named character vector or list.")
      bad <- setdiff(names(.category_colors), levels_vec)
      if (length(bad)) warning("These levels in .category_colors not in data: ", paste(bad, collapse=", "))
      category_colors <- setNames(rep(NA_character_, n_lvls), levels_vec)
      overlap <- intersect(names(.category_colors), levels_vec)
      category_colors[overlap] <- .category_colors[overlap]
      unmapped <- levels_vec[is.na(category_colors)]
      if (length(unmapped)) warning("Unmapped levels, using fallback: ", paste(unmapped, collapse=", "))
      fb <- setNames(.palette, levels_vec)
      category_colors[is.na(category_colors)] <- fb[is.na(category_colors)]
    }
  } else {
    category_colors <- setNames(.palette, levels_vec)
  }

  # -- Gray out categories
  if (!is.null(.gray_category)) {
    gray_lvls <- if (!is.null(.wrap_length)) stringr::str_wrap(.gray_category, width = .wrap_length)
    else .gray_category
    to_gray <- intersect(gray_lvls, levels_vec)
    if (length(to_gray)) {
      gr_shades <- scales::grey_pal(start=0.8,end=0.7)(length(to_gray))
      category_colors[to_gray] <- gr_shades
    }
  }

  # -- Filter for text labels
  omit_vec <- .omit_label %||% character()
  if (!is.null(.wrap_length)) omit_vec <- stringr::str_wrap(omit_vec, width = .wrap_length)
  filtered <- dplyr::filter(.base,
                            !(.data[[var_label]] %in% omit_vec),
                            pct >= .text_threshold)

  # -- Build plot
  p <- ggplot2::ggplot(.base,
                       ggplot2::aes(x=.data[[var_label]], y=pct, fill=.data[[var_label]])) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values=category_colors) +
    ggplot2::scale_y_continuous(
      limits = c(0,1.2),
      breaks = seq(from = 0, to = 1, by = 0.25),
      labels = scales::percent(seq(from = 0, to = 1, by = 0.25), accuracy= 1)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size=.legend_text_size),
      axis.title=ggplot2::element_blank(),
      legend.title=ggplot2::element_blank(),
      panel.grid.minor=ggplot2::element_blank()
    )

  # -- Dynamic text placement based on orientation
  if (.horizontal) {
    p <- p + ggplot2::geom_text(
      data = filtered,
      ggplot2::aes(label = scales::percent(pct, accuracy = 1 / (10 ^ .digits))),
      vjust = 0.5,
      hjust = -0.1,
      size = .plot_text_size,
      color = rep_len(.plot_text_color, nrow(filtered))
    ) + ggplot2::coord_flip()

  } else {
    p <- p + ggplot2::geom_text(
      data = filtered,
      ggplot2::aes(label = scales::percent(pct, accuracy = 1 / (10 ^ .digits))),
      vjust = -0.5,
      size = .plot_text_size,
      color = rep_len(.plot_text_color, nrow(filtered))
    )
  }

  p
}
