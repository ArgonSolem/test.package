#' Generate (Possibly Grouped) Bar Plot
#'
#' This function generates a bar plot (univariate or grouped), displaying the count
#' and percentage of each category of a variable within a dataframe. If `.group`
#' is supplied, bars are dodged by group; otherwise it calls
#' `plot_bar_univariadas_test()` for a univariate plot.
#'
#' @param .base A raw dataframe or one processed by tablas_univariadas.
#' @param .var Variable to plot (if .base lacks pct column).
#' @param .group Optional grouping variable: each group will get its own set of bars.
#' @param .wt Optional weight variable for frequency counts.
#' @param .palette Optional fallback color palette (vector of hex codes).
#' @param .category_colors Optional named vector or list mapping levels to hex colors.
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
plot_bar_univariadas_grouped <- function(
    .base,
    .var,
    .group = NULL,
    .wt = NULL,
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
  #-- Tidy evaluation
  var_q   <- rlang::enquo(.var)
  group_q <- rlang::enquo(.group)
  wt_q    <- rlang::enquo(.wt)

  #-- Fallback to univariate version if group missing
  if (rlang::quo_is_missing(group_q)) {
    return(
      plot_bar_univariadas_test(
        .base = .base,
        .var = !!var_q,
        .palette = .palette,
        .category_colors = .category_colors,
        .digits = .digits,
        .plot_text_size = .plot_text_size,
        .plot_text_color = .plot_text_color,
        .legend_text_size = .legend_text_size,
        .gray_category = .gray_category,
        .wrap_length = .wrap_length,
        .text_threshold = .text_threshold,
        .omit_label = .omit_label,
        .horizontal = .horizontal
      )
    )
  }

  #-- Extract string names
  var_lab <- rlang::as_name(var_q)
  grp_lab <- rlang::as_name(group_q)

  #-- Build grouped frequency table
  df <- .base %>%
    dplyr::group_by(!!group_q) %>%
    tablas_univariadas(.base = ., .var = !!var_q, .wt = !!wt_q) %>%
    dplyr::ungroup()

  #-- Optionally wrap category labels
  if (!is.null(.wrap_length)) {
    lvls    <- levels(as.factor(df[[var_lab]]))
    wrapped <- stringr::str_wrap(lvls, width = .wrap_length)
    df[[var_lab]] <- factor(
      stringr::str_wrap(as.character(df[[var_lab]]), width = .wrap_length),
      levels = wrapped
    )
  }

  #-- Determine palette fallback
  levels_vec <- unique(df[[var_lab]])
  n_lvls     <- length(levels_vec)
  if (is.null(.palette)) {
    .palette <- if (n_lvls == 2) c("#AC0000", "#08305B") else
      RColorBrewer::brewer.pal(min(n_lvls, 11), "RdBu")
  }

  #-- Build category_colors mapping
  if (!is.null(.category_colors)) {
    if (is.list(.category_colors)) {
      flat <- unlist(
        lapply(names(.category_colors), function(col) {
          lvls_in <- .category_colors[[col]]
          setNames(rep(col, length(lvls_in)), lvls_in)
        })
      )
      bad <- setdiff(names(flat), levels_vec)
      if (length(bad)) warning("Levels in .category_colors not in data: ", paste(bad, collapse = ", "))
      category_colors <- setNames(rep(NA_character_, n_lvls), levels_vec)
      overlap <- intersect(names(flat), levels_vec)
      category_colors[overlap] <- flat[overlap]
      fb <- setNames(.palette, levels_vec)
      category_colors[is.na(category_colors)] <- fb[is.na(category_colors)]
    } else {
      bad <- setdiff(names(.category_colors), levels_vec)
      if (length(bad)) warning("Levels in .category_colors not in data: ", paste(bad, collapse = ", "))
      category_colors <- setNames(rep(NA_character_, n_lvls), levels_vec)
      overlap <- intersect(names(.category_colors), levels_vec)
      category_colors[overlap] <- .category_colors[overlap]
      fb <- setNames(.palette, levels_vec)
      category_colors[is.na(category_colors)] <- fb[is.na(category_colors)]
    }
  } else {
    category_colors <- setNames(.palette, levels_vec)
  }

  #-- Gray out specific categories
  if (!is.null(.gray_category)) {
    gray_lvls <- if (!is.null(.wrap_length))
      stringr::str_wrap(.gray_category, width = .wrap_length)
    else .gray_category
    to_gray <- intersect(gray_lvls, levels_vec)
    if (length(to_gray)) {
      gr_shades <- scales::grey_pal(start = 0.8, end = 0.7)(length(to_gray))
      category_colors[to_gray] <- gr_shades
    }
  }

  #-- Filter for label display
  omit_vec <- .omit_label %||% character()
  if (!is.null(.wrap_length)) omit_vec <- stringr::str_wrap(omit_vec, width = .wrap_length)
  filtered <- df %>%
    dplyr::filter(
      !(.data[[var_lab]] %in% omit_vec),
      pct >= .text_threshold
    )

  #-- Build the plot
  p <- ggplot2::ggplot(df,
                       ggplot2::aes(
                         x    = .data[[grp_lab]],
                         y    = pct,
                         fill = .data[[var_lab]]
                       )) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
    ggplot2::scale_fill_manual(
      values = category_colors,
      guide  = ggplot2::guide_legend(position = "top")
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.2),
      breaks = seq(0, 1, 0.25),
      labels = scales::percent(seq(0, 1, 0.25), accuracy = 1)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text      = ggplot2::element_text(size = .legend_text_size),
      axis.title       = ggplot2::element_blank(),
      legend.title     = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    # Add text labels with dodge
    ggplot2::geom_text(
      data = filtered,
      ggplot2::aes(
        x     = .data[[grp_lab]],
        label = scales::percent(pct, accuracy = 1 / (10 ^ .digits)),
        group = .data[[var_lab]]
      ),
      position = ggplot2::position_dodge(width = 0.8),
      vjust    = if (.horizontal) 0.5 else -0.5,
      hjust    = if (.horizontal) -0.1 else NULL,
      size     = .plot_text_size,
      color    = rep_len(.plot_text_color, nrow(filtered))
    )

  #-- Horizontal flip if requested
  if (.horizontal) {
    p <- p +
      ggplot2::coord_flip() +
      ggplot2::scale_x_discrete(limits = rev(levels(df[[grp_lab]])))
  }

  p
}
