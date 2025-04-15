#' Generate Univariate Donut Plot
#'
#' This function generates a univariate donut plot, displaying the count
#' and percentage of each category of a variable within a dataframe.
#' It supports optional color for plot and size for plot and legend text.
#'
#' @param .base A raw dataframe or processed one by tablas_univariadas.
#' @param .var Optional var to plot.
#' @param .palette Optional color palette.
#' @param .hsize Parameter that allows to modify the size of the donut.
#' @param .plot_text_size Parameter that allows to modify the size of the text inside the plot.
#' @param .plot_text_color Parameter that allows to modify the color of the text inside the plot.
#' @param .legend_text_size Parameter that allows to modify the size of the text inside the legend.
#' @param .gray_category Optional categories to be filled with gray color.
#' @param .wrap_length Optional parameter to define the maximum length of category labels before wrapping.
#' @param .text_threshold Optional parameter to define the threshold of the minimum percent to display.
#' @param .omit_label Optional parameter to define custom categories to omit percent to display.
#'
#' @return A plot with counts and percentages for each category of the selected variable.
#' @export
#'
#' @importFrom rlang as_name enquo
#' @importFrom ggplot2 ggplot aes geom_col geom_text scale_fill_manual coord_polar xlim guides theme_minimal theme element_rect element_blank element_text
#' @importFrom scales percent grey_pal
#' @importFrom RColorBrewer brewer.pal
#' @importFrom assertthat assert_that
#' @importFrom stringr str_wrap
#' @importFrom dplyr filter
#' @importFrom magrittr %>%

plot_donut_univariadas_test2 <- function(.base, .var = NULL, .palette = NULL,
                                         .hsize = 1.3, .digits = 2,
                                         .plot_text_size = 6,
                                         .plot_text_color = "#252525",
                                         .legend_text_size = 16,
                                         .gray_category = NULL,
                                         .wrap_length = NULL,
                                         .text_threshold = 0.05,
                                         .omit_label = NULL) {

  # Type Checks
  assert_that(is.data.frame(.base), msg = "Input .base must be a data frame.")
  assert_that(is.numeric(.hsize) && .hsize > 0, msg = "Parameter .hsize must be a positive number.")
  assert_that(is.numeric(.digits) && .digits >= 0 && .digits %% 1 == 0,
              msg = "Parameter .digits must be a non-negative integer.")
  assert_that(is.numeric(.plot_text_size) && .plot_text_size > 0,
              msg = "Parameter .plot_text_size must be a positive number.")
  assert_that(is.numeric(.legend_text_size) && .legend_text_size > 0,
              msg = "Parameter .legend_text_size must be a positive number.")
  if (!is.null(.plot_text_color))
    assert_that(is.character(.plot_text_color) || is.vector(.plot_text_color),
                msg = "Parameter .plot_text_color must be a character vector.")
  if (!is.null(.palette))
    assert_that(is.character(.palette) || is.vector(.palette),
                msg = "Parameter .palette must be a character vector.")
  if (!is.null(.gray_category))
    assert_that(is.character(.gray_category),
                msg = "Parameter .gray_category must be a character vector.")
  if (!is.null(.wrap_length))
    assert_that(is.numeric(.wrap_length) && .wrap_length > 0,
                msg = "Parameter .wrap_length must be a positive number.")
  assert_that(is.numeric(.text_threshold) && .text_threshold > 0 && .text_threshold <= 1,
              msg = "Parameter .text_threshold must be a positive number between 0 and 1.")
  if (!is.null(.omit_label))
    assert_that(is.character(.omit_label), msg = "Parameter .omit_label must be a character vector.")

  # Determine variable to use
  if (!("pct" %in% colnames(.base))) {
    if (rlang::quo_is_missing(rlang::enquo(.var))) {
      stop("If providing raw data, you must specify `.var`.")
    }
    .base <- .base %>% tablas_univariadas(., {{ .var }})
    var_label <- rlang::as_name(rlang::enquo(.var))
  } else {
    var_label <- colnames(.base)[1]  # Assume first column is the categorical variable
  }

  # Apply text wrapping if requested
  if (!is.null(.wrap_length)) {
    original_levels <- levels(as.factor(.base[[var_label]]))
    wrapped_levels <- stringr::str_wrap(original_levels, width = .wrap_length)

    .base[[var_label]] <- factor(stringr::str_wrap(as.character(.base[[var_label]]), width = .wrap_length),
                                 levels = wrapped_levels)
  }

  # Warn if percentages don't sum to ~100%
  if (sum(.base$pct, na.rm = TRUE) < 0.99) {
    warning("The percentage represented is less than 100%. This may indicate missing data or rounding errors.")
  }

  # Define default palette if not provided based on unique categories
  unique_cats <- unique(.base[[var_label]])
  num_unique <- length(unique_cats)
  if (is.null(.palette)) {
    if (num_unique == 2) {
      .palette <- c("#AC0000", "#08305B")
    } else {
      .palette <- RColorBrewer::brewer.pal(n = min(num_unique, 11), name = "RdBu")
    }
  }

  # Create initial color mapping for all categories
  category_colors <- setNames(.palette, unique_cats)

  # Modify colors if gray categories are specified
  if (!is.null(.gray_category)) {
    # Wrap gray category labels if wrap length is provided
    wrapped_gray_categories <- if (!is.null(.wrap_length)) {
      stringr::str_wrap(.gray_category, width = .wrap_length)
    } else {
      .gray_category
    }
    gray_categories <- wrapped_gray_categories[wrapped_gray_categories %in% .base[[var_label]]]
    if (length(gray_categories) > 1) {
      gray_shades <- scales::grey_pal(start = 0.8, end = 0.7)(length(gray_categories))
      category_colors[gray_categories] <- gray_shades
    } else if (length(gray_categories) == 1) {
      category_colors[gray_categories] <- "gray"
    }
  }

  # Process omit labels similarly
  omit_labels <- if (!is.null(.omit_label)) {
    if (!is.null(.wrap_length)) {
      stringr::str_wrap(.omit_label, width = .wrap_length)
    } else {
      .omit_label
    }
  } else {
    NULL
  }

  # Filter data for labels to display: remove omitted labels and those below threshold
  filtered_data <- dplyr::filter(
    .base,
    if (!is.null(omit_labels)) !(!!rlang::sym(var_label) %in% omit_labels) else TRUE,
    pct >= .text_threshold
  )

  # Build the plot
  ggplot(.base, aes(x = .hsize, y = pct, fill = .data[[var_label]])) +
    geom_col() +
    scale_fill_manual(values = category_colors) +
    coord_polar(theta = "y", start = pi/2) +
    xlim(c(0.2, .hsize + 0.5)) +
    guides(fill = guide_legend(title = NULL)) +
    theme_minimal() +
    theme(
      legend.text = element_text(size = .legend_text_size),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
    ) +
    geom_text(
      data = filtered_data,
      aes(label = scales::percent(pct, accuracy = 1 / (10 ^ .digits))),
      position = position_stack(vjust = 0.5),
      size = .plot_text_size,
      color = rep_len(.plot_text_color, nrow(filtered_data))
    )
}
