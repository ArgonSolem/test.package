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
#'
#' @return A plot with counts and percentages for each category of the selected variable.
#' @export
#'
#' @importFrom rlang as_name enquo
#' @importFrom ggplot2 ggplot aes geom_col geom_text scale_fill_manual coord_polar xlim theme element_rect element_blank element_text guides guide_legend
#' @importFrom scales percent grey_pal
#' @importFrom RColorBrewer brewer.pal
#' @importFrom assertthat assert_that
#' @importFrom stringr str_wrap

plot_donut_univariadas <- function(.base, .var = NULL, .palette = NULL, .hsize = 1.3, .plot_text_size = 6, .plot_text_color = "#FFF5F0", .legend_text_size = 16, .gray_category = NULL, .wrap_length = NULL) {

  # Type Checks
  assert_that(is.data.frame(.base), msg = "Input .base must be a data frame.")
  assert_that(is.numeric(.hsize) && .hsize > 0, msg = "Parameter .hsize must be a positive number.")
  assert_that(is.numeric(.plot_text_size) && .plot_text_size > 0, msg = "Parameter .plot_text_size must be a positive number.")
  assert_that(is.character(.plot_text_color) && length(.plot_text_color) == 1, msg = "Parameter .plot_text_color must be a single color string.")
  assert_that(is.numeric(.legend_text_size) && .legend_text_size > 0, msg = "Parameter .legend_text_size must be a positive number.")
  if (!is.null(.palette)) assert_that(is.character(.palette) || is.vector(.palette), msg = "Parameter .palette must be a character vector.")
  if (!is.null(.gray_category)) assert_that(is.character(.gray_category), msg = "Parameter .gray_category must be a character vector.")
  if (!is.null(.wrap_length)) assert_that(is.numeric(.wrap_length) && .wrap_length > 0, msg = "Parameter .wrap_length must be a positive number.")

  # Check if the input is a frequency table (has "pct" column) or raw data
  if (!("pct" %in% colnames(.base))) {
    if (is.null(.var)) stop("If providing raw data, you must specify `.var`.")
    .base <- tablas_univariadas(.base, {{ .var }})
    var_label <- rlang::as_name(rlang::enquo(.var))
  } else {
    var_label <- colnames(.base)[1]  # Assume first column is the categorical variable
  }

  # Apply text wrapping if .wrap_length is provided
  if (!is.null(.wrap_length)) {
    .base[[var_label]] <- stringr::str_wrap(.base[[var_label]], width = .wrap_length)
  }

  # Check if pct column sums to at least 0.99
  if (sum(.base$pct, na.rm = TRUE) < 0.99) {
    warning("The percentage represented is less than 100%. This may indicate missing data or rounding errors.")
  }

  # Define default colors if not provided
  if (is.null(.palette)) {
    if(nrow(.base) == 2) {
      .palette <- c("#AC0000", "#08305B")
    } else {
      .palette <- RColorBrewer::brewer.pal(n = min(nrow(.base), 11), name = "RdBu")
    }
  }

  # Modify colors if gray categories are specified
  category_colors <- setNames(.palette, .base[[var_label]])
  if (!is.null(.gray_category)) {
    wrapped_gray_categories <- stringr::str_wrap(.gray_category, width = .wrap_length)
    gray_categories <- wrapped_gray_categories[wrapped_gray_categories %in% .base[[var_label]]]
    if (length(gray_categories) > 1) {
      gray_shades <- scales::grey_pal(start = 0.8, end = 0.7)(length(gray_categories))
      category_colors[gray_categories] <- gray_shades
    } else {
      category_colors[gray_categories] <- "gray"
    }
  }

  # Create plot
  ggplot(.base, aes(x = .hsize, y = pct, fill = !!rlang::sym(var_label))) +
    geom_col() +
    geom_text(aes(label = scales::percent(pct, accuracy = 0.01)),
              position = position_stack(vjust = 0.5), size = .plot_text_size, color = .plot_text_color) +
    scale_fill_manual(values = category_colors) +
    coord_polar(theta = "y") +
    xlim(c(0.2, .hsize + 0.5)) +
    guides(fill = guide_legend(title = NULL)) +
    theme(
      legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_blank(),
      panel.background = element_rect(fill = "transparent"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "transparent", color = NA),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      legend.text = element_text(size = .legend_text_size)
    )
}
