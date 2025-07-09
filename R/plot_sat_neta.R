#' Plot Satisfaction vs Insatisfaction (Custom Contrast)
#'
#' @param .data A tibble with columns: .var, satisfaction, insatisfaction, Neto, p_value, direction.
#' @param .var Variable to group by (e.g., P28).
#' @param .color_satisfaction Color for satisfaction bar.
#' @param .color_insatisfaction Color for insatisfaction bar.
#' @param .color_neto Color for Neto label background.
#' @param .digits Digits to round percentage labels.
#' @param .plot_text_size Size of percent text.
#' @param .legend_text_size Size of legend text.
#' @param .wrap_length Max width before wrapping x labels.
#' @param .text_threshold Minimum percent to show text label.
#' @param .omit_label Vector of values to omit from x-axis.
#'
#' @return A ggplot2 plot
#' @export
plot_satisfaction_neto <- function(
    .data,
    .var,
    .color_satisfaction   = "#4CAF50",
    .color_insatisfaction = "#AA0000",
    .color_neto           = "#333333",
    .digits               = 1,
    .plot_text_size       = 6,
    .legend_text_size     = 16,
    .wrap_length          = NULL,
    .text_threshold       = 0.05,
    .omit_label           = NULL
) {
  var_q <- rlang::enquo(.var)
  var_name <- rlang::as_name(var_q)

  df <- .data

  # Optional wrapping
  if (!is.null(.wrap_length)) {
    lvls <- levels(as.factor(df[[var_name]]))
    wrapped <- stringr::str_wrap(lvls, width = .wrap_length)
    df[[var_name]] <- factor(
      stringr::str_wrap(as.character(df[[var_name]]), width = .wrap_length),
      levels = wrapped
    )
  }

  # Omit labels
  omit_vec <- .omit_label %||% character()
  if (!is.null(.wrap_length)) omit_vec <- stringr::str_wrap(omit_vec, width = .wrap_length)

  df <- df %>%
    dplyr::filter(!(!!var_q %in% omit_vec))

  # Start ggplot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_name]])) +

    # Bars
    ggplot2::geom_col(ggplot2::aes(y = satisfaction), fill = .color_satisfaction, width = 0.6) +
    ggplot2::geom_col(ggplot2::aes(y = -insatisfaction), fill = .color_insatisfaction, width = 0.6) +

    # Labels for satisfaction
    ggplot2::geom_text(
      data = dplyr::filter(df, satisfaction >= .text_threshold),
      ggplot2::aes(
        y = satisfaction + 0.015,
        label = scales::percent(satisfaction, accuracy = 1 / (10 ^ .digits))
      ),
      color = .color_satisfaction,
      size = .plot_text_size,
      vjust = 0
    ) +

    # Labels for insatisfaction
    ggplot2::geom_text(
      data = dplyr::filter(df, insatisfaction >= .text_threshold),
      ggplot2::aes(
        y = -insatisfaction - 0.015,
        label = scales::percent(insatisfaction, accuracy = 1 / (10 ^ .digits))
      ),
      color = .color_insatisfaction,
      size = .plot_text_size,
      vjust = 1
    ) +

    # Neto label in middle
    ggplot2::geom_label(
      ggplot2::aes(
        y = 0,
        label = scales::percent(Neto, accuracy = 1 / (10 ^ .digits))
      ),
      fill = .color_neto,
      color = "white",
      size = .plot_text_size,
      fontface = "bold"
    ) +

    # Directional arrow
    ggplot2::geom_text(
      ggplot2::aes(
        y = satisfaction,
        label = dplyr::case_when(
          p_value < 0.05 & direction == 1 ~ "▲",
          p_value < 0.05 & direction == -1 ~ "▼",
          TRUE ~ ""
        ),
        color = dplyr::case_when(
          direction == 1 ~ "#4CAF50",
          direction == -1 ~ "#AA0000",
          TRUE ~ NA_character_
        )
      ),
      size = .plot_text_size,
      hjust = 0.5,
      vjust = -1.5,
      fontface = "bold",
      show.legend = FALSE,
      na.rm = TRUE
    ) +

    # Y scale and theme
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks =  seq(from = -1, to = 1, by = 0.25),
      limits = c(-1, 1.2)
    )+
    ggplot2::scale_color_identity() +
    ggplot2::labs(y = NULL) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "none",  # no auto-legend
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  #-- Manual legend setup
  legend_df <- data.frame(
    x     = c(1, 2, 3),
    y     = 1.1,
    label = c("Satisfacción", "Insatisfacción", "Neto"),
    color = c(.color_satisfaction, .color_insatisfaction, .color_neto)
  )

  #-- Add manual legend to plot
  p <- p +
    ggplot2::geom_tile(
      data = legend_df,
      ggplot2::aes(x = x, y = y, fill = label),
      width = 0.05,
      height = 0.05,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = setNames(legend_df$color, legend_df$label)
    ) +
    ggplot2::geom_text(
      data = legend_df,
      ggplot2::aes(x = x + 0.1, y = y, label = label),
      inherit.aes = FALSE,
      size = .legend_text_size / 3.5,
      hjust = 0,    # left-aligned horizontally
      vjust = 0.5   # center-aligned vertically
    )

  return(p)
}
