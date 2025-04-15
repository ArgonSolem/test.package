#' Generate Crosstabulations with Banner using expss
#'
#' This function generates a list of crosstabulations using the expss package. It supports optional weighting,
#' optional case count inclusion, and flexible bannering.
#'
#' @param .base A dataframe to be used for the tabulations.
#' @param .var A selection of variables to tabulate.
#' @param .banner Optional variable to use as column banner in the tabulation.
#' @param .wt Optional variable name to be used for weights.
#' @param .cases Logical. If \code{TRUE}, includes case counts in the tabulation.
#'
#' @return A list of tabulation tables, one per variable selected.
#' @export
#'
#' @importFrom dplyr select %>%
#' @importFrom expss tab_cols tab_weight tab_cells tab_stat_cases tab_stat_cpct tab_last_sig_cpct tab_pivot total
#' @importFrom assertthat assert_that is.string is.flag
#'

tabulados_banner <- function(.base, .var, .banner = NULL, .wt = NULL, .cases = FALSE) {
  # Validate .base
  assertthat::assert_that(is.data.frame(.base), msg = "`.base` must be a data frame.")

  # Convert quosure inputs to character safely
  banner_name <- if (!rlang::quo_is_null(rlang::enquo(.banner))) rlang::as_name(rlang::ensym(.banner)) else NULL
  wt_name <- if (!rlang::quo_is_null(rlang::enquo(.wt))) rlang::as_name(rlang::ensym(.wt)) else NULL

  # Validate column existence in .base
  if (!is.null(banner_name)) {
    assertthat::assert_that(banner_name %in% names(.base), msg = glue::glue("Column '{banner_name}' not found in `.base`."))
  }
  if (!is.null(wt_name)) {
    assertthat::assert_that(wt_name %in% names(.base), msg = glue::glue("Column '{wt_name}' not found in `.base`."))
  }

  # Validate .cases
  assertthat::assert_that(is.logical(.cases) && length(.cases) == 1, msg = "`.cases` must be a single logical value.")

  # Prepare tab_cols inputs safely
  cols_to_use <- list(total())
  if (!is.null(banner_name)) {
    !!!cols_to_use <- append(cols_to_use, list(.base[[banner_name]]))
  }

  # Main loop for tabulations
  lapply(.base %>% dplyr::select({{ .var }}), function(variable) {
    tab <- .base %>%
      tab_cols(cols_to_use) %>%
      {
        if (!is.null(wt_name)) tab_weight(., weight = .base[[wt_name]]) else .
      } %>%
      tab_cells(variable)

    # Optional: add cases
    if (.cases) {
      tab <- tab %>%
        tab_stat_cases(
          total_statistic = if (!is.null(wt_name)) c("w_cases", "u_cases") else "u_cases",
          total_label = if (!is.null(wt_name)) c("Casos válidos ponderados", "Casos válidos no ponderados") else "Casos válidos no ponderados",
          label = "Total (n)"
        )
    }

    # Always include column percentages
    tab %>%
      tab_stat_cpct(
        total_statistic = if (!is.null(wt_name)) c("w_cpct", "u_cpct") else "u_cpct",
        total_label = if (!is.null(wt_name)) c("Casos válidos ponderados", "Casos válidos no ponderados") else "Casos válidos no ponderados",
        label = "Total (%)"
      ) %>%
      tab_last_sig_cpct(bonferroni = TRUE) %>%
      tab_pivot(stat_position = "inside_columns") %>%
      if_na(., 0)
  })
}


