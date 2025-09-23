#' Generate Crosstabulations with Banner using expss
#'
#' This function generates a list of crosstabulations using the expss package. It supports optional weighting,
#' optional case count inclusion, and flexible bannering.
#'
#' @param .base A dataframe to be used for the tabulations.
#' @param .var A selection of variables to tabulate (can be one or more).
#' @param .banner Optional variable(s) to use as column banner in the tabulation.
#' @param .wt Optional variable to be used for weights.
#' @param .cases Logical. If \code{TRUE}, includes case counts in the tabulation.
#'
#' @return A list of tabulation tables, one per variable selected.
#' @export
#'
#' @importFrom dplyr select %>%
#' @importFrom expss tab_cols tab_weight tab_cells tab_stat_cases tab_stat_cpct tab_last_sig_cpct tab_pivot total
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom rlang ensyms syms

tabulados_banner <- function(.base, .var, .banner = NULL, .wt = NULL, .cases = FALSE) {
  # Validate .base
  assertthat::assert_that(is.data.frame(.base), msg = "`.base` must be a data frame.")

  # Capture .banner (must be c(var1, var2, ...))
  banner_expr <- rlang::enexpr(.banner)
  banner_names <- character(0)
  if (!rlang::quo_is_null(rlang::enquo(.banner))) {
    if (rlang::is_call(banner_expr, "c")) {
      banner_syms <- rlang::call_args(banner_expr)
      banner_names <- purrr::map_chr(banner_syms, rlang::as_name)
    } else {
      rlang::abort("`.banner` must be provided as c(var1, var2, ...)")
    }
  }

  # Capture weight
  wt_sym  <- rlang::enquo(.wt)
  wt_name <- if (!rlang::quo_is_null(wt_sym)) rlang::as_name(wt_sym) else NULL

  # Validate column existence
  if (length(banner_names) > 0) {
    missing_cols <- setdiff(banner_names, names(.base))
    assertthat::assert_that(
      length(missing_cols) == 0,
      msg = glue::glue("Columns not found in `.base`: {paste(missing_cols, collapse=', ')}")
    )
  }
  if (!is.null(wt_name)) {
    assertthat::assert_that(
      wt_name %in% names(.base),
      msg = glue::glue("Column '{wt_name}' not found in `.base`.")
    )
  }

  # Validate .cases
  assertthat::assert_that(
    is.logical(.cases) && length(.cases) == 1,
    msg = "`.cases` must be a single logical value."
  )

  # Prepare tab_cols inputs
  cols_to_use <- list(total())
  if (length(banner_names) > 0) {
    cols_to_use <- append(cols_to_use, purrr::map(banner_names, ~ .base[[.x]]))
  }

  # Main loop for tabulations
  lapply(.base %>% dplyr::select({{ .var }}), function(variable) {
    tab <- .base %>%
      tab_cols(cols_to_use) %>%
      {
        if (!is.null(wt_name)) tab_weight(., weight = .base[[wt_name]]) else .
      } %>%
      tab_cells(variable)

    # Add cases if requested
    if (.cases) {
      tab <- tab %>%
        tab_stat_cases(
          total_statistic = if (!is.null(wt_name)) c("w_cases", "u_cases") else "u_cases",
          total_label = if (!is.null(wt_name))
            c("Casos válidos ponderados", "Casos válidos no ponderados")
          else
            "Casos válidos no ponderados",
          label = "Total (n)"
        )
    }

    # Always include column percentages
    tab %>%
      tab_stat_cpct(
        total_statistic = if (!is.null(wt_name)) c("w_cpct", "u_cpct") else "u_cpct",
        total_label = if (!is.null(wt_name))
          c("Casos válidos ponderados", "Casos válidos no ponderados")
        else
          "Casos válidos no ponderados",
        label = "Total (%)"
      ) %>%
      tab_last_sig_cpct(bonferroni = TRUE) %>%
      tab_pivot(stat_position = "inside_columns") %>%
      if_na(., 0)
  })
}


