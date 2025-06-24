#' Calculate Net Satisfaction Summary Table
#'
#' This function computes a satisfaction summary for selected variables,
#' returning counts and percentages of satisfaction, insatisfaction, and net satisfaction.
#'
#' @param .base A dataframe containing the variables to summarize.
#' @param .vars Variables to evaluate (unquoted or tidyselect-compatible).
#' @param .satisfaction Vector of values considered as satisfaction (must be numeric).
#' @param .insatisfaction Vector of values considered as insatisfaction (must be numeric).
#' @param .wt Optional numeric weighting variable.
#'
#' @return A dataframe summarizing satisfaction metrics per variable.
#' @export
tablas_sat_neta <- function(
    .base,
    .vars,
    .satisfaction,
    .insatisfaction,
    .wt = NULL
) {
  # -- Assertions
  assertthat::assert_that(
    is.numeric(.satisfaction),
    msg = ".satisfaction must be a numeric vector."
  )
  assertthat::assert_that(
    is.numeric(.insatisfaction),
    msg = ".insatisfaction must be a numeric vector."
  )

  wt_quo <- rlang::enquo(.wt)
  has_weight <- !rlang::quo_is_null(wt_quo)

  if (has_weight) {
    wt_name <- rlang::as_name(wt_quo)
    assertthat::assert_that(
      wt_name %in% colnames(.base),
      msg = "The weighting variable does not exist in the dataset."
    )
    assertthat::assert_that(
      is.numeric(.base[[wt_name]]),
      msg = ".wt must be a numeric variable."
    )
  }

  # -- Grouping & variable selection
  group_vars <- dplyr::group_vars(.base)
  selected_vars <- dplyr::select(dplyr::ungroup(.base), {{ .vars }}) %>% names()

  # -- Compute satisfaction stats per variable
  purrr::map_dfr(selected_vars, function(var) {
    var_sym <- rlang::sym(var)

    .base %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        variable       = var,
        n              = if (has_weight) {
          sum(ifelse(!is.na(!!var_sym), !!wt_quo, 0), na.rm = TRUE)
        } else {
          sum(!is.na(!!var_sym))
        },
        satisfaction   = {
          sat_total <- if (has_weight) {
            sum(ifelse(!!var_sym %in% .satisfaction, !!wt_quo, 0), na.rm = TRUE)
          } else {
            sum(!!var_sym %in% .satisfaction, na.rm = TRUE)
          }
          sat_total / n
        },
        insatisfaction = {
          insat_total <- if (has_weight) {
            sum(ifelse(!!var_sym %in% .insatisfaction, !!wt_quo, 0), na.rm = TRUE)
          } else {
            sum(!!var_sym %in% .insatisfaction, na.rm = TRUE)
          }
          insat_total / n
        },
        Neto           = satisfaction - insatisfaction,
        .groups        = "drop"
      ) %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(group_vars), sjlabelled::to_label)
      )
  })
}
