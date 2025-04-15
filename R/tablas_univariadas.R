#' Generate Univariate Frequency Tables
#'
#' This function generates a univariate frequency table, displaying the count
#' and percentage of each category of a variable within a dataframe.
#' It supports optional weighting and can remove missing values if specified.
#'
#' @param .base A dataframe.
#' @param .var A column name.
#' @param .wt Optional weights.
#' @param na.rm Logical, whether to remove NAs.
#'
#' @return A tibble with counts and percentages for each category of the selected variable.
#' @export
#'
#' @importFrom dplyr group_vars filter count mutate group_by select across
#' @importFrom rlang as_name enquo quo_is_null
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom tibble tibble

tablas_univariadas <- function(.base, .var, .wt = NULL, na.rm = TRUE) {

  var_label <- rlang::as_name(rlang::enquo(.var))  # Extract variable name
  existing_groups <- dplyr::group_vars(.base)  # Capture existing groups

  # Validate if the column exists in .base
  assertthat::assert_that(var_label %in% colnames(.base),
                          msg = glue::glue("Column '{var_label}' doesn't exist in the dataframe."))

  # Check if the column contains only NAs
  if (all(is.na(.base[[var_label]]))) {
    if (na.rm) {
      return(tibble::tibble(!!var_label := character(), n = integer(), pct = numeric()))
    } else {
      warning(glue::glue("Column '{var_label}' contains only NA values. Proceeding without removal."))
    }
  }

  # Validate weight column if provided
  if (!rlang::quo_is_null(rlang::enquo(.wt))) {
    wt_label <- rlang::as_name(rlang::enquo(.wt))
    assertthat::assert_that(wt_label %in% colnames(.base),
                            msg = glue::glue("Weight column '{wt_label}' doesn't exist in the dataframe."))
    assertthat::assert_that(is.numeric(.base[[wt_label]]),
                            msg = glue::glue("Weight column '{wt_label}' must be numeric."))
  }

  # Compute frequency table while preserving existing groups
  data_var <- .base %>%
    {if (na.rm) dplyr::filter(., !is.na({{ .var }})) else .} %>%
    dplyr::count(., dplyr::across(all_of(existing_groups)), {{ .var }}, wt = {{ .wt }}, name = "n") %>%
    dplyr::group_by(dplyr::across(all_of(existing_groups))) %>%
    dplyr::mutate(
      total_n = sum(n, na.rm = TRUE),
      pct = ifelse(total_n == 0, 0, n / total_n)
    ) %>%
    dplyr::select(-total_n)

  #Transform to labelled factor or factor in case of no labels
  if (!is.null(attributes(data_var[1])$labels))
    data_var[1] <- to_label(data_var[1])
  else data_var[1] <- as_factor(data_var[1])

  return(data_var)
}

utils::globalVariables(c(".", "n", "total_n", "response"))  # Prevents R CMD check warnings

