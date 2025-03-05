#' Generate Frequency Tables from Multiple Response variables
#'
#' This function generates a multiple response frequency table for a set of variables,
#' displaying the count and percentage of each category within the dataframe.
#' It supports optional weighting and can remove missing values if specified.
#'
#' @param .base A dataframe.
#' @param .vars A set of columns.
#' @param .wt Optional weights.
#' @param na.rm Logical, whether to remove NAs.
#'
#' @return A tibble with counts and percentages.
#' @export
#'
#' @importFrom dplyr group_vars filter count mutate group_by select across
#' @importFrom tidyr pivot_longer
#' @importFrom rlang as_name enquo quo_is_null
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom tibble tibble

tablas_resp_multiple <- function(.base, .vars, .wt = NULL, na.rm = TRUE) {


  existing_groups <- dplyr::group_vars(.base)  # Capture existing groups


  # Validate weight column if provided
  if (!rlang::quo_is_null(rlang::enquo(.wt))) {
    wt_label <- rlang::as_name(rlang::enquo(.wt))
    assertthat::assert_that(wt_label %in% colnames(.base),
                            msg = glue::glue("Weight column '{wt_label}' does not exist in the dataframe."))
    assertthat::assert_that(is.numeric(.base[[wt_label]]),
                            msg = glue::glue("Weight column '{wt_label}' must be numeric."))
  }

  # Transform data to long format and compute frequencies
  data_var <- .base %>%
    dplyr::select({{ .vars }}, {{ .wt }}, all_of(existing_groups)) %>%  # Select relevant columns including weight
    tidyr::pivot_longer(cols = c(-{{ .wt }} , -all_of(existing_groups)), names_to = "variable", values_to = "response") %>%
    dplyr::mutate(response = to_label(response)) %>%
    {if (na.rm) dplyr::filter(., !is.na(response)) else .} %>%
    dplyr::count(.,dplyr::across(all_of(existing_groups)), response, wt = {{ .wt }}, name = "n") %>%
    dplyr::group_by(dplyr::across(all_of(existing_groups))) %>%
    dplyr::mutate(
      total_n = sum(n, na.rm = TRUE),
      pct = ifelse(total_n == 0, 0, n / total_n)
    ) %>%
    dplyr::select(-total_n)

  return(data_var)
}

utils::globalVariables(c(".", "n", "total_n", "response", "variable"))  # Prevents R CMD check warnings


