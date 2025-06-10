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
#' @param .pct_formula Charater, to define the type of denominator for the percentage.
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

tablas_resp_multiple <- function(.base, .vars, .wt = NULL, na.rm = TRUE,
                                 .pct_formula = c("responses", "cases")) {
  # Capture existing grouping variables
  existing_groups <- dplyr::group_vars(.base)

  # Capture weight argument as a quosure
  wt_quo <- rlang::enquo(.wt)

  # Validate weight column if provided
  if (!rlang::quo_is_null(wt_quo)) {
    wt_label <- rlang::as_name(wt_quo)
    assertthat::assert_that(
      wt_label %in% colnames(.base),
      msg = glue::glue("Weight column '{wt_label}' does not exist in the dataframe.")
    )
    assertthat::assert_that(
      is.numeric(.base[[wt_label]]),
      msg = glue::glue("Weight column '{wt_label}' must be numeric.")
    )
  }

  # Match pct formula option
  pct_formula <- match.arg(.pct_formula)

  # Prepare a working copy of .base with grouping, vars, and optionally a 'weight' column
  if (rlang::quo_is_null(wt_quo)) {
    tmp_base <- .base %>%
      dplyr::select(dplyr::all_of(existing_groups), {{ .vars }})
  } else {
    tmp_base <- .base %>%
      dplyr::select(dplyr::all_of(existing_groups), {{ .vars }}, weight = !!wt_quo)
  }

  # If percentage is by cases, compute valid-case denominators per group
  if (pct_formula == "cases") {
    case_denom <- tmp_base %>%
      # Create a consistent weight column (.wt2)
      dplyr::mutate(.wt2 = if (rlang::quo_is_null(wt_quo)) 1 else weight) %>%
      # Determine which rows count as valid cases
      dplyr::mutate(
        valid = if (na.rm)
          dplyr::if_any({{ .vars }}, ~ !is.na(.))
        else
          TRUE
      ) %>%
      # Sum weights of valid cases within each group
      dplyr::group_by(dplyr::across(dplyr::all_of(existing_groups))) %>%
      dplyr::summarise(
        valid_case_denom = sum(.wt2, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Reshape to long format and compute counts
  data_long <- tmp_base %>%
    tidyr::pivot_longer(
      cols = {{ .vars }},
      names_to = "variable",
      values_to = "response"
    ) %>%
    dplyr::mutate(response = to_label(response)) %>%
    { if (na.rm) dplyr::filter(., !is.na(response)) else . }

  # Count responses, weighted or unweighted
  if (rlang::quo_is_null(wt_quo)) {
    data_counts <- data_long %>%
      dplyr::count(
        dplyr::across(dplyr::all_of(existing_groups)),
        response,
        name = "n"
      )
  } else {
    data_counts <- data_long %>%
      dplyr::count(
        dplyr::across(dplyr::all_of(existing_groups)),
        response,
        wt = weight,
        name = "n"
      )
  }

  # Compute percentage based on chosen formula
  if (pct_formula == "responses") {
    result <- data_counts %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(existing_groups))) %>%
      dplyr::mutate(
        total_n = sum(n, na.rm = TRUE),
        pct = ifelse(total_n == 0, 0, n / total_n)
      ) %>%
      dplyr::select(-total_n)
  } else {
    # Join counts with case denominators, using cross_join if no grouping
    result <- if (length(existing_groups) > 0) {
      data_counts %>% dplyr::left_join(case_denom, by = existing_groups)
    } else {
      data_counts %>% dplyr::cross_join(case_denom)
    }
    result <- result %>%
      dplyr::mutate(
        pct = ifelse(valid_case_denom == 0, 0, n / valid_case_denom)
      ) %>%
      dplyr::select(-valid_case_denom)
  }

  return(result)
}

utils::globalVariables(c(".", "n", "total_n", "valid_case_denom","response", "variable"))  # Prevents R CMD check warnings


