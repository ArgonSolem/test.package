#' Add Significance Testing to Satisfaction Table
#'
#' This function compares satisfaction proportions between groups using the
#' `prop.test()` and returns a table with p-values and direction of differences.
#'
#' Two comparison modes are supported:
#' - `"reference"`: compares all groups to a reference group.
#' - `"sequential"`: compares each row to the next in sequence.
#'
#' @param .tbl A dataframe, usually the output from `tab_sat_neta()`.
#' @param .group_col Grouping column (unquoted variable name).
#' @param .prop_col Proportion column (unquoted variable name).
#' @param .mode Type of comparison: "reference" or "sequential".
#' @param .ref_group Reference group value for comparison (used only in "reference" mode).
#'
#' @return The original table with `p_value` and `direction` columns added.
#' @export
tablas_diff_sig <- function(
    .tbl,
    .group_col,
    .prop_col,
    .mode = c("reference", "sequential"),
    .ref_group = NULL
) {
  # -- Convert inputs
  group_quo <- rlang::enquo(.group_col)
  prop_quo  <- rlang::enquo(.prop_col)
  group_nm  <- rlang::as_name(group_quo)
  prop_nm   <- rlang::as_name(prop_quo)
  n_sym     <- rlang::sym("n")  # always using column 'n'

  # -- Assertions
  assertthat::assert_that("n" %in% names(.tbl), msg = "Column 'n' not found in table.")
  assertthat::assert_that(is.numeric(.tbl$n), msg = "Column 'n' must be numeric.")
  assertthat::assert_that(prop_nm %in% names(.tbl), msg = "prop_col not found in table.")
  assertthat::assert_that(is.numeric(.tbl[[prop_nm]]), msg = "prop_col must be numeric.")

  .mode <- match.arg(.mode)

  # -- Track original row order
  .tbl <- dplyr::mutate(.tbl, row_id__ = dplyr::row_number())

  if (.mode == "reference") {
    .ref_group <- .ref_group %||% dplyr::pull(.tbl, !!group_quo)[1]

    ref_values <- .tbl %>%
      dplyr::filter(!!group_quo == .ref_group) %>%
      dplyr::transmute(
        ref_success = round(n * !!prop_quo),
        ref_total   = n,
        ref_prop    = !!prop_quo
      ) %>%
      dplyr::slice(1)

    if (nrow(ref_values) == 0) {
      stop(glue::glue("Reference group '{.ref_group}' not found."))
    }

    ref_success <- ref_values$ref_success
    ref_total   <- ref_values$ref_total
    ref_prop    <- ref_values$ref_prop

    out <- .tbl %>%
      dplyr::mutate(
        success = round(n * !!prop_quo),
        p_value = dplyr::case_when(
          !!group_quo == .ref_group ~ NA_real_,
          TRUE ~ purrr::map2_dbl(success, n, ~ stats::prop.test(
            x = c(ref_success, .x),
            n = c(ref_total, .y)
          )$p.value)
        ),
        direction = dplyr::case_when(
          !!group_quo == .ref_group ~ NA_integer_,
          !!prop_quo > ref_prop     ~ 1L,
          TRUE                      ~ -1L
        )
      ) %>%
      dplyr::select(-success, -row_id__)
  }

  if (.mode == "sequential") {
    .tbl <- .tbl %>%
      dplyr::arrange(row_id__) %>%
      dplyr::mutate(success = round(n * !!prop_quo))

    successes <- .tbl$success
    totals    <- .tbl$n
    props     <- .tbl[[prop_nm]]

    p_values <- purrr::map2_dbl(
      1:(nrow(.tbl) - 1),
      2:nrow(.tbl),
      ~ stats::prop.test(
        x = c(successes[.x], successes[.y]),
        n = c(totals[.x], totals[.y])
      )$p.value
    )
    p_values  <- c(NA_real_, p_values)
    directions <- c(NA_integer_, sign(props[-1] - props[-length(props)]))

    out <- .tbl %>%
      dplyr::mutate(
        p_value   = p_values,
        direction = directions
      ) %>%
      dplyr::select(-success, -row_id__)
  }

  return(out)
}
