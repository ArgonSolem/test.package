#' Generación de frecuencias univariadas
#'
#' Description of what it does
#'
#' @param .base A dataframe.
#' @param .var A column name.
#' @param .wt Optional weights.
#' @param na.rm Logical, whether to remove NAs.
#'
#' @return A tibble with counts and percentages.
#' @export

tablas_univariadas <- function(.base, .var, .wt = NULL, na.rm = TRUE) {

  # Validate if the column exists in .base
  existing_var <- .base %>% select({{ .var }}) %>% names()
  assert_that(existing_var %in% colnames(.base), msg = glue("Column '{.var_label}' doesn't exist."))

  data_var <- .base %>%
    mutate(!!quo_name(enquo(.var)) := to_label({{ .var }})) %>%
    {if (na.rm) filter(., !is.na({{ .var }})) else .} %>%
    count({{ .var }}, wt = {{.wt}}, name = "n") %>%
    mutate(pct = n/sum(n,na.rm = TRUE))
  return(data_var)

}
