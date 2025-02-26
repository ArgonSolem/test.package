#' Generación de frecuencias univariadas
#'
#' Description of what it does
#'
#' @param .base A dataframe.
#' @param .vars Set of columns.
#' @param .wt Optional weights.
#' @param na.rm Logical, whether to remove NAs.
#'
#' @return A tibble with counts and percentages.
#' @export

tablas_resp_multiple <- function(.base, .vars, .wt = NULL, na.rm = TRUE) {

  data_var <- .base %>%
    select({{ .vars }}) %>% # Select relevant columns
    pivot_longer(cols = everything(), names_to = "variable", values_to = "response") %>%
    mutate(response = to_label(response)) %>% # Convert responses to labels
    {if (na.rm) filter(., !is.na(response)) else .} %>%
    count(response, wt = {{.wt}}, name = "n") %>%  # Count occurrences of each response
    mutate(pct = n / sum(n, na.rm = TRUE))  # Calculate percentage

  return(data_var)
}
