#' Save banner tabulations to an Excel file
#'
#' This function takes a list of tabulated banner objects (typically created by
#' `tabulados_banner()`), along with corresponding sheet names, and saves them
#' into a single Excel workbook. The output filename is automatically suffixed
#' with the current date.
#'
#' @param .sheet_objet A list of banner tabulations (usually resulting from `tabulados_banner()`).
#' @param .sheet_name A character vector of names to be used as sheet titles in the Excel file.
#' @param .xlsx_name A character string indicating the base name of the Excel file (without extension).
#'
#' @return An `.xlsx` file is created and saved in the working directory. The function returns no R object (`NULL` invisibly).
#'
#' @details The function adds each banner tabulation to a new worksheet in an Excel file. The filename is
#' automatically suffixed with the current date in the format `[dd-mm-yyyy]`. The tabulations are written using
#' `xl_write()`, which should be available in the package environment or imported separately.
#'
#' @importFrom assertthat assert_that
#' @importFrom openxlsx createWorkbook addWorksheet saveWorkbook
#' @importFrom glue glue
#' @importFrom purrr walk2
#'
#' @export
save_tabulados_banner <- function(.sheet_objet, .sheet_name, .xlsx_name) {
  # Validate inputs
  assertthat::assert_that(is.list(.sheet_objet), msg = ".sheet_objet must be a list of sheet objects.")
  assertthat::assert_that(is.character(unlist(.sheet_name)), msg = ".sheet_name must be a character vector.")
  assertthat::assert_that(is.character(.xlsx_name) && length(.xlsx_name) == 1, msg = ".xlsx_name must be a single string.")
  assertthat::assert_that(length(.sheet_objet) == length(.sheet_name),
                          msg = "The number of objects in .sheet_objet must match the number of names in .sheet_name.")

  # Create workbook
  wb <- openxlsx::createWorkbook()

  # Write each sheet
  purrr::walk2(.sheet_objet, .sheet_name, ~ {
    openxlsx::addWorksheet(wb, .y)
    xl_write(
      obj = .x,
      wb = wb,
      sheet = .y,
      rownames = TRUE,
      colnames = TRUE,
      col_symbols_to_remove = "#",
      row_symbols_to_remove = "#"
    )
  })

  # Save workbook with date-stamped filename
  file_name <- glue::glue("{.xlsx_name} [{format(Sys.Date(), '%d-%m-%Y')}].xlsx")
  openxlsx::saveWorkbook(wb, file = file_name, overwrite = TRUE)

  invisible(NULL)
}
