#' GGDC Productivity Level Database
#'
#' The GGDC Productivity Level Database provides data on relative prices and labor productivity across countries for 42 major economies and up to 35 industries in 2005.
#'
#' These data complement the World Input-Output Database and are largely based on the results of the 2005 International Comparisons Program (ICP), which estimates purchasing power parities (PPPs) for a global sample of countries.
#'
#' The description of each worksheet is as follows:
#'
#' \itemize{
#'
#'   \item \code{1}: GDP and major sector value added relative prices for 2005, USA GDP = 1;
#'   \item \code{2}: GDP and major sector labor productivity (value added per hour worked) for 2005, USA = 1;
#'   \item \code{3}: GDP and major sector gross output relative prices for 2005, USA GDP = 1;
#'   \item \code{4}: Sector gross output relative prices for 2005, USA GDP = 1; and
#'   \item \code{5}: Industry gross output relative prices for 2005, USA GDP = 1.
#'
#' }
#'
#'
#' @note
#'
#' Except in sheet \code{2}, all data are relative prices, so the purchasing power parity (PPP) divided by the exchange rate. All relative prices are relative to the USA GDP price level, shown in sheet \code{1}. Labor productivity is defined as PPP-converted value added divided by the number of hours worked. PPPs used are those shown in sheet \code{1}.
#'
#' Number of hours worked is from the WIOD Socio-Economic Accounts, except for Argentina, Chile, and South Africa, where data on the number of persons engaged is from the 10-Sector Database and the Africa Sector Database. The average number of hours worked in South Africa is assumed to be equal to that in the USA in every sector. The average number of hours worked in Argentina and Chile is taken from PWT8.0 and assumed to be equal across sectors.
#'
#' @param sheet a number ranging from \code{1} to \code{5}.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom utils download.file unzip
#'
#' @references
#'
#' Robert Inklaar and Marcel P. Timmer (2014), \emph{"The Relative Price of Services"}, Review of Income and Wealth 60(4): 727-746, available at: \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/roiw.12012}.
#'
#' @return A tidy \code{tibble}
#'
#' @export
#'
#' @examples
#' library(ggdcr)
#' productivity_level_database(2)
productivity_level_database <- function(sheet) {

  # download path
  url <- "https://www.rug.nl/ggdc/docs/benchmark_2005.xlsx"
  destfile <- url %>%
    basename()

  possible_curl <- purrr::possibly(
    .f        = curl::curl_download,
    otherwise = "Internet connection has failed. Please try again!",
    quiet     = FALSE
  )

  possible_curl(url, destfile)

  sheets <- destfile %>%
    readxl::excel_sheets()

  # regex
  bad_names <- "[ |-]"


  # conditional statements for sheets 2, 3 and 4
  if (sheet %in% 1:3) {

    sheet_tbl <- readxl::read_excel(destfile, sheet = sheets[[sheet]], range = "A2:K44", na = "n.a.")

    # fix col names
    names(sheet_tbl) <- sheet_tbl %>%
      names() %>%
      stringr::str_to_lower() %>%
      stringr::str_trim(side = "both")

    if (any(stringr::str_detect(names(sheet_tbl), bad_names))) {

      names(sheet_tbl) <- stringr::str_replace_all(names(sheet_tbl), bad_names, "_")

    }

    sheet_tbl <- sheet_tbl %>%
      dplyr::select(-dplyr::contains("x__1"))

    # conditional statements for sheets 5 and 6
  } else if (sheet %in% 5:6) {

    range_names <- dplyr::if_else(sheet == 5, "C2:L2", "C2:AK2")
    range_data  <- dplyr::if_else(sheet == 5, "A3:L45", "A3:AK45")
    col_types   <- if (sheet == 6) c("text", "text", rep("numeric", 35))

    sheet_names <- readxl::read_excel(
      path      = destfile,
      sheet     = sheets[[sheet]],
      range     = range_names,
      col_names = TRUE
      ) %>%
      names()

    # fix col names
    if (any(stringr::str_detect(sheet_names, bad_names))) {

      sheet_names <- sheet_names %>%
        stringr::str_replace_all(bad_names, "_") %>%
        stringr::str_to_lower()

    }

    if (any(stringr::str_detect(sheet_names, ",|&_"))) {

      sheet_names <- sheet_names %>%
        stringr::str_replace_all(",|&_", "") %>%
        stringr::str_to_lower()

    }


    sheet_tbl <- suppressWarnings(
      readxl::read_excel(
        path      = destfile,
        sheet     = sheets[[sheet]],
        range     = range_data,
        na        = "n.a.",
        col_types = col_types
        )
    ) %>%
      rlang::set_names(nm = c("country", "iso_code", sheet_names))

  # error
  } else {

    rlang::abort("Sheet must be equal to 2, 3, 4, 5 or 6.")

  }

  # return
  sheet_tbl %>%
    tidyr::gather(key = "key", value = "value", -.data$iso_code, -.data$country) %>%
    dplyr::mutate_if(purrr::is_character, forcats::as_factor)


}
