#' World Input-Output Database
#'
#' The World Input-Output Database (WIOD) consists of a series of databases on output, prices, capital stocks, and employment. It covers 28 EU countries and 15 other major countries in the world for the period from 2000 to 2014.
#'
#' @return A tidy \code{tibble}.
#'
#' @references
#'
#' Timmer, M. P., Dietzenbacher, E., Los, B., Stehrer, R. and de Vries, G. J. (2015), \emph{"An Illustrated User Guide to the World Input-Output Database: The Case of Global Automotive Production"}, Review of International Economics., 23: 575-605, available at \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/roie.12178}.
#'
#' @export
#'
#' @examples
#' library(ggdcr)
#' socio_economic_accounts()
socio_economic_accounts <- function() {

  url      <- "http://www.wiod.org/protected3/data16/SEA/WIOD_SEA_Nov16.xlsx"
  destfile <- "WIOD_SEA_Nov16.xlsx"
  curl::curl_download(url, destfile)
  tbl      <- readxl::read_excel(destfile, sheet = 2, na = "NA")

  tbl %>%
    tidyr::gather(key = "year", value = "value", -c(.data$country:.data$code)) %>%
    dplyr::mutate(year = .data$year %>%
                    stringr::str_c("-12-31") %>%
                    lubridate::as_date()
    ) %>%
    dplyr::rename(code_description = .data$description) %>%
    dplyr::mutate(variable_description = dplyr::case_when(
      .data$variable == "GO"     ~ "Gross output by industry at current basic prices (in millions of national currency)",
      .data$variable == "II"     ~ "Intermediate inputs at current purchasers' prices (in millions of national currency)",
      .data$variable == "VA"     ~ "Gross value added at current basic prices (in millions of national currency)",
      .data$variable == "EMP"    ~ "Number of persons engaged (thousands)",
      .data$variable == "EMPE"   ~ "Number of employees (thousands)",
      .data$variable == "H_EMPE" ~ "Total hours worked by employees (millions)",
      .data$variable == "COMP"   ~ "Compensation of employees (in millions of national currency)",
      .data$variable == "LAB"    ~ "Labour compensation (in millions of national currency)",
      .data$variable == "CAP"    ~ "Capital compensation (in millions of national currency)",
      .data$variable == "K"      ~ "Nominal capital stock (in millions of national currency)",

      .data$variable == "GO_PI"  ~ "Price levels gross output, 2010=100",
      .data$variable == "II_PI"  ~ "Price levels of intermediate inputs, 2010=100",
      .data$variable == "VA_PI"  ~ "Price levels of gross value added, 2010=100",

      .data$variable == "GO_PI"  ~ "Gross output, volume indices, 2010=100",
      .data$variable == "GO_PI"  ~ "Intermediate inputs, volume indices, 2010=100",
      TRUE                       ~ "Gross value added, volume indices, 2010=100"
    )
    ) %>%
    dplyr::mutate_if(names(.) %in% c("country", "variable", "code", "variable_description"), forcats::as_factor) %>%
    dplyr::select(
      .data$country,
      .data$variable,
      .data$code:.data$variable_description,
      .data$code_description
    )

}


