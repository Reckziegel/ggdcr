#' Download the Penn World Table 9.0
#'
#' Downloads the data of PWT 9.0, as available on \url{www.ggdc.net/pwt}.
#'
#' Penn World Table (PWT) version 9.0 is a database with information on relative levels of income, output, input and productivity, covering 182 countries between 1950 and 2014.
#'
#' Different indicators are grouped in the column \code{key} and are deffined as follows:
#'
#'
#' \strong{Real GDP, employment and population levels}
#'
#' \itemize{
#'   \item \code{rgdpe}: Expenditure-side real GDP at chained PPPs (in mil. 2011US$)
#'   \item \code{rgdpo}: Output-side real GDP at chained PPPs (in mil. 2011US$)
#'   \item \code{pop}: Population (in millions)
#'   \item \code{emp}: Number of persons engaged (in millions)
#'   \item \code{avh}: Average annual hours worked by persons engaged
#'   \item \code{hc}: Human capital index, based on years of schooling and returns to education
#' }
#'
#'
#' \strong{Current price GDP, capital and TFP}
#'
#' \itemize{
#'   \item \code{ccon}: Real consumption of households and government, at current PPPs (in mil. 2011US$)
#'   \item \code{cda}: Real domestic absorption, (real consumption plus investment), at current PPPs (in mil. 2011US$)
#'   \item \code{cgdpe}: Expenditure-side real GDP at current PPPs (in mil. 2011US$)
#'   \item \code{cgdpo}: Output-side real GDP at current PPPs (in mil. 2011US$)
#'   \item \code{ck}: Capital stock at current PPPs (in mil. 2011US$)
#'   \item \code{ctfp}: TFP level at current PPPs (USA=1)
#'   \item \code{cwtfp}: Welfare-relevant TFP levels at current PPPs (USA=1)
#' }
#'
#'
#' \strong{National Accounts-Based Variables}
#'
#' \itemize{
#'   \item \code{rgdpna}: Real GDP at constant 2011 national prices (in mil. 2011US$)
#'   \item \code{rconna}: Real consumption at constant 2011 national prices (in mil. 2011US$)
#'   \item \code{rdana}: Real domestic absorption at constant 2011 national prices (in mil. 2011US$)
#'   \item \code{rkna}: Capital stock at constant 2011 national prices (in mil. 2011US$)
#'   \item \code{rtfpna}: TFP at constant national prices (2011=1)
#'   \item \code{rwtfpna}:Welfare-relevant TFP at constant national prices (2011=1)
#'   \item \code{labsh}: Welfare-relevant TFP at constant national prices (2011=1)
#'   \item \code{delta}: Average depreciation rate of the capital stock
#' }
#'
#'
#' \strong{Exchange rates and GDP price levels}
#'
#' \itemize{
#'   \item \code{xr}: Exchange rate, national currency/USD (market+estimated)
#'   \item \code{pl_con}: Price level of CCON (PPP/XR), price level of USA GDPo in 2011=1
#'   \item \code{pl_da}: Price level of CDA (PPP/XR), price level of USA GDPo in 2011=1
#'   \item \code{pl_gdpo}: Price level of CGDPo (PPP/XR),  price level of USA GDPo in 2011=1
#' }
#'
#'
#' \strong{Shares in CGDPo}
#'
#' \itemize{
#'   \item \code{csh_c}: Share of household consumption at current PPPs
#'   \item \code{csh_i}: Share of gross capital formation at current PPPs
#'   \item \code{csh_g}: Share of government consumption at current PPPs
#'   \item \code{csh_x}: Share of merchandise exports at current PPPs
#'   \item \code{csh_m}: Share of merchandise imports at current PPPs
#'   \item \code{csh_r}: Share of residual trade and GDP statistical discrepancy at current PPPs
#' }
#'
#'
#' \strong{Price levels, expenditure categories and capital}
#'
#' \itemize{
#'   \item \code{pl_c}: Price level of household consumption,  price level of USA GDPo in 2011=1
#'   \item \code{pl_i}: Price level of capital formation,  price level of USA GDPo in 2011=1
#'   \item \code{pl_g}: Price level of government consumption,  price level of USA GDPo in 2011=1
#'   \item \code{pl_x}: Price level of exports, price level of USA GDPo in 2011=1
#'   \item \code{pl_m}: Price level of imports, price level of USA GDPo in 2011=1
#'   \item \code{pl_k}: Price level of the capital stock, price level of USA in 2011=1
#' }
#'
#'
#'
#' @return A tidy \code{tibble}.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' library(ggdcr)
#' penn_world_table()
penn_world_table <- function() {

  safely_read_dta <- purrr::safely(.f = haven::read_dta, quiet = FALSE)

  tbl <- safely_read_dta("https://www.rug.nl/ggdc/docs/pwt90.dta")

  tbl <-
    suppressWarnings(
      tidyr::gather(
        data  = tbl[['result']],
        key   = 'key',
        value = 'value',
        ...   = -c(.data$countrycode:.data$year, .data$i_cig:.data$statcap)
        )
      ) %>%
    dplyr::select(.data$countrycode:.data$year, .data$key, .data$value, dplyr::everything())

  tbl %>%
    dplyr::mutate_if(purrr::is_character, forcats::as_factor) %>%
    dplyr::rename(iso_code = "countrycode")

}





