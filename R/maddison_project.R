#' Maddison Project Database
#'
#' The 2018 Maddison Project Database provides information on comparative economic growth and income levels over the very long run. This database covers 169 countries up to 2016.
#'
#'
#' @return A tidy \code{tibble} containing 6 columns.
#'
#' The \code{key} column agregates:
#'
#' \itemize{
#'
#'   \item \code{real_gdp_pc}: Real GDP per capita (US$ 2011) with multiple benchmarks. This is a suitable indicator for cross-country income comparisons;
#'   \item \code{real_gdp_pc_2011}: Real GDP per capita (US$ 2011 benchmark), which is advisable for cross-country growth comparisons; and
#'   \item \code{pop}: population in thousands.
#'
#' }
#'
#' Unfortunately, for some periods of time the data had to be estimated. In those cases, \code{i_cig} and \code{i_bm} provide the type of method used. \code{NA} means that no method was need.
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @references
#'
#' \itemize{
#'
#'   \item \strong{Maddison Project Database}, version 2018. Bolt, Jutta, Robert Inklaar, Herman de Jong and Jan Luiten van Zand (2018), \emph{"Rebasing Maddison: new income comparisons and the shape of long-run economic development"}, \href{https://www.rug.nl/ggdc/historicaldevelopment/maddison/research}{Maddison Project Working Paper 10}
#'
#'   \item For the references to the original research on individual countries, see Appendix A of \strong{Bolt et al. (2018)}.
#'
#' }
#'
#' @export
#'
#' @examples
#' library(ggdcr)
#' maddison_project()
maddison_project <- function() {

  safely_read_dta <- purrr::safely(.f = haven::read_dta, quiet = FALSE)

  tbl <- safely_read_dta("https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2018.dta")

  tbl <- tbl[["result"]] %>%
    dplyr::rename(
      real_gdp_pc      = .data$cgdppc,
      real_gdp_pc_2011 = .data$rgdpnapc,
      iso_code         = .data$countrycode
      )

  tbl <-
    suppressWarnings(
      tidyr::gather(
        data  = tbl,
        key   = "key",
        value = "value",
        ...   = -c(.data$iso_code:.data$year, .data$i_cig:.data$i_bm)
      )
    ) %>%
    dplyr::select(.data$iso_code:.data$year, .data$key:.data$value, dplyr::everything())

  tbl %>%
    dplyr::mutate_if(purrr::is_character,      forcats::as_factor) %>%
    dplyr::mutate_if(~ class(.) == "labelled", forcats::as_factor)
}


