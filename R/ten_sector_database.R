#' GGDC 10-Sector Database
#'
#' The GGDC 10-Sector Database provides a long-run internationally comparable dataset on sectoral productivity performance in Africa, Asia, and Latin America. Variables covered in the data set are annual series of value added, output deflators, and persons employed for 10 broad sectors.
#'
#' The GGDC 10-Sector Database gives sector detail to the historical macro data in Maddison (2003) from 1950 onwards. It consists of series for 11 countries in Africa, 11 countries in Asia, 2 countries in the Middle East and North Africa, and 9 in Latin-America.
#'
#' It should be stressed that the estimates for the total economy are aggregated across sectors and that, because of adjustments at the sector level, the aggregate results are not fully consistent with the national accounts aggregates (see the sources and methods document). Also note that value added data in this database are expressed in local currencies.
#'
#' The sectors covered are in the \code{key} column and include:
#'
#' \itemize{
#'
#'   \item \code{Agriculture}: Agriculture, Hunting and Forestry, Fishing;
#'   \item \code{Mining}: Mining and Quarrying;
#'   \item \code{Manufacturing}: Manufacturing;
#'   \item \code{Public Utilities}: Electricity, Gas and Water supply;
#'   \item \code{Construction}: Construction;
#'   \item \code{Retail}: Wholesale and Retail trade; repair of motor vehicles, motorcycles and personal and household goods, Hotels and Restaurants;
#'   \item \code{Transport}: Transport, Storage and Communications;
#'   \item \code{Business}: Financial Intermediation, Renting and Business Activities (excluding owner occupied rents);
#'   \item \code{Government}: Public Administration and Defense, Education, Health and Social work; and
#'   \item \code{Other}: Other Community, Social and Personal service activities, Activities of Private Households.
#'
#' }
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @references
#'
#' \itemize{
#'
#'   \item Timmer, M.P., de Vries, G.J., & de Vries, K. (2015). \emph{Patterns of Structural Change in Developing Countries.} . In J. Weiss, & M. Tribe (Eds.), Routledge Handbook of Industry and Development. (pp. 65-83). Routledge.
#'
#'   \item The working paper can be found at \url{www.ggdc.net/publications/memorandum/gd149.pdf}
#'
#'   \item Sources and methods used for calculations: \url{https://www.rug.nl/ggdc/docs/10sector_sm_jan2015.pdf}
#'
#' }
#'
#' @return A "tidy" \code{tibble}.
#'
#' @export
#'
#' @examples
#' library(ggdcr)
#' ten_sector_database()
ten_sector_database <- function() {

  # download and unzip STATA file
  td <- tempdir() # create a temporary directory
  tf <- tempfile(tmpdir = td, fileext = ".zip") # create the placeholder file
  download.file("https://www.rug.nl/ggdc/docs/10sd_jan15_2014.zip", tf) # download into the placeholder file
  fname <- unzip(tf, list = TRUE)$Name[1] # get the name of the first file in the zip archive
  unzip(tf, files = fname, exdir = td, overwrite = TRUE) # unzip the file to the temporary directory
  file_path = file.path(td, fname) # file_path is the full path to the extracted file

  safely_read_dta <- purrr::safely(.f = haven::read_dta, quiet = FALSE)

  tbl <- safely_read_dta(file_path)

  tbl <- tbl[["result"]] %>%
    dplyr::rename_all(stringr::str_to_lower) %>%
    dplyr::rename(
      region_code      = .data$regioncode,
      agriculture      = .data$agr,
      mining           = .data$min,
      manufacturing    = .data$man,
      public_utilities = .data$pu,
      construction     = .data$con,
      retail           = .data$wrt,
      transport        = .data$tra,
      business         = .data$fire,
      government       = .data$gov,
      other            = .data$oth
      ) %>%
    dplyr::select(-.data$sum)

  tbl <-
    suppressWarnings(
      tidyr::gather(
        data  = tbl,
        key   = "key",
        value = "value",
        ...   = -c(.data$country:.data$year)
        )
      ) %>%
    dplyr::mutate(variable = dplyr::case_when(
      .data$variable  == "VA"     ~ "va",
      .data$variable  == "VA_Q05" ~ "va_2005",
      .data$variable  == "VA_Q10" ~ "va_2010",
      .data$variable  == "VA_Q91" ~ "va_1991",
      TRUE                        ~ "people_employed"
      )
    ) %>%
    dplyr::mutate_if(purrr::is_character, forcats::as_factor)

  # return
  tbl

}
