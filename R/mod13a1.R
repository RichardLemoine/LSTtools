#' MODIS MOD13A1 product
#'
#' MODIS Vegetation Indices MOD13A1 product
#'
#' @docType data
#'
#' @usage data(mod13a1)
#'
#' @format A .hdf MODIS MOD13A1 file
#'
#' @keywords MODIS
#'
#' @references Didan, K., A. Barreto-Munoz, R. Solano & A. Huete. (2015), MODIS
#' Vegetation Index User’s Guide (MOD13 Series) Collection-6. Arizona, USA, 32.
#'
#' @source https://earthexplorer.usgs.gov/
#'
#' @examples
#' data(mod13a1)
#' sds <- get_subdatasets(mod13a1)
#' print(sds)
"mod13a1"
