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
#' # For NDVI MOD13Q1 product, filtering pixels with usefulness <= 2 and view zenith angle <= 35
#' v <- veg_filter(mod13a1, vi = "NDVI", rel = FALSE, usef = 2, angle = 35)
#' plot(v[[1]], main = "Filtered NDVI")
#' plot(v[[2]], main = "Reliability")
#' plot(v[[3]], main = "Usefulness")
#' plot(v[[4]], main = "viewing zenith angle (°)")
"mod13a1"
