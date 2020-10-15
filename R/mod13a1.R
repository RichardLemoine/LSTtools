#' MODIS MOD13A1 product
#'
#' MODIS Vegetation Indices MOD13A1 product
#'
#' @docType data
#'
#' @usage data(system.file("data/MOD13A1.hdf", package="LSTtools"))
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
#' data(system.file("data/MOD13A1.hdf", package="LSTtools"))
#' # For NDVI MOD13Q1 product, filtering pixels with usefulness <= 2 and view zenith angle <= 35
#' v <- veg_filter(MOD13A1, vi = "NDVI", rel = FALSE, usef = 2, angle = 35)
#' raster::plot(v[[1]], main = "Filtered NDVI")
#' raster::plot(v[[2]], main = "Reliability")
#' raster::plot(v[[3]], main = "Usefulness")
#' raster::plot(v[[4]], main = "viewing zenith angle (°)")
"MOD13A1"
