#' MODIS MOD13A1 product
#'
#' MODIS Vegetation Indices MOD13A1 product, bundled as a sample
#' .hdf file (not a lazy-loaded R object).
#'
#' @name MOD13A1
#'
#' @format A .hdf MODIS MOD13A1 file, located with
#' \code{system.file("extdata", "MOD13A1.hdf", package = "LSTtools")}
#'
#' @keywords MODIS
#'
#' @references Didan, K., A. Barreto-Munoz, R. Solano & A. Huete. (2015), MODIS
#' Vegetation Index User’s Guide (MOD13 Series) Collection-6. Arizona, USA, 32.
#'
#' @source https://earthexplorer.usgs.gov/
#'
#' @examples
#' mod13a1 <- system.file("extdata", "MOD13A1.hdf", package = "LSTtools")
#' # For NDVI MOD13Q1 product, filtering pixels with usefulness <= 2 and view zenith angle <= 35
#' v <- veg_filter(mod13a1, vi = "NDVI", rel = FALSE, usef = 2, angle = 35)
#' terra::plot(v[[1]], main = "Filtered NDVI")
#' terra::plot(v[[2]], main = "Reliability")
#' terra::plot(v[[3]], main = "Usefulness")
#' terra::plot(v[[4]], main = "viewing zenith angle (°)")
NULL
