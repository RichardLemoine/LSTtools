#' MODIS MOD11A1 product
#'
#' MODIS Land Surface Temperature and Emissivity MOD11A1 product,
#' bundled as a sample .hdf file (not a lazy-loaded R object).
#'
#' @name MOD11A1
#'
#' @format A .hdf MODIS MOD11A1 file, located with
#' \code{system.file("extdata", "MOD11A1.hdf", package = "LSTtools")}
#'
#' @keywords MODIS
#'
#' @references Wan. Z. (2013), Collection-6 MODIS Land Surface Temperature Products
#' User’s guide. Santa Barbara, USA, 33.
#'
#' @source https://earthexplorer.usgs.gov/
#'
#' @examples
#' mod11a1 <- system.file("extdata", "MOD11A1.hdf", package = "LSTtools")
#' # For LST day MOD11A1 product, filtering pixels with LST error <= 1 and view zenith angle <= 35
#' r <- lst_filter(mod11a1, time = "day", flag  = 1, angle = 35, conv = TRUE)
#' terra::plot(r[[1]], col=RColorBrewer::brewer.pal(9, 'YlOrRd'), main = "Filtered LST (°C)")
#' terra::plot(r[[2]], main = "LST error (°C)")
#' terra::plot(r[[3]], main = "viewing zenith angle (°)")
NULL
