#' MODIS MOD11A1 product
#'
#' MODIS Land Surface Temperature and Emissivity MOD11A1 product
#'
#' @docType data
#'
#' @usage data(mod11a1)
#'
#' @format A .hdf MODIS MOD11A1 file
#'
#' @keywords MODIS
#'
#' @references Wan. Z. (2013), Collection-6 MODIS Land Surface Temperature Products
#' User’s guide. Santa Barbara, USA, 33.
#'
#' @source https://earthexplorer.usgs.gov/
#'
#' @examples
#' data(mod11a1)
#' # For LST day MOD11A1 product, filtering pixels with LST error <= 1 and view zenith angle <= 35
#' r <- suppressWarnings(lst_filter(mod11a1, time = "day", flag  = 1, angle = 35, conv = TRUE))
#' raster::plot(r[[1]], col=RColorBrewer::brewer.pal(9, 'YlOrRd'), main = "Filtered LST (°C)")
#' raster::plot(r[[2]], main = "LST error (°C)")
#' raster::plot(r[[3]], main = "viewing zenith angle (°)")
"mod11a1"
