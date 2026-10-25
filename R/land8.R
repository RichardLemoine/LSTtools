#' Landsat 8 bands
#'
#' Landsat 8 subset with atmospherically corrected (surface reflectance) bands 2, 3, 4, 5, 6, 7 and band 10 in DN
#'
#' @docType data
#'
#' @usage data(land8)
#'
#' @format A terra \code{PackedSpatRaster} (the serializable form of a
#' \code{SpatRaster}). \code{SpatRaster} objects hold an external
#' pointer to data on disk/memory and cannot be stored directly in a
#' package's \code{data/} folder, so \code{land8} must be unpacked with
#' \code{\link[terra]{unwrap}} after loading.
#'
#' @keywords Landsat
#'
#' @references USGS. (2019). Landsat 8 data users handbook version 4.
#' USGS Earth Resources Observation and Science (EROS). Sioux Falls,
#' South Dakota. USA. 106.
#'
#' @source https://earthexplorer.usgs.gov/
#'
#' @examples
#' data(land8)
#' land8 <- terra::unwrap(land8)
#' # Define the Landsat 8 bands that will be employed
#' red <- land8[[3]]
#' nir <- land8[[4]]
#' # Compute NDVI
#' veg <- ndvi(nir, red)
"land8"
