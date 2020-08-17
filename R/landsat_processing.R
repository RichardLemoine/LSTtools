#' @title ndvi
#'
#' @description Compute the Normalized Difference Vegetation Index (NDVI).
#'
#' @param x NIR band.
#' @param y Red band.
#'
#' @details Compute the Normalized Difference Vegetation index (NDVI)
#'employing the surface reflectance NIR and Red bands of a satellite image.
#' @return NDVI values ranging from -1 to 1.
#'
#' @references Rouse, J.W., R.H. Haas, J.A. Schell, and D.W. Deering.
#' (1974). Monitoring vegetation systems in the Great Plains with
#' ERTS, In: S.C. Freden, E.P. Mercanti, and M. Becker (eds) Third
#' Earth Resources Technology Satellite-1 Syposium. Volume I:
#' Technical Presentations, NASA SP-351, NASA, Washington, D.C.,
#' pp. 309-317.
#' @examples
#'# For Landsat 8 bands
#'ndviind <- ndvi(B5, B4)
#'
#' @export
#'
#'
ndvi <- function(x, y) {
  r <- (x - y) / (x + y)
  r[r < -1 | r > 1 ] <- NA
  return(r)
}


#' @title evi
#'
#' @description Compute the Enhanced Vegetation Index (EVI)
#' employing the surface reflectance NIR, Red and Blue bands
#' of a satellite image.
#'
#' @param x NIR band.
#' @param y Red band.
#' @param b Blue band.
#'
#' @details Compute the Enhanced Vegetation Index (EVI)
#'employing the surface reflectance NIR, Red and Blue
#'bands of a satellite image.
#' @return EVI values ranging from -1 to 1.
#'
#' @references Liu H.Q., Huete A.R. A. (1995). A feedback based
#' modification of the NDV I to minimize canopy background and
#' atmospheric noise. IEEE Transactions on Geoscience and Remote
#' Sensing 33:457-465.
#'
#' Huete A.R., Liu, H.Q., Batchily, K., van Leeuwen, W. (1997).
#' A comparison of vegetation indices global set of TM images
#' for EOS-MODIS. Remote Sensing of Environment, 59:440-451.
#'
#' Huete,A., K. Didan, T. Miura, E.P. Rodriguez, X. Gao and
#' L.G. Ferreira. (2002). Overview of the radiometric and
#' biophysical performance of the MODIS vegetation indices.
#' Remote Sensing of Environment 83:195-213.
#'
#' @examples
#'# For Landsat 8 bands
#'eviind <- evi(B5, B4, B2)
#'
#' @export
#'
#'
evi <-function(x, y, b){
  r <- 2.5 * ((x - y) / (x + 6 * y - 7.5 * b + 1))
  r[r < -1 | r > 1] <- NA
  return(r)
}


#' @title savi
#'
#' @description Compute the Soil-Adjusted Vegetation Index (SAVI).
#'
#' @param x NIR band.
#' @param y Red band.
#'
#' @details Compute the Soil-Adjusted Vegetation Index
#'(SAVI) employing the surface reflectance NIR and Red
#'bands of a satellite image.
#' @return SAVI values ranging from -1 to 1.
#'
#' @references Huete, A.R. (1988). A soil-adjusted vegetation
#' index (SAVI). Remote Sens. Environ., 25, pp. 295-309.
#'
#' @examples
#'# For Landsat 8 bands
#'saviind <- savi(B5, B4)
#'
#' @export
#'
#'
savi <-function(x, y){
  r <- ((x - y) / (x + y + 0.5)) * (1.5)
  r[r < -1 | r > 1] <- NA
  return(r)
}


#' @title msavi
#'
#' @description Compute the Modified Soil-Adjusted Vegetation Index (MSAVI).
#'
#' @param x NIR band.
#' @param y Red band.
#'
#' @details Compute the Modified Soil-Adjusted Vegetation Index
#'(MSAVI) employing the surface reflectance NIR and Red bands
#'of a satellite image.
#'
#' @return MSAVI values ranging from -1 to 1.
#'
#' @references Qi J., Kerr Y., Chehbouni A. (1994). External
#' factor consideration in vegetation index development.
#' Proc. of Physical Measurements and Signatures in Remote
#' Sensing, ISPRS, 723-730.
#'
#' @examples
#'# For Landsat 8 bands
#'msaviind <- msavi(B5, B4)
#'
#' @export
#'
#'
msavi <-function(x, y){
  r <- (2 * x + 1 - sqrt((2 * x + 1)^2 - 8 * (x - y))) / 2
  r[r < -1 | r > 1] <- NA
  return(r)
}


#' @title ndbi
#'
#' @description Compute the Normalized Difference Built-up Index (NDBI).
#'
#' @param x SWIR band.
#' @param y NIR band.
#'
#' @details Compute the Normalized Difference Built-up Index (NDBI)
#'employing the surface reflectance SWIR and NIR bands of a
#'satellite image.
#'
#' @return NDBI values ranging from -1 to 1.
#'
#' @references ZHA, Y., GAO, Y. and NI, S. (2003). Use of normalized
#' difference built-up index in automatically mapping urban areas
#' from TM imagery. International Journal of Remote Sensing, 24, pp.
#' 583-594.
#'
#' @examples
#'# For Landsat 8 bands
#'ndbiind <- ndbi(B6, B5)
#'
#' @export
#'
#'
ndbi <- function(x, y) {
  r <- (x - y) / (x + y)
  r[r < -1 | r > 1] <- NA
  return(r)
}


#' @title urbi
#'
#' @description Compute the Urban Index (UI).
#'
#' @param x SWIR band.
#' @param y NIR band.
#'
#' @details Compute the Urban Index (UI) employing the surface
#'reflectance SWIR and NIR bands of a satellite image.
#'
#' @return UI values ranging from -1 to 1.
#'
#' @references Kawamura, M., Jayamana, S. and Tsujiko, Y. (1996).
#' Relation between social and environmental conditions in
#' Colombo Sri Lanka and the urban index estimated by satellite
#' remote sensing data. Int. Arch. Photogramm. Remote Sens.
#' 31(Part B7), 321-326.
#'
#' @examples
#'# For Landsat 8 bands
#'urbind <- urbi (B7, B5)
#'
#' @export
#'
#'
urbi <- function(x, y) {
  r<- (x - y) / (x + y)
  r[r < -1 | r > 1] <- NA
  return(r)}


#' @title ibi
#'
#' @description Compute the Index based built-up Index (IBI).
#'
#' @param x SWIR band.
#' @param y NIR band.
#' @param r Red band.
#' @param g Green band.
#'
#' @details Compute the Index based built-up Index (IBI) employing
#'the surface reflectance SWIR, NIR, Red and Green bands of a
#'satellite image.
#'
#' @return IBI values ranging from -1 to 1.
#'
#' @references Xu, H. (2008). A new index for delineating built-up
#' land features in satellite imagery. Int. J. Remote Sens. 29, 4269-4276.
#'
#' @examples
#'# For Landsat 8 bands
#'ibiind <- ibi(B6, B5, B4, B3)
#'
#' @export
#'
#'
ibi <- function(x, y, r, g) {
  r <- ((2 * x / (x + y)) - ((y / (y + r) + g / (g + x)))) /
    ((2 * x / (x + y)) + ((y / (y + r) + g / (g + x))))
  r[r < -1 | r > 1] <- NA
  return(r)
}


#' @title emissivity
#'
#' @description Compute land surface emissivity.
#'
#' @param x NDVI raster layer.
#' @param nonveg NDVI threshold value for non-vegetated pixels.
#' @param veg NDVI threshold value for vegetated pixels.
#' @param enonveg Emissivity value for non-vegetated pixels.
#' @param eveg Emissivity value for vegetated pixels.
#' @param pveg Logical; if TRUE, proportion of vegetation is given as an output.
#'
#' @details Computes the per-pixel emissivity based on the modified
#' NDVI threshold method (Sobrino et al., 2008). Threshold values
#' for non-vegetated and vegetated pixels can be defined by the user.
#' Defaults are nonveg <= 0.2 and veg >= 0.5, based on the NDVI values.
#' Emissivity values for non-vegetated and vegetated pixels can be
#' also defined. Defaults are enonveg = 0.95 and eveg = 0.99.  pveg
#' is logical. If TRUE, the proportion of vegetation layer is added
#' to the output. Default is FALSE.
#'
#' @return Raster layer containing emissivity values. If pveg = TRUE,
#' a list with emissivity and pveg raster layers.
#'
#' @references Sobrino, J. A., Jim?nez-Mu?oz, J. C., S?ria, G.,
#' Romaguera, M., Guanter, L., Moreno, J. & Mart?nez, P. (2008).
#' Land surface emissivity retrieval from different VNIR and TIR
#' sensors. IEEE Transactions on Geoscience and Remote Sensing,
#' 46(2): 316-327.
#'
#' @examples
#'# For default non-vegetation and vegetation emissivity values
#'emiss <- emissivity(x, enonveg = 0.95, eveg = 0.99, pveg = TRUE)
#'
#' @export
#' @importFrom raster merge
#'
#'
emissivity <- function(x, nonveg = 0.2, veg = 0.5, enonveg = 0.95, eveg = 0.99, pveg= FALSE) {
  x[x < nonveg] <-0
  x[x > veg] <-1
  mx <- x
  x[x > nonveg & x < veg] <- NA
  Evs <- x
  Evs[Evs == 0] <- enonveg
  Evs[Evs == 1] <- eveg
  mx[mx < nonveg | mx > veg] <- NA
  mx <- ((mx - 0.2) / (0.5 - 0.2))^2
  Pv <- merge(mx, x)
  names(Pv) <- "Prop.veg"
  Emx <- mx
  Emx <- enonveg + (eveg - enonveg) * Pv
  E <- merge(Emx, Evs)
  names(E) <- "Emissivity"
  if (isTRUE(pveg))
  { r <- list(E, Pv)
  }else{
    r <- E
  }
  return(r)
}


#' @title landsat_lst
#'
#' @description Compute land surface temperature.
#'
#' @param x Brightness temperature layer.
#' @param y Emissivity layer.
#' @param sensor Landsat sensor of the input bands. Possible values are "L4", "L5", "L7" and "L8".
#' @param conv Logical; if TRUE, units are converted to Celsius degrees.
#'
#' @details Compute the per-pixel LST of a Landsat brightness
#' temperature layer based on the inversion of the Planck function,
#' which corrects the bias present in brightness temperature values,
#' in which is assumed that the land surface is a black body with an
#' emissivity of 1.  Landsat sensor must be specified in order to
#' apply the proper center wavelength values for the corresponding
#' Landsat TIR band. If parameter conv = TRUE, temperature units
#' are convert from Kelvin to Celsius degrees. This is the default.
#'
#' @return LST layer in Celsius or Kelvin degrees.
#'
#' @references USGS. (2019). Landsat 7 data users handbook version 2.
#' USGS Earth Resources Observation and Science (EROS).
#' Sioux Falls, South Dakota. USA. 139.
#'
#' USGS. (2019). Landsat 8 data users handbook version 4. USGS
#' Earth Resources Observation and Science (EROS). Sioux Falls,
#' South Dakota. USA. 106.
#'
#' Stathopoulou, M. and Cartalis, C. (2007). Daytime urban heat
#' islands from Landsat ETM+ and Corine land cover data: An
#' application to major cities in Greece. Sol. Energy. 81 358-368.
#'
#' @examples
#'# For LST derived from Landsat 8 sensor in Celsius degrees.
#'L8lst <- landsat_lst(Brtempb10, emiss, Sensor = "L8", conv = TRUE)
#'
#' @export
#'
#'
landsat_lst <- function(x, y, sensor = NULL, conv = TRUE){
  if (is.null(sensor)){
    stop("Sensor must be provided")
  }else if (grepl("L4|L5|L7", sensor)){
    cwl <- 11.45
  }else if (sensor == "L8"){
    cwl <- 10.895
  }else{
    stop("Sensor must be one of L4, L5, L7 or L8")
  }
  r <- x / (1 + (cwl * x / 14388) * log(y))
  if (isTRUE(conv)){
    r <- r - 273.15
  }else{
    return(r)
  }}
