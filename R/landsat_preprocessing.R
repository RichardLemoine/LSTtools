#' @title untar_landsat
#'
#' @description Uncompress .tar.gz folders containing
#' Landsat images as downloaded from NASA EarthExplorer.
#'
#' @param orig Path or folder containing the .tar.gz Landsat images.
#' @param dest Path or folder to export the uncompressed Landsat images.
#'
#' @details Uncompress .tar.gz files containing Landsat
#'images and save them in a folder with the original
#'name of the compressed file. If dest is not specified,
#'default is the same as orig.
#' @return Uncompressed Landsat bands and metadata.
#'
#' @references https://earthexplorer.usgs.gov/
#' @examples
#' # Export files to the same folder as input
#' untar_landsat("C:/Landsat/2017")
#'
#' # Export files to a folder different than the input
#' untar_landsat("C:/Landsat/2017", "C:/Landsat")
#' @export
#' @importFrom utils untar
#'
#'
untar_landsat <- function(orig, dest = orig) {
  setwd(orig)
  checktar <- list.files(pattern = ".gz$",
                         recursive = FALSE,
                         full.names = TRUE)
  {
    if (identical(checktar, character(0))) {
      print("no .tar files in folder")
    } else{
      if (dest != orig) {
       sapply(checktar, function(i)
          untar(i, exdir = paste(dest, sep = "", gsub(
            "./", "/", gsub("\\.tar.gz$", "", i)
          ))))
      } else{
        sapply(checktar, function(i)
          untar(i, exdir = gsub("\\.tar.gz$", "", i)))
      }
    }
  }
}


#' @title toa_rad
#'
#' @description Convert the DN contained in a Landsat TIR band to TOA radiance.
#'
#' @param x Image band in DN to be converted.
#' @param band Character. Number of the band to convert.
#' @param mult Radiance multiplicative band rescaling factor.
#' @param add Radiance additive band rescaling factor.
#'
#' @details Convert the DN of a Landsat TIR band to TOA radiance
#' employing the radiance multiplicative and additive band
#' rescaling factors. If band is specified, the function
#' reads the metadata (.txt file) directly from the work directory
#' (folder containing bands as downloaded from NASA EarthExplorer)
#' and automatically extracts the radiance multiplicative and additive
#' rescaling factors. These scaling factors can be manually defined
#' employing mult and add parameters. In this case, band is ignored.
#' @return Raster layer with TOA radiance values.
#'
#' @references USGS. (2019). Landsat 8 data users handbook version 4.
#' USGS Earth Resources Observation and Science (EROS). Sioux Falls,
#' South Dakota. USA. 106.
#' @examples
#' # For Landsat 8 band 10 defining band and extracting scaling
#' # factors from metadata
#' toaradB10 <- toa_rad(B10, band = "10")
#' # For Landsat 8 band 10 defining manually the multiplicative
#' # and additive scaling factors
#' toaradB10 <- toa_rad(B10, mult = 0.00033420, add = 0.1)
#'
#' @export
#'
#'
toa_rad <- function(x, band = NULL, mult = NULL, add = NULL) {
if (is.null(mult) & is.null(add)){
  MTL <- list.files(pattern = "...MTL.txt$")
  MTL <- readLines(MTL)
  Radmb <- grep(paste("RADIANCE_MULT_BAND_", band, " ", sep = ""), MTL, value = TRUE)
  Radmb <- as.numeric(gsub(".*= ", "", Radmb))
  Radab <- grep(paste("RADIANCE_ADD_BAND_", band, " ", sep = ""), MTL, value = TRUE)
  Radab <- as.numeric(gsub(".*= ", "", Radab))
  x[x == 0] <- NA
  x <- (Radmb * x + Radab)
}else{
  x[x == 0] <- NA
  x <- (mult * x + add)
}
}


#' @title toa_ref
#'
#' @description Convert the DN contained in a Landsat band to TOA reflectance.
#'
#' @param x Image band in DN to be converted.
#' @param band Character. Number of the Landsat band to convert.
#' @param mult Reflectance multiplicative band rescaling factor.
#' @param add Reflectance additive band rescaling factor.
#'
#'@details Convert the DN of a Landsat optical band to TOA reflectance
#'employing the reflectance multiplicative and additive band rescaling
#'factors. If band is specified, the function reads the metadata (.txt)
#'directly from the work directory (folder containing bands as
#'downloaded from NASA EarthExplorer) and automatically extracts the
#'multiplicative and additive rescaling factors. These scaling factors
#'can be manually defined employing mult and add parameters. In this
#'case, band is ignored.
#' @return Raster layer with TOA reflectance values.
#'
#' @references USGS. (2019). Landsat 8 data users handbook version 4.
#' USGS Earth Resources Observation and Science (EROS). Sioux Falls,
#' South Dakota. USA. 106.
#' @examples
#' # For Landsat 8 band 4 defining band and extracting scaling
#' factors from metadata
#' toarefB4 <- toa_ref(B4, band = "4")
#' # For Landsat 8 band 4 defining manually the multiplicative
#' and additive scaling factors
#' toarefB4 <- toa_ref(B4, mult = 0.00033420, add = 0.1)
#'
#' @export
#'
#'
toa_ref <- function(x, band= NULL, mult = NULL, add = NULL) {
if (is.null(mult) & is.null(add)){
  MTL <- list.files(pattern ="...MTL.txt$")
  MTL <- readLines(MTL)
  Refmb <- grep(paste("REFLECTANCE_MULT_BAND_", band, " ", sep = ""), MTL, value=TRUE)
  Refmb <- as.numeric(gsub(".*= ","", Refmb))
  Refab <- grep(paste("REFLECTANCE_ADD_BAND_", band, " ", sep = ""), MTL, value=TRUE)
  Refab <- as.numeric(gsub(".*= ","", Refab))
  x[x == 0] <- NA
  x <- (Refmb * x + Refab)
}else{
  x[x == 0] <- NA
  x <- (mult * x + add)
}
}


#' @title toa_refsun
#'
#' @description Convert the DN contained in a Landsat band to TOA reflectance.
#'
#' @param x Image band in DN to be converted.
#' @param band Character. Number of the Landsat band to convert.
#' @param mult Reflectance multiplicative band rescaling factor.
#' @param add Reflectance additive band rescaling factor.
#' @param sune Scene center sun elevation angle
#'
#'@details Convert the DN of a Landsat optical band to TOA reflectance
#'employing the reflectance multiplicative and additive band rescaling
#'factors with a correction for the sun angle. If band is specified,
#'the function reads the metadata file (.txt) directly from the work
#'directory (folder containing bands as downloaded from NASA
#'EarthExplorer) and automatically extracts the multiplicative and
#'additive rescaling factors and the scene sun elevation angle. These
#'parameters can be manually defined employing mult, add and sune
#'parameters. In this case, band is ignored.
#' @return Raster layer with TOA reflectance values corrected for the sun angle.
#'
#' @references USGS. (2019). Landsat 8 data users handbook version 4.
#' USGS Earth Resources Observation and Science (EROS). Sioux Falls,
#' South Dakota. USA. 106.
#' @examples
#' # For Landsat 8 band 4 defining band and extracting scaling
#' factors from metadata
#' toarefsunB4 <- toa_refsun(B4, band = "4")
#' # For Landsat 8 band 4 defining manually the multiplicative and
#' additive scaling factors and scene sun elevation angle
#' toarefsunB4 <- toa_refsun (B4, mult = 0.00033420,
#' add = 0.1, sune = 37.22752222)
#'
#' @export
#'
#'
toa_refsun <- function(x, band= NULL, mult = NULL, add = NULL, sune = NULL) {
  if (is.null(mult) & is.null(add) & is.null(sune)){
    MTL <- list.files(pattern ="...MTL.txt$")
    MTL <- readLines(MTL)
    Refmb <- grep(paste("REFLECTANCE_MULT_BAND_", band, " ", sep = ""), MTL, value=TRUE)
    Refmb <- as.numeric(gsub(".*= ","", Refmb))
    Refab <- grep(paste("REFLECTANCE_ADD_BAND_", band, " ", sep = ""), MTL, value=TRUE)
    Refab <- as.numeric(gsub(".*= ","", Refab))
    sune <- grep("SUN_ELEVATION", MTL, value=TRUE)
    sune <- as.numeric(gsub(".*= ","", sune))
    suntheta <- sin(sune * pi/180)
    x[x == 0] <- NA
    x <- (Refmb * x + Refab) / suntheta
  }else{
    suntheta <- sin(sune * pi/180)
    x[x == 0] <- NA
    x <- (mult * x + add) / suntheta
  }
}


#' @title br_temp
#'
#' @description Convert the DN contained in a Landsat TIR band
#' to TOA brightness temperature.
#'
#' @param x Image band in DN to be converted.
#' @param band Character. Number of the Landsat band to convert.
#' @param conv Logical; if TRUE, units are converted to Celsius degrees.
#' @param mult Radiance multiplicative band rescaling factor.
#' @param add Radiance additive band rescaling factor.
#' @param k1 k1 thermal conversion constant.
#' @param k2 k2 thermal conversion constant.
#'
#'@details Convert the DN of a Landsat TIR band to TOA brightness
#'temperature in Kelvin or Celsius degrees employing the radiance
#'multiplicative and additive band rescaling factors and K1 and K2
#'constants. If band is specified, the function reads the
#'metadata (.txt) directly from the work directory (folder
#'containing bands as downloaded from NASA EarthExplorer) and
#'automatically extracts the multiplicative and additive rescaling
#'factors and k1 and k2 constants. These scaling factors and
#'constants can be manually defined employing mult, add, k1 and
#'k2 parameters. In this case band is ignored. If parameter
#'conv = TRUE, temperature units are convert to Celsius degrees.
#'This is the default.
#' @return Raster layer with TOA brightness temperature values
#' in Kelvin or Celsius degrees.
#'
#' @references USGS. (2019). Landsat 8 data users handbook version 4.
#' USGS Earth Resources Observation and Science (EROS). Sioux Falls,
#' South Dakota. USA. 106.
#' @examples
#'# For Landsat 8 band 10 defining band and extracting scaling
#'factors and constants from metadata
#'brtempB10 <- br_temp(B10, band = "10", conv = TRUE)
#'# For Landsat 8 band 10 defining manually the multiplicative
#'and additive scaling factors and the k1 and k2 constants
#'brtempB10 <- br_temp(B10, conv = TRUE, mult = 0.00033420,
#'add = 0.1, k1 = 774.8853, k2 = 1321.0789)
#'
#' @export
#'
#'
br_temp <- function(x, band = "", conv = TRUE, mult = NULL, add = NULL, k1 = NULL, k2 = NULL) {
  if (is.null(mult) & is.null(add) & is.null(k1) & is.null(k2)){
    MTL <- list.files(pattern ="...MTL.txt$")
    MTL <- readLines(MTL)
    Radmb <- grep(paste("RADIANCE_MULT_BAND_", band, " ", sep = ""), MTL, value=TRUE)
    Radmb <- as.numeric(gsub(".*= ","", Radmb))
    Radab <- grep(paste("RADIANCE_ADD_BAND_", band, " ", sep = ""), MTL, value=TRUE)
    Radab <- as.numeric(gsub(".*= ","", Radab))
    K1 <- grep(paste("K1_CONSTANT_BAND_", band, " ", sep = ""), MTL, value=TRUE)
    K1 <- as.numeric(gsub(".*= ","", K1))
    K2 <- grep(paste("K2_CONSTANT_BAND_", band, " ", sep = ""), MTL, value=TRUE)
    K2 <- as.numeric(gsub(".*= ","", K2))
    x[x == 0] <- NA
    x <-  K2 / log(K1 / (Radmb * x + Radab) + 1)
  }else{
    x[x == 0] <- NA
    x <-  k2 / log(k1 / (mult * x + add) + 1)
  }
  if (isTRUE(conv))
  {x - 273.15
  }else{
    return(x)
  }}
