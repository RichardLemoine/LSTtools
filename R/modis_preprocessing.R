#' @title hdf_extract
#'
#' @description Extract and scale MODIS layers from a .hdf MODIS LST or
#' vegetation index product as downloaded from NASA EarthExplorer.
#'
#' @param x .hdf MODIS file.
#' @param dest Path or folder to export the extracted bands.
#' @param conv Logical; if TRUE, temperature units are converted to Celsius degrees.
#'
#'@details Extract MODIS layers from a .hdf MODIS product retaining
#'the product name, acquisition date, tile identifier, collection
#'version and julian date of production in the output name. The
#'extraction of names is suited specifically for vegetation indices
#'(MXD13) and LST and Emissivity (MXD11) MODIS products as downloaded
#'from NASA EarthExplorer. Products MXD11_L2 and MXD21_L2 are not
#'supported. The function scales the values of each band. If parameter
#'conv = TRUE, temperature units are converted to Celsius degrees for
#'LST bands. This is the default.
#'
#'A negative sign of the viewing angle in MXD11 products means MODIS
#'viewing the grid from east. The view zenith angle itself is always
#'a positive number, the zenith angle from nadir. The information of
#'MODIS viewing the grid from east or west may be important in
#'understanding the view angle effect in the temporal variations in
#'LST, especially in rugged regions.
#'
#' @return Exported .tif layers.
#'
#'@references https://earthexplorer.usgs.gov/
#'
#' Wan, Z. (1999). "MODIS Land-Surface Temperature Algorithm
#' Theoretical Basis Document (LST ATBD) Version 3.3, April 1999".
#'
#' Wolfe, R.E., D.P. Roy, E. Vermote. (1998). "MODIS land data storage,
#' gridding and compositing methodology: level 2 grid".
#' IEEE Trans. Geosci. Remote Sens., v36, n4, pp. 1324-1338.
#'
#' @examples
#' \dontrun{# For MOD11A2 LST product saving .tif bands in a different folder
#' hdf_extract("MOD11A2.A2019169.h08v07.006.2019178033848.hdf", "C:/MODISLST", conv = TRUE)}
#'
#' @export
#'
#' @importFrom gdalUtils get_subdatasets
#' @importFrom raster raster writeRaster
#' @importFrom rgdal readGDAL
#'
#'
hdf_extract <- function(x, dest = getwd(), conv = TRUE) {
  sds <- get_subdatasets(x)
  for (i in 1:length(sds)){
    if (grepl("UNKNOWN", sds[i])){
      next
    }
    name <- unlist(strsplit(basename(sds[i]), "\\:"))
    name <- gsub("[.]", "_", gsub( " ", "_", gsub("hdf", "", paste(name[3], name[5], sep = ""))))
    if(grepl("13Q1|13A1|13A2|13A3|13C1|13C2", name) & grepl("angle", name)){
      r <- suppressWarnings(raster(readGDAL(sds[i], as.is = TRUE, silent = TRUE)))
      r <- r * 0.01
    }else{
      r <- suppressWarnings(raster(readGDAL(sds[i], as.is= FALSE, silent = TRUE)))
      if (grepl("NDVI|EVI|red|NIR|blue|MIR", name)){
        r <- r / 1e8
      }else if (grepl("LST", name) & !grepl("Error", name)){
        if (isTRUE(conv)){
          r <- r - 273.15
        }
      }}
    dir.create(paste0(dest, "/", sub(".hdf", "", x)), showWarnings = FALSE)
    writeRaster(r, paste(dest, "/", sub(".hdf", "", x), "/", name, ".tif", sep = ""), overwrite=TRUE, format="GTiff")
  }}


#' @title lst_filter
#'
#' @description Filter pixels of MODIS LST products based on the
#' LST error and angle layers.
#'
#' @param x .hdf MODIS file corresponding to a LST product.
#' @param time A character specifying the time of the data to filter.
#' Possible values are "day" or "night".
#' @param flag LST error bit flag or accuracy tolerance from 1-4.
#' @param angle Viewing zenith angle tolerance to filter pixels (in degrees).
#' @param conv Logical; if TRUE, units are converted to Celsius degrees.
#'
#'@details Filter the pixels of a day or night data of a MODIS LST
#'product, based on tolerance values of LST error and zenith viewing
#'angle defined by the user. If parameter conv = TRUE, temperature
#'units are convert to Celsius degrees. This is the default. Products
#'MXD11_L2 and MYD21_L2 are not supported.
#'
#'LST error bit flag to filter pixels must be defined as an integer
#'from 0-3 corresponding to: 0=00, 1=01, 2=10 and 3=11 in the 7 & 6
#'bit flags for 8 bit QC layers and 15 & 14 bit flags for 16 bit QC
#'layers. LST error flags differ between MXD11 and MXD21 products.
#'In MXD11 products, the flag represents average LST error values
#'in Kelvin degrees, with an increase of 1 degree. In MXD21 products,
#'the level of error is represented as accuracy in Kelvin degrees,
#'with increases of 0.5 degrees. See table 13 (MXD11; Wan, 2013) and
#'table 10 (MXD21; Hulley et al., 2016) in the MXD11 and MXD21 user
#'guides, respectively.
#'
#'A negative sign of the viewing angle means MODIS viewing the grid
#'from east. The view zenith angle itself is always a positive number,
#'the zenith angle from nadir. The information of MODIS viewing
#'the grid from east or west may be important in understanding the
#'view angle effect in the temporal variations in LST, especially
#'in rugged regions.
#'
#' @return List of three raster layers: LST layer in Kelvin or
#' Celsius degrees, error or accuracy layer and viewing zenith
#' angle layer. Only pixels that are not NA in the original LST
#' layer are included in the error and view zenith angle layers.
#'
#'@references Hulley, G., Freepartner, R., Malakar, N. & Sarkar, S.
#'(2016), Moderate Resolution Imaging Spectroradiometer (MODIS) - Land
#'Surface Temperature and Emissivity Product (MxD21) User Guide
#'Collection-6. Pasadena, EUA, 29.
#'
#'Hulley, G., Hook, S., & Hughes, C. (2012). MODIS MxD21 Land
#'Surface Temperature and Emissivity Algorithm Theoretical Basis
#'Document. In: Jet Propulsion Laboratory, California Institute
#'of Technology, JPL Publication 12-17, August, 2012.
#'
#'Z. Wan, & Z.-L. Li. (2011), Chapter 25, MODIS land surface temperature
#'and emissivity. In B. Ramachandran, C. O. Justice, & M. J.
#'Abrams (Eds.), Land remote sensing and global environmental
#'change, NASA's Earth observing system and the science of ASTER
#'and MODIS, NASA's Earth observing system and the science of ASTER
#'and MODIS. New York Dordrecht Heidelberg London: Springer,
#'http://dx.doi.org/10.1007/978-1-4419-6749-7.
#'
#'Z. Wan. (2013), Collection-6 MODIS Land Surface Temperature Products
#'User's guide. Santa Barbara, USA, 33.
#'
#'Z. Wan, Y. Zhang, Q. Zhang, and Z.-L. Li. (2002), "Validation of
#'the land-surface temperature products retrieved from Terra Moderate
#'Resolution Imaging Spectroradiometer data", Remote Sens.
#'Environ., 83, 163-180.
#'
#'Gawuc, L., & Struzewska, J. (2016). Impact of MODIS quality
#'control on temporally aggregated urban surface temperature and
#'long-term surface urban heat island intensity. Remote Sensing,
#'8(5). https://doi.org/10.3390/rs8050374
#'
#'Hu, L., Brunsell, N. A., Monaghan, A. J., Barlage, M.,
#'& Wilhelmi, O. V. (2014). How can we use MODIS land surface
#'temperature to validate long-term urban model simulations?
#'Journal of Geophysical Research, 119(6), 3185-3201.
#'https://doi.org/10.1002/2013JD021101
#'
#' @examples
#' \dontrun{# For LST day MXD11 product, filtering pixels with LST error
#' # <= 1 and view zenith angle <= 35
#' x <- "MOD11A1.A2019305.h14v09.006.2019306084028.hdf"
#' lstfiltered  <- lst_filter(x, time = "day", flag  = 1, angle = 35, conv = TRUE)}
#'
#' @export
#' @importFrom gdalUtils get_subdatasets
#' @importFrom raster raster writeRaster values reclassify getValues
#' @importFrom rgdal readGDAL
#'
#'
lst_filter <- function(x, time = "day", flag, angle, conv = TRUE){
  sds <- get_subdatasets(x)
  if(grepl("11A1|11A2|11B1|11B2|11B3|11C1", x)) {
    op <- "<="
    bta <- 25
    btb <- 26
    if (time == "day"){
      LST <- suppressWarnings(raster(readGDAL(sds[1], as.is = FALSE, silent = TRUE)))
      qc <- suppressWarnings(raster(readGDAL(sds[2], as.is = FALSE, silent = TRUE)))
      zenit_abs_angle <- suppressWarnings(raster(readGDAL(sds[4], as.is = FALSE, silent = TRUE)))
      names(zenit_abs_angle) <- "View.angle"
    }else if (time == "night"){
      LST <-  suppressWarnings(raster(readGDAL(sds[5], as.is = FALSE, silent = TRUE)))
      qc <- suppressWarnings(raster(readGDAL(sds[6], as.is = FALSE, silent = TRUE)))
      zenit_abs_angle <- suppressWarnings(raster(readGDAL(sds[8], as.is = FALSE, silent = TRUE)))
      names(zenit_abs_angle) <- "View.angle"
    }
  } else if(grepl("11C2|11C3", x))  {
    op <- "<="
    bta <- 25
    btb <- 26
    if (time == "day"){
      LST <- suppressWarnings(raster(readGDAL(sds[1], as.is = FALSE, silent = TRUE)))
      qc <- suppressWarnings(raster(readGDAL(sds[2], as.is = FALSE, silent = TRUE)))
      zenit_abs_angle <- suppressWarnings(raster(readGDAL(sds[4], as.is = FALSE, silent = TRUE)))
      names(zenit_abs_angle) <- "View.angle"
    }else if (time == "night"){
      LST <-  suppressWarnings(raster(readGDAL(sds[6], as.is = FALSE, silent = TRUE)))
      qc <- suppressWarnings(raster(readGDAL(sds[7], as.is = FALSE, silent = TRUE)))
      zenit_abs_angle <- suppressWarnings(raster(readGDAL(sds[9], as.is = FALSE, silent = TRUE)))
      names(zenit_abs_angle) <- "View.angle"
    }
  } else if(grepl("MYD21A1D", x))  {
    op <- ">="
    bta <- 17
    btb <- 18
    if (time == "day"){
      LST <- suppressWarnings(raster(readGDAL(sds[1], as.is = FALSE, silent = TRUE)))
      qc <- suppressWarnings(raster(readGDAL(sds[2], as.is = FALSE, silent = TRUE)))
      zenit_abs_angle <- suppressWarnings(raster(readGDAL(sds[3], as.is = FALSE, silent = TRUE)))
      names(zenit_abs_angle) <- "View.angle"
    }else if (time == "night"){
      stop('This product does not contain night LST measurement. Introduce product MXD21A1N to process this information')
    }
  } else if(grepl("MYD21A1N", x))  {
    op <- ">="
    bta <- 17
    btb <- 18
    if (time == "day"){
      stop('This product does not contain day LST measurement. Introduce product MXD21A1D to process this information')
    }else if (time == "night"){
      LST <-  suppressWarnings(raster(readGDAL(sds[1], as.is = FALSE, silent = TRUE)))
      qc <- suppressWarnings(raster(readGDAL(sds[2], as.is = FALSE, silent = TRUE)))
      zenit_abs_angle <- suppressWarnings(raster(readGDAL(sds[3], as.is = FALSE, silent = TRUE)))
      names(zenit_abs_angle) <- "View.angle"
    }
  } else if(grepl("MYD21A2", x))  {
    op <- ">="
    bta <- 25
    btb <- 26
    if (time == "day"){
      LST <- suppressWarnings(raster(readGDAL(sds[1], as.is = FALSE, silent = TRUE)))
      qc <- suppressWarnings(raster(readGDAL(sds[2], as.is = FALSE, silent = TRUE)))
      zenit_abs_angle <- suppressWarnings(raster(readGDAL(sds[3], as.is = FALSE, silent = TRUE)))
      names(zenit_abs_angle) <- "View.angle"
    }else if (time == "night"){
      LST <-  suppressWarnings(raster(readGDAL(sds[5], as.is = FALSE, silent = TRUE)))
      qc <- suppressWarnings(raster(readGDAL(sds[6], as.is = FALSE, silent = TRUE)))
      zenit_abs_angle <- suppressWarnings(raster(readGDAL(sds[7], as.is = FALSE, silent = TRUE)))
      names(zenit_abs_angle) <- "View.angle"
    }
  } else {
    stop('Non-valid MODIS product')
  }
  values_qc <- unique(values(qc))
  m <- matrix(ncol=2,nrow=length(values_qc),NA)
  m[,1]<- values_qc

  for (i in 1:nrow(m)){
    bt <- substr(paste(as.integer(rev(intToBits(m[i,1]))), collapse = ""), bta, btb) #25-26 bits 7&6. 17-18 bits 15&14
    n <- strtoi(bt, 2L)
    m[i,2] <- n
  }
  flag_LST <- reclassify(qc, m)
  if (grepl("MYD21A1D|MYD21A1N|MYD21A2", x))  {
    names(flag_LST) <- "LST.accuracy"
  }else{
    names(flag_LST) <- "LST.error"
  }
  flag_LST[is.na(LST)] <- NA
  maskLST <- do.call(op, list(flag_LST, flag))
  maskLST[maskLST==0] <- NA
  zenit_abs_angle[is.na(flag_LST)] <- NA
  mask_angle <- zenit_abs_angle >= -angle & zenit_abs_angle <= angle
  mask_angle[mask_angle==0] <- NA
  LST2 <- LST*maskLST*mask_angle
  names(LST2) <- "LST"
  if (isTRUE(conv)){
    LST2 <- LST2 - 273.15
  }
  outputs <- list(LST2,flag_LST,zenit_abs_angle)
  {if (all(is.na(getValues(outputs[[1]])))){
    warning('Empty LST layer created due to non-achievement of desired quality. Check error and angle layers to verify values')
  }}
  return(outputs)
}


#' @title veg_filter
#'
#' @description Filter pixels of MODIS vegetation index products
#' based on the reliability and/or usefulness layers.
#'
#' @param x .hdf MODIS file corresponding to a vegetation index product.
#' @param vi A character specifying the index to filter. Possible
#' values are "NDVI" or "EVI".
#' @param rel 	Logical; if TRUE, reliability level = "Good data" is used to filter pixels.
#' @param usef Usefulness level to filter pixels according to MODIS user's guide.
#' @param angle Viewing zenith angle tolerance to filter pixels (in degrees).
#'
#' @details Filter the pixels of a MODIS vegetation index product
#'based on reliability values if rel = TRUE (default) or usefulness
#'level and/or view zenith angle defined by the user.
#'
#'If rel = TRUE, only pixels with "good data" level of reliability
#'are included in the output and usefulness is not taken into
#'account. If rel = FALSE user can define the usefulness level
#'to be used to filter the pixels according to the bits 2-5 of
#'the QA MODIS layer, with values from 0 (0000; highest quality)
#'to 15 (1111; not useful; see Didan et al., 2015).
#'
#'Angle for all MXD13 products except MXD13C1 and MXD13C2 in which
#'this is not included can be used in the filter.
#'
#' @return List of three or four (when angle is included) raster
#' layers: NDVI/EVI layer, reliability layer, usefulness
#' layer and zenith view angle layer. Only pixels that are
#' not NA in the original vegetation index layer are included
#' in the reliability, usefulness and zenith view angle output layers.
#'
#' @references K. Didan, A. Barreto-Munoz, R. Solano & A. Huete.
#'(2015), MODIS Vegetation Index User's Guide (MOD13 Series)
#'Collection-6. Arizona, USA, 32.
#'
#'Land Processes Distributed Active Archive Center (LP DAAC),
#'USGS EROS Data Center. (2004). MODIS Land Data Operational
#'Product Evaluation (LDOPE) Tools, release 1.4. Arizona, USA, 103.
#'
#' @examples
#' \dontrun{# For NDVI MOD13Q1 product, filtering pixels with
#' usefulness <= 2 and view zenith angle <= 35
#' x <- "MOD13Q1.A2019305.h14v09.006.2019323202113.hdf"
#' NDVIfiltered <- veg_filter(x, vi = "NDVI", rel = FALSE, usef = 2, angle = 35)}
#'
#' @export
#' @importFrom gdalUtils get_subdatasets
#' @importFrom raster raster writeRaster values reclassify getValues
#' @importFrom rgdal readGDAL
#'
#'
veg_filter <- function(x, vi, rel = TRUE, usef = NULL, angle = NULL){
  sds <- get_subdatasets(x)
  if(!grepl("13A1|13A2|13Q1|13A3|13C1|13C2", x)){
    stop('Product is not one of MXD13Q1, MXD13A1, MXD13A2, MXD13A3, MXD13C1 or MXD13C2')
  }
  if(!grepl("NDVI|EVI", vi)){
    stop('VI is not one of NDVI or EVI')
  }
   if (vi == "NDVI") {
    vind <- suppressWarnings(raster(readGDAL(sds[1], as.is = TRUE, silent = TRUE)))
  } else if(vi == "EVI") {
    vind <- suppressWarnings(raster(readGDAL(sds[2], as.is = TRUE, silent = TRUE)))
  }
  qa <- suppressWarnings(raster(readGDAL(sds[3], as.is = FALSE, silent = TRUE)))
  if(!is.null(angle)){
    view_angle <- suppressWarnings(raster(readGDAL(sds[8], as.is = TRUE, silent = TRUE)))
    view_angle <- view_angle * 0.01
    names(view_angle) <- "View.angle"
  }
  if(grepl("13A1|13A2|13Q1", x)) {
    reliab <- suppressWarnings(raster(readGDAL(sds[12], as.is = FALSE, silent = TRUE)))
    names(reliab) <- "Reliability"
    if (isTRUE(rel)) {
      mask_r <- reliab == 0
      mask_r[mask_r == 0] <- NA
    } else {
      mask_r <- 1
    }}
  if (grepl("13A3", x))  {
    reliab <- suppressWarnings(raster(readGDAL(sds[11], as.is = FALSE, silent = TRUE)))
    names(reliab) <- "Reliability"
    if (isTRUE(rel)) {
      mask_r <- reliab == 0
      mask_r[mask_r == 0] <- NA
    } else {
      mask_r <- 1
    }}
  if (grepl("13C1|13C2", x))  {
    angle <- NULL
    reliab <- suppressWarnings(raster(readGDAL(sds[13], as.is = FALSE, silent = TRUE)))
    names(reliab) <- "Reliability"
    reliab[reliab > 4] <- NA
    if (isTRUE(rel)) {
      mask_r <- reliab == 0
      mask_r[mask_r == 0] <- NA
    } else {
      mask_r <- 1
    }}
  values_q <- unique(values(qa))
  values_q <- values_q[!is.na(values_q)]
  m <- matrix(ncol=2,nrow=length(values_q),NA)
  m[,1]<- values_q
  for (i in 1:nrow(m)){
    bt <- substr(paste(as.integer(rev(intToBits(m[i,1]))), collapse = ""), 27, 30)
    n <- strtoi(bt, 2L)
    m[i,2] <- n
  }
  usefulness_map <- reclassify(qa, m)
  names(usefulness_map) <- "Usefulness"
  usefulness_map[is.na(vind)] <- NA
  if (!isTRUE(rel)){
    mask_u <- usefulness_map <= usef
    mask_u[mask_u==0] <- NA
  }else{
    mask_u <- 1
  }
  if (!is.null(angle)){
    view_angle[is.na(usefulness_map)] <- NA
    mask_angle <- view_angle <= angle
    mask_angle[mask_angle==0] <- NA
    vind <- vind*mask_r*mask_u*mask_angle
    names(vind) <- vi
    vind <- vind * 0.0001
    outputs <- list(vind, reliab, usefulness_map, view_angle)
  }else{
    vind <- vind*mask_r*mask_u
    names(vind) <- vi
    vind <- vind * 0.0001
    outputs <- list(vind, reliab, usefulness_map)
  }
  {if (all(is.na(getValues(outputs[[1]])))){
    warning('Empty vegetation index layer created due to non-achievement of desired quality. Check reliability, usefulness and angle layers to verify values')
  }}
  return(outputs)
}
