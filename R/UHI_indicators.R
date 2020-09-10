#' @title uhi_stats
#'
#' @description Computes Urban Heat Island (UHI) indicators based
#' on a LST raster and spatial limits defined by a raster layer.
#'
#' @param x LST raster layer.
#' @param y Raster indicating spatial limits (e.g., land cover).
#' @param id Numeric: ID of the spatial limit of the urban area (e.g., urban class).
#'
#' @details Computes LST descriptive statistics (i.e., min, mean,
#' max and sd) and the difference and magnitude UHI indicators.
#' Difference is computed as the mean LST urban minus mean LST
#' other(s) and magnitude is the maximum LST minus mean LST for each
#' class. x and y must have the same projection and id must be a
#' numeric value.
#'
#' @return A data frame with descriptive statistics and difference
#' and magnitude UHI indicators.
#'
#' @references Dousset, B. and Gourmelon, F. (2003). Satellite
#' multi-sensor data analysis of urban surface temperatures and
#' landcover. ISPRS Journal of Photogrammetry and Remote
#' Sensing. 58(1-2), 43-54.
#'
#' Chen, X. L., Zhao, H. M., Li, P. X. and Yin, Z. Y. (2006).
#' Remote sensing image-based analysis of the relationship
#' between urban heat island and land use/cover changes. Remote
#' Sensing of Environment. 104(2), 133-146.
#'
#' Rajasekar, U. and Weng, Q. H. (2009). Urban heat island monitoring
#' and analysis using a non-parametric model: A case study of
#' Indianapolis. ISPRS Journal of Photogrammetry and Remote
#' Sensing. 64(1), 86-96.
#'
#' Zhou, J., Li, J. and Yue, J. (2010). Analysis of urban heat
#' island (UHI) in the Beijing metropolitan area by time-series
#' MODIS data. IEEE International Geoscience and Remote Sensing
#' Symposium (IGARSS 2010) (pp. 3327-3330).
#'
#' @examples
#' \dontrun{# For urban land use class with id = 2
#' uhiind <- uhi_stats(lstL8, landuse, id = 2)}
#'
#' @export
#' @importFrom raster crop resample zonal
#'
#'
uhi_stats <- function(x, y, id = NULL){
  x <- crop(x, y)
  x <- resample(x, y, "bilinear")
  mins <- round(zonal(x, y, na.rm = TRUE, 'min'), 2)
  means <- round(zonal(x, y, na.rm = TRUE, 'mean'), 2)
  maxs <- round(zonal(x, y, na.rm = TRUE, 'max'), 2)
  sds <- round(zonal(x, y, na.rm = TRUE, 'sd'), 2)
  df <- Reduce(merge, list(mins, means, maxs, sds))
  names(df)[1] <- "id"
  df$difference <- round(df[which(df$id == id), names(df) %in% "mean"] - df$mean, 2)
  df[which(df$id == id), names(df) %in% "dif"] <- ""
  df$magnitud <- round(df$max - df$mean, 2)
  return(df)
}


#' @title hia
#'
#' @description Computes the Heat Island Area (HIA) indicator based
#' on a LST raster layer.
#'
#' @param x LST raster layer.
#' @param y Optional. Shapefile polygon of the city extent.
#'
#' @details Computes the Hot Island Area (HIA) indicator in hectares.
#' If y is supplied, it must have the same projection as x.
#' When y is not defined, it is assumed that the LST raster is
#' already cropped to the city extent.
#'
#' @return A list with the LST raster layer representing the
#' areas with values higher than the HIA threshold, LST mean,
#' LST sd, HIA threshold and HIA in hectares.
#'
#' @references Zhang, J. Q., and Wang, Y. P. (2008). Study of the
#' relationships between the spatial extent of surface urban heat
#' islands and urban characteristic factors based on Landsat ETM
#' plus data. Sensors, 8(11), 7453-7468.
#'
#' @examples
#' \dontrun{# Specifying city polygon
#' HIA.City <- hia(Llstl8, citypol)}
#'
#' @export
#' @importFrom raster crop trim mask cellStats area
#'
#'
hia <- function(x, y = NULL){
  if (!is.null(y)){
    x <- crop(x, y)
    x <- trim(mask(x, y))
  }
  means <- round(cellStats(x, na.rm = TRUE, stat='mean'), 2)
  sds <- round(cellStats(x, na.rm = TRUE, stat='sd'), 2)
  tr <- means + sds
  x[x <= tr] <- NA
  x <- trim(x)
  a <- suppressWarnings(sum(tapply(area(x), x[], sum)/10000))
  f <- list(x, means, sds, tr, a)
  names(f) <- c("HIA layer", "Mean", "SD",  "HIA threshold", "HIA (ha)")
  return(f)
}


#' @title getis
#'
#' @description Compute a hot and cold spots analysis employing
#' Getis-Ord Gi* statistic and the False Discovery Rate (FDR)
#' correction.
#'
#' @param x Raster layer with values to analyze.
#' @param dist Distance for local neighborhood size from each cell.
#' @param p P-value to reject the null hypothesis.
#'
#' @details Compute a hot and cold spots analysis based on the
#' Getis-Ord Gi* statistic and the False Discovery Rate (FDR)
#' correction. Center cell is included in the local neighborhood
#' size and the limit is the distance specified by dist employing
#' queen's case. False Discovery Rate (FDR) correction is applied
#' to potentially reduce the critical p-value thresholds in order
#' to account for multiple testing and spatial dependency. Possible
#' p values to reject the null hypothesis are 0.1, 0.05, 0.01,
#' 0.001 and 0.0001.
#'
#' @return A shapefile with the resulting z-scores, FDR values
#' and label of cluster (i.e., hot spot, cold spot or no sig).
#'
#' @references Benjamini, Y., and Hochberg, Y. (1995). Controlling
#' the false discovery rate: a practical and powerful approach
#' to multiple testing. Journal of the Royal Statistical Society
#' Series B, 57, 289-300.
#'
#' Getis, A. and Ord, J. K. (1996). Local spatial statistics:
#' an overview. In P. Longley and M. Batty (eds) Spatial analysis:
#' modelling in a GIS environment (Cambridge: Geoinformation
#' International), 261-277.
#'
#' @examples
#' \dontrun{# For an LST layer derived from Landsat 8
#' spots <- getis(lstl8, dist = 70, p = 0.05)}
#'
#' @export
#' @importFrom usdm lisa
#' @importFrom raster rasterToPolygons
#' @importFrom stats p.adjust pnorm
#'
#'
getis <- function(x, dist = NULL, p = 0.05){
  if (isFALSE(grepl("0.0001|0.001|0.01|0.05|0.1", p))){
    stop("P is not one of 0.1, 0.05, 0.01, 0.001 or 0.0001")
  }
  g <- lisa(x, d1 = 0, d2 = dist, statistic= "G*")
  pv <- round(2*pnorm(-abs(getValues(g))), 7)
  pv <- as.data.frame(pv)
  FDR <- round(p.adjust(pv[,1], "fdr"), 7)
  g <- round(g, 2)
  shp <- rasterToPolygons(g, na.rm = T)
  names(shp) <- "Z.scores"
  shp$FDR <- FDR
  shp$cluster <- ifelse(shp$Z.scores > 0 & shp$FDR < p, "Hot spot",
                        ifelse(shp$Z.scores < 0 & shp$FDR < p, "Cold spot", "No sig"))
  return(shp)
}
