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
#'# For urban land use class with id = 2
#'uhiind <- uhi_stats(lstL8, landuse, id = 2)
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
#'# Specifying city polygon
#'HIA.City <- hia(Llstl8, citypol)
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
#'# For an LST layer derived from Landsat 8
#'spots <- getis(lstl8, dist = 70, p = 0.05)
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


#' @title space_time_trends
#'
#' @description Compute a multi-temporal hot and cold spots analysis
#' employing Getis-Ord Gi* statistic and Mann-Kendall trend test
#' based on a space-time cube approach.
#'
#' @param x Raster stack with at least 5 bands, each one
#' corresponding to one evenly distributed time-step with non-NA
#' values in valid cells.
#' @param space Optional. Number of cells for spatial aggregation.
#' @param time Optional. Number of time-steps for temporal aggregation.
#' @param stat Statistic employed for spatial and/or temporal
#' aggregation. Possible values are "mean" or "max". Default value
#' is "mean".
#' @param dist Distance for local neighborhood size from each cell.
#' @param p P-value to reject the null hypothesis.
#'
#' @details Compute a multi-temporal hot and cold spots analysis
#' employing Getis-Ord Gi* statistic and Mann-Kendall trend test
#' based on a space-time cube approach. If space is defined, a
#' spatial aggregation (x, y dimensions) by the specified number
#' of cells is applied. If time is defined, a temporal aggregation
#' (temporal dimension) by the specified number of observations
#' (time-steps) is applied. Both of the aforementioned aggregations
#' are computed based on the statistic specified in stat. Possible
#' values for stat are "mean" and "max". Default value is "mean".
#'
#' Center cell is included in the local neighborhood size for Getis
#' Ord- Gi* and the search limit is the distance specified by dist
#' employing queen's case. If space is defined, the new raster
#' resolution after spatial aggregation must be considered when
#' defining dist.
#'
#' False Discovery Rate (FDR) correction is applied to potentially
#' reduce the critical p-value thresholds of the Getis-Ord Gi*
#' z-scores, in order to account for multiple testing and spatial
#' dependency. Possible p values to reject the null hypothesis
#' are 0.1, 0.05, 0.01, 0.001 and 0.0001.
#'
#' The non-parametric Mann-Kendall test is applied to test for a
#' monotonic trend based on the Theil-Sen slope modification.
#' Each cell represents a time-series. x must not have NA values
#' in valid cells and time-steps should be evenly distributed on
#' time for optimal results.
#'
#' Each cell is labeled based on: (1) its statistical significance
#' in the Getis-Ord Gi* FDR values and Mann-Kendall p-values, (2)
#' the symbol of the values of the z-scores of GETIS-Ord Gi* (hot,
#' cold or not significant), (3) the direction of the scores derived
#' from the Mann-Kendall test (positive, negative or no trend) and
#' (4) the percentage of time-steps on which cells showed to be hot
#' or cold spots:
#'
#' New hot spot: statistically significant hot spot only in the last
#' time-step.
#'
#' Increasing hot spot: statistically significant hot spot in >90%
#' of the time-steps with an increasing trend.
#'
#' Steady hot spot: statistically significant hot spot in >90% of
#' the time-steps without trend.
#'
#' Decreasing hot spot: statistically significant hot spot in >90%
#' of the time-steps with a decreasing trend.
#'
#' No pattern: does not meet any of the other criteria.
#'
#' Decreasing cold spot: statistically significant cold spot in >90%
#' of the time-steps with a decreasing trend.
#'
#' Steady cold spot: statistically significant cold spot in >90%
#' of the time-steps without trend.
#'
#' Increasing cold spot: statistically significant cold spot in >90%
#' of the time-steps with an increasing trend.
#'
#' New cold spot: statistically significant cold spot only in the
#' last time-step	.
#'
#' @return A shapefile containing the space-time trend ID, the
#' space-time trend label, the Mann-Kendall p-values, z-scores,
#' tau values and slope, the percentage of time-steps on which each
#' cell showed to be hot spot and the percentage of time-steps on
#' which each cell showed to be cold spot.
#'
#' @references Theil, H. (1950). A rank invariant method for linear
#' and polynomial regression analysis. Nederl. Akad. Wetensch. Proc.
#' Ser. A 53:386-392 (Part I), 53:521-525 (Part II), 53:1397-1412
#' (Part III).
#'
#' Sen, P.K. (1968). Estimates of Regression Coefficient Based on
#' Kendall's tau. Journal of the American Statistical Association.
#' 63(324):1379-1389.
#'
#' Kendall, M.G. (1976). Rank Correlation Methods. 4th Ed. Griffin.
#'
#' Benjamini, Y., and Hochberg, Y. (1995). Controlling
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
#'# For a MODIS LST raster stack
#'lst_trends <- space_time_trends(x, time = 2, stat = "mean", dist = 2100, p = 0.05)
#'
#' @export
#' @importFrom usdm lisa
#' @importFrom raster rasterToPolygons merge aggregate stackApply stack setValues getValues nlayers res
#' @importFrom spatialEco raster.kendall
#' @importFrom stats na.omit p.adjust pnorm
#'
#'
space_time_trends <- function(x, space = NULL, time = NULL, stat = "mean", dist, p){
  if (nlayers(x) < 5) {
    stop("Minimum number of time-steps (bands in stack) must be 5")
  }
  if (!is.null(space)){
    x <- aggregate(x, fact = space, fun = stat)
  }
  if (res(x)[1] > dist) {
    stop("dist is less than spatial resolution after spatial aggregation")
  }
  if (!is.null(time)){
    ind <- rep(1:nlayers(x), each = time)
    x <- suppressWarnings(stackApply(x, ind, fun = stat))
  }
  st <- stack()
  for (i in 1:nlayers(x)){
    r <- lisa(x[[i]], d1 = 0, d2 = dist, statistic = "G*")
    r <- round(r, 2)
    st <- stack(st, r)
  }
  fdrs <- stack()
  for (i in 1:nlayers(st)){
    p_ge <- round(2*pnorm(-abs(getValues(st[[i]]))), 7)
    p_ge <- as.data.frame(p_ge)
    p_ge$FDR <- round(p.adjust(p_ge[,1], "fdr"), 7)
    fdr <- setValues(st[[1]], p_ge$FDR)
    fdrs <- stack(fdrs, fdr)
  }
  sp <- stack()
  for (i in 1:nlayers(fdrs)){
    nosig <- fdrs[[i]] < p
    nosig[nosig == 1] <- NA
    hot <- st[[i]] > 0 & fdrs[[i]] < p
    hot[hot == 0] <- NA
    cold <- st[[i]] < 0 & fdrs[[i]] < p
    cold[cold == 0] <- NA
    cold[cold == 1] <- -1
    l <- list(nosig, hot, cold)
    r <- do.call(merge, l)
    sp <- stack(sp, r)
  }

  hs <- sum(sp == 1)
  cs <- sum(sp == -1)

  hs <- (hs * 100 / nlayers(sp))
  cs <- (cs * 100 / nlayers(sp))

  k <- raster.kendall(x, p.value = TRUE, tau = TRUE, z.value = TRUE)
  k <- round(k, 7)

  nosig <- k[[2]] < p
  nosig[nosig == 1] <- NA
  incr <- k[[3]] > 0 & k[[2]] < p
  incr[incr == 0] <- NA
  decr <- k[[3]] < 0 & k[[2]] < p
  decr[decr == 0] <- NA
  decr[decr == 1] <- -1
  l <- list(nosig, incr, decr)
  ks <- do.call(merge, l)

  #1 new hot spot
  emh <- sp[[nlayers(sp)]] == 1
  h <- sum(sp[[1:(nlayers(sp) -1)]]  == 1)
  emh <- emh == 1 & h == 0
  emh[emh == 0] <- NA
  #2 increasing hot spot
  inth <- hs >= 90 & ks == 1
  inth[inth == 0] <- NA
  inth[inth == 1] <- 2
  #3 decreasing hot spot
  dech <- hs >= 90 & ks == -1
  dech[dech == 0] <- NA
  dech[dech == 1] <- 3
    #4 Steady hot spot
  perh <- hs >= 90 & ks == 0
  perh[perh == 0] <- NA
  perh[perh == 1] <- 4
  #-1 new cold spot
  emc <- sp[[nlayers(sp)]] == -1
  h <- sum(sp[[1:(nlayers(sp) -1)]]  == -1)
  emc <- emc == 1 & h == 0
  emc[emc == 0] <- NA
  emc[emc == 1] <- -1
  #-2 increasing cold spot
  intc <- cs >= 90 & ks == -1
  intc[intc == 0] <- NA
  intc[intc == 1] <- -2
  #-3 decreasing cold spot
  decc <- cs >= 90 & ks == 1
  decc[decc == 0] <- NA
  decc[decc == 1] <- -3
  #-4 steady cold spot
  perc <- cs >= 90 & ks == 0
  perc[perc == 0] <- NA
  perc[perc == 1] <- -4
  l <- list(emh, inth, dech, perh, emc, intc, decc, perc)
  r <- do.call(merge, l)
  # 0 no pattern
  r[is.na(r) & !is.na(ks)] <- 0

  shp <- rasterToPolygons(r, na.rm = TRUE)
  names(shp) <- "ID_trend"
  shp$trend <- ifelse(shp$ID_trend == 0, "No pattern",
                      ifelse(shp$ID_trend == 1, "New hot spot",
                             ifelse(shp$ID_trend == 2, "Increasing hot spot",
                                    ifelse(shp$ID_trend == 3, "Decreasing hot spot",
                                           ifelse(shp$ID_trend == 4, "Steady hot spot",
                                                  ifelse(shp$ID_trend == -1, "New cold spot",
                                                         ifelse(shp$ID_trend == -2, "Increasing cold spot",
                                                                ifelse(shp$ID_trend == -3, "Decreasing cold spot",
                                                                       ifelse(shp$ID_trend == -4, "Steady cold spot", "")))))))))

  shp$p_trend <- na.omit(as.vector(k[[2]]))
  shp$z_trend <- na.omit(as.vector(k[[3]]))
  shp$slope <- na.omit(as.vector(k[[1]]))
  shp$Tau <- na.omit(as.vector(k[[4]]))
  shp$perc_hot <- na.omit(as.vector(hs))
  shp$perc_cold <- na.omit(as.vector(cs))
  return(shp)
}
