---
title: "LSTtools: An R package to process Land Surface Temperature (LST) data derived from Landsat and MODIS images"
author:
- Richard Lemoine Rodriguez^[Institute of Geography, Ruhr-Universität Bochum]
- Jean François Mas^[Centro de Investigaciones en Geografía Ambiental, Universidad Nacional Autónoma de México]
description: >
  Describe the steps to compute Land Surface Temperature (LST) from a Landsat 8 image and filter the quality of LST and NDVI MODIS pixels.
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{LSTtools: An R package to process Land Surface Temperature (LST) data derived from Landsat and MODIS images}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

## The LSTtools package provides tools for the analysis of Land Surface Temperature (LST) data derived from Landsat and MODIS satellites. It includes a number of pre-processing, processing and post-processing tools to conduct Urban Heat Island (UHI) assessments.

### To install and load the package run
```{r}
install.packages("devtools")
library(devtools)
install_github("RichardLemoine/LSTtools")
library(LSTtools)
```
