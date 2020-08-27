# LSTtools: An R package to process Land Surface Temperature (LST) data derived from Landsat and MODIS images
## Authors:
- Richard Lemoine Rodriguez, Institute of Geography, Ruhr-Universität Bochum.
- Jean François Mas, Centro de Investigaciones en Geografía Ambiental, Universidad Nacional Autónoma de México.

### The LSTtools package provides tools for the analysis of Land Surface Temperature (LST) data derived from Landsat and MODIS satellites. It includes a number of pre-processing, processing and post-processing tools to conduct Urban Heat Island (UHI) assessments.

### To install and load the package run
```{r}
install.packages("devtools")
library(devtools)
install_github("RichardLemoine/LSTtools")
library(LSTtools)
library(raster)
library(RColorBrewer)
```
## Load and define Landsat 8 bands
```{r warning=FALSE}
# Load the data
data(land8)
# Define the Landsat 8 bands that will be employed
red <- land8[[3]]
nir <- land8[[4]]
tir <- land8[[7]]
```

## Compute NDVI based on the NIR and Red bands (Rouse et al., 1974)
```{r, echo=FALSE}
veg <- ndvi(nir, red)
plot(veg, col=rev(colorRampPalette(c("green4", "yellow", "firebrick"))(255)), main = "NDVI")
```
