

library(spectrolab)
library(torch)
library(RStoolbox)

setwd("C:/lecospec")
source("C:/lecospec/Functions/lecospectR.R")

#Read in plot image spectra

trees_image_spectra<-
  read.csv("C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_clean_resampled.csv")
trees_image_spectra_df <- speclib_to_df(trees_image_spectra)

trees_image_spectra_VIs <- get_vegetation_indices(trees_image_spectra_df)

#Calculate vegetation indices for the pixels
trees_image_spectra_VIs <- get_vegetation_indices(trees_image_spectra_df, NULL)
beep(5)

tree_image_spectra_VIs <- cbind(as.data.frame(trees_image_spectra)[,2:5],trees_image_spectra_VIs) 
write.csv(tree_image_spectra_VIs_bind,  "C:/Users/PaintRock/Documents/Data processing/Hyperspectral/QGIS_VIs_5nm.csv")
