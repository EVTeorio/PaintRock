install.packages("plotly")
library(spectrolab)
library(hsdar)
library(plotly)

#Calculate vegetation indices
source("Functions/lecospectR.R")
source("Functions/vegindex.R")
source("Functions/spectral_operations.R")


install.packages("rgdal")
help("install_github")
library(rgdal)

#Read in plot image spectra

trees_image_spectra<-read.csv("output/speclib/spruce_canopy_speclib_5nm.csv")
trees_image_spectra <- data
trees_image_spectra_df<- speclib_to_df(trees_image_spectra)
#Calculate vegetation indices for the pixels
trees_image_spectra_VIs<-get_vegetation_indices(trees_image_spectra_df, NULL) 

tree_image_spectra_VIs<-cbind(as.data.frame(trees_image_spectra)[,2:5],trees_image_spectra_VIs) 
#write.csv(plots_image_spectra_VIs,  "WymansNutrientTrial2023/output/speclib/plot_VIs.csv")
write.csv(tree_image_spectra_VIs,  "output/speclib/spruce_VIs_5nm.csv")
