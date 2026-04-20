#' This function reads raw occurrence data from file and ensures compatibility
#' with environmental predictors
#'
#' The resulting dataset contains unique occurrence points aligned with the
#' environmental raster grid and free of missing predictor values.
#'
#' @param file_name Character. Path to the CSV file containing occurrence data.
#' @param env SpatRaster. Stack of environmental predictor variables.
#'
#' @return A data.frame of cleaned occurrence coordinates (longitude, latitude).

prep_occ <- function(file_name, env) {
  # observations
  occ <- read.table(file_name, sep=",", header=TRUE)[,c(2,3)]
  occ <- occ[!duplicated(occ), ]
  occ <- occ[complete.cases(occ), ]
  
  # remove occurrences that are cell duplicates
  occs.cells <- terra::extract(env[[1]], occ, cellnumbers = TRUE, ID = FALSE)
  occ <- occ[!duplicated(occs.cells[,1]), ]
  
  # remove NA values from our environmental variable rasters
  occs.z <- terra::extract(env, occ, ID = FALSE)
  occ <- occ[complete.cases(occs.z), ]
  
  return(occ)
  
}
