create_bias <- function(occ, env, bandwidth) {
  #' Generate a sampling bias surface and biased background points
  #'
  #' This function creates a spatial bias surface based on occurrence data and
  #' uses it to sample background points that reflect the same sampling bias.
  #' This approach accounts for non-random sampling effort (e.g., opportunistic
  #' data such as fisheries bycatch) and reduces bias in species distribution
  #' models.
  #'
  #' The procedure is as follows:
  #'
  #' 1. Occurrence points are converted to a spatial vector and projected to a
  #'    planar coordinate system (EPSG:3035) to ensure distance-based operations
  #'    are meaningful.
  #'
  #' 2. Occurrences are rasterized onto the environmental grid, creating a binary
  #'    presence raster.
  #'
  #' 3. A Gaussian kernel density surface is computed using a focal operation
  #'    with a specified bandwidth, producing a continuous bias surface
  #'    representing sampling intensity.
  #'
  #' 4. The bias surface is normalized to the range [0, 1].
  #'
  #' 5. Areas with missing environmental data are masked out to ensure background
  #'    points are only sampled where predictors are available.
  #'
  #' 6. Background points are sampled probabilistically from the bias surface,
  #'    such that areas with higher sampling intensity have a higher probability
  #'    of being selected.
  #'
  #' 7. Background points are reprojected back to geographic coordinates
  #'    (EPSG:4326) for compatibility with modelling functions.
  #'
  #' The resulting background dataset reflects the same spatial sampling bias
  #' as the occurrence data, improving model reliability under non-uniform
  #' sampling effort.
  #'
  #' @param occ data.frame. Occurrence coordinates (longitude, latitude).
  #' @param env SpatRaster. Environmental predictor variables.
  #'
  #' @return A list containing:
  #'   bg - data.frame of sampled background points (lon/lat)
  #'   occ_vect - SpatVector of occurrence points
  #'   bg_vect - SpatVector of background points
  
  
  # sample the background with the same bias that occurrences have
  occ_vect <- vect(occ, geom = c("dd.long","dd.lat"), crs = "EPSG:4326")
  
  occ_proj <- project(occ_vect, "EPSG:3035") # Euclidean for Gaussian filter
  env_proj <- project(env, "EPSG:3035")
  
  occ_ras <- rasterize(occ_proj, env_proj, field = 1, background = NA)
  
  bias <- focal(occ_ras,
                w = focalMat(occ_ras, d = bandwidth, type = "Gauss"),
                fun = "sum",
                na.rm = TRUE)
  bias <- bias / global(bias, "max", na.rm = TRUE)[1,1]   # standardize
  
  env_mask <- !is.na(sum(env_proj))   # remove bg where env is na
  bias <- mask(bias, env_mask) 
  
  bg_spat <- terra::spatSample(bias,
                               size = 10000,
                               method = "weights",
                               na.rm = TRUE,
                               xy = TRUE)
  
  bg_vect_1 <- vect(bg_spat, geom=c("x","y"), crs=crs(env_proj))
  bg_vect_1 <- project(bg_vect_1, "EPSG:4326")
  
  bg <- data.frame(terra::crds(bg_vect_1))
  colnames(bg) <- colnames(occ)
  
  # convert to vectors for polygon handling
  bg_vect  <- vect(bg,  geom = c("dd.long","dd.lat"), crs = "EPSG:4326")
  
  return(list(bg, occ_vect, bg_vect))
}
