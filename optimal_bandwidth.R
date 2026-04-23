coords_occ <- crds(occ_proj)

# or use 2D plug-in estimator
library(ks)

H <- Hpi(coords_occ)  # plug-in bandwidth matrix
bw_m <- mean(sqrt(diag(H)))  # isotropic summary in meters
bw_km <- bw_m / 1000
