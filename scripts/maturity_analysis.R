library(here)

source(here("R/load_packages.R"))
source(here("R/load_shapefiles.R"))
source(here("R/load_all.R"))

load_packages()

env_path <- "aligned_mean_rasters_2025"

shapefiles <- load_shapefiles('data/shapefiles')
med_poly <- shapefiles$med
regions <- shapefiles$reg
regions$med <- med_poly

env <- load_env(env_path)
env_polished <- calculate_vif(env)
env <- env_polished$env
env.vif <- env_polished$vif
env.rem <- env_polished$rem

config <- list(
  # paths
  res.dir = "experiments",
  base_folder = "",
  
  # data
  occ_file = "",
  env = env,
  regions = regions,
  
  # model settings
  partitions = c("block", "checkerboard", "checkerboard"),
  partition.folders = c("block", "checkerboard", "hierarchical_checkerboard"),
  ag = list(NULL, 10, c(10,10)),
  
  fc = c('L','Q','P','LQ','H'),
  rm = seq(1,5,0.5),
  
  # metadata
  year.range = "2015_2025",
  thin = FALSE,
  parallel = FALSE,
  
  # bias
  bandwidth = c(20000)
)

name_files <- list('data/maturity_data/mat_a.csv', 'data/maturity_data/mat_y.csv')
folders <- list("maturity/adult", "maturity/juvenile")

for (i in seq_along(name_files)) {
  config$occ_file <- name_files[[i]]
  config$base_folder <- folders[[i]]
  
  run_pipeline(config)
}
