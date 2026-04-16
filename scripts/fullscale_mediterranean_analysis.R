library(here)

source(here("R/load_packages.R"))
source(here("R/load_shapefiles.R"))
source(here("R/load_all.R"))

load_packages()

env_path <- "aligned_mean_rasters_2025"

shapefiles <- load_shapefiles('data/shapefiles')
med_poly <- shapefiles$med
regions <- shapefiles$reg

env <- load_env(env_path)
env_polished <- calculate_vif(env)
env <- env_polished$env
env.vif <- env_polished$vif
env.rem <- env_polished$rem

config <- list(
  # paths
  res.dir = "experiments",
  base_folder = "full_scale_mediterranean",
  
  # data
  occ_file = "data/occurrences_2025.csv",
  env = env,
  regions = med_poly,
  
  # model settings
  partitions = c("block", "checkerboard", "checkerboard"),
  partition.folders = c("block", "checkerboard", "hierarchical_checkerboard"),
  ag = list(NULL, 10, c(10,10)),
  
  fc = c('L','Q','P','H','LQ','LP','QP','QH','LQP','LQH'),
  rm = seq(1,5,0.5),
  
  # metadata
  year.range = "2015_2025",
  thin = FALSE,
  parallel = FALSE,
  
  # bias
  bandwidth = c(20000),
  n.bg = NULL
)

best_path <- run_pipeline(config)
copy_best("results", config$base_folder, config$regions$NAME, best_path[[1]])

