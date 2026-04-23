library(here)

source(here("R/load_packages.R"))
source(here("R/load_shapefiles.R"))
source(here("R/load_all.R"))

load_packages()

env_path <- env_path <- list("aligned_mean_season_rasters_2025/WINSPR",
                             "aligned_mean_season_rasters_2025/SUMAUT",
                             "aligned_mean_season_rasters_2025/WINSPR",
                             "aligned_mean_season_rasters_2025/SUMAUT")

shapefiles <- load_shapefiles('data/shapefiles')
med_poly <- shapefiles$med
regions <- shapefiles$reg
regions$med <- med_poly

name_files <- list('data/ontogenetic_data/aw.csv',
                   'data/ontogenetic_data/as.csv',
                   'data/ontogenetic_data/jw.csv',
                   'data/ontogenetic_data/js.csv')
folders <- list("season_ontogeny/adult_winspr",
                "season_ontogeny/adult_sumaut",
                "season_ontogeny/juvenile_winspr",
                "season_ontogeny/juvenile_sumaut")

config <- list(
  # paths
  res.dir = "experiments",
  base_folder = "",
  
  # data
  occ_file = "",
  # env = env,
  regions = med_poly,
  
  # model settings
  partitions = c("block", "checkerboard", "checkerboard"),
  partition.folders = c("block", "checkerboard", "hierarchical_checkerboard"),
  ag = list(NULL, 10, c(10,10)),
  
  fc = c('L','Q','H','P','LQ','LH','LP','QH','QP','HP','LQH','LQP','QHP','LQHP'),
  rm = seq(1,5,0.5),
  
  # metadata
  year.range = "2015_2025",
  thin = FALSE,
  parallel = FALSE,
  
  # bias
  bandwidth = c(45000),
  n.bg = NULL,
  
  # prediction type
  pred.type = 'cloglog'
)

for (i in seq_along(name_files)) {
  env_full <- load_env(env_path[[i]])
  env_polished <- calculate_vif(env_full)
  env <- env_polished$env
  env.vif <- env_polished$vif
  env.rem <- env_polished$rem
  
  if (!("chl" %in% env.rem)) {
    env <- env[[names(env) != "chl"]]
  }
  
  if ('nppv' %in% env.rem) {
    env <- c(env, env_full[['nppv']])
  }
  
  cat("file:", name_files[[i]], 'env: ', names(env))
  
  config$env <- env
  config$occ_file <- name_files[[i]]
  config$base_folder <- folders[[i]]
  
  best_path <- run_pipeline(config)
  copy_best("results", config$base_folder, config$regions$NAME, best_path[[1]])
}
