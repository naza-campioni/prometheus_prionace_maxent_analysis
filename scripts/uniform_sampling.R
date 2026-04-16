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

region <- med_poly

res.dir = "experiments"
base_folder <- "uniform_sampling"
folder <- file.path(base_folder, region$NAME[1])

occ_file = "data/occurrences_2025.csv"
occ <- prep_occ(occ_file, env)

partitions = 'checkerboard'
partition.folders = 'checkerboard'

ag = 10
fc = c('L','Q','P','H','LQ','LP','QP','QH','LQP','LQH')
rm = seq(1,5,0.5)

year.range = "2015_2025"

thin = FALSE
parallel = FALSE
bandwidth = 'uniform_sampling'


maxent_calculate(res.dir = res.dir, year.range, med_poly, env, occ, folder,
                 partition.folders, partitions, a.g = ag, algorithm = 'maxent.jar',
                 bg = NULL, n.bg = 10000, fc = fc, rm = rm, bandwidth = bandwidth)