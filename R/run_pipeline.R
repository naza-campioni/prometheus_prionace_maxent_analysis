#' pipeline function that takes an experiment configuration as input and creates
#' loop to run maxent for different partition methods, mediterrenean regions and
#' possibly different bandwidths
#' 
#' the pipeline is as follows:
#' 
#' prepare data -> create bias -> select data falling into region of interest
#' -> run maxent -> extract best models and save them in the result section 
#' 
#' @param config Configuration of experiment scenario
#' 
#' @return maxent results in experiments folder and only best results in 
#' results folder

run_pipeline <- function(config) {
  # setup config
  res.dir <- config$res.dir
  base_folder <- config$base_folder
  
  occ_file <- config$occ_file
  env <- config$env
  regions <- config$regions
  
  partitions <- config$partitions
  partition.folders <- config$partition.folders
  ag <- config$ag
  
  fc <- config$fc
  rm <- config$rm
  
  year.range <- config$year.range
  thin <- config$thin
  parallel <- config$parallel
  
  bandwidth <- config$bandwidth
  n.bg <- config$n.bg
  
  path <- list()
  
  occ <- prep_occ(occ_file, env)
  
  for (bw in seq_along(bandwidth)) {
    vectors <- create_bias(occ, env, bandwidth[bw])
    bg <- vectors[[1]]
    occ_vect <- vectors[[2]]
    bg_vect <- vectors[[3]]
    
    # ensure occurrences fall on valid predictor cells
    occ_vals <- terra::extract(env, occ, ID=FALSE)
    occ <- occ[complete.cases(occ_vals), ]
    
    # ensure background falls on valid predictor cells
    bg_vals <- terra::extract(env, bg, ID=FALSE)
    bg <- bg[complete.cases(bg_vals), ]
    
    # ensure regions is list so it's iterable
    if (!is.list(regions)) {
      regions <- list(regions)
    }
    
    for (region in regions) {
      first.run <- TRUE
      
      # select data falling into the region of interest
      occ_ <- occ_vect[region, ]
      bg_ <- bg_vect[region, ]
      
      occ_ <- as.data.frame(occ_, geom = 'XY')
      colnames(occ_) <- colnames(occ)
      
      bg_ <- as.data.frame(bg_, geom = 'XY')
      colnames(bg_) <- colnames(bg)
      
      env_ <- mask(crop(env, region), region)
      
      # remove points that fall on NA predictors after masking
      occ_vals <- terra::extract(env_, occ_, ID = FALSE)
      occ_keep <- complete.cases(occ_vals)
      occ_ <- occ_[occ_keep, ]
      
      bg_vals <- terra::extract(env_, bg_, ID = FALSE)
      bg_keep <- complete.cases(bg_vals)
      bg_ <- bg_[bg_keep, ]
      
      # if more than 1 bandwidth then modify folder accordingly
      if (length(bandwidth) > 1) {
        folder <- file.path(base_folder, paste0('bw_', bandwidth[bw]),
                            region$NAME[1])
        } else {
        folder <- file.path(base_folder, region$NAME[1])
      }
      
      # check if csv file already exists before first run to remove it
      logfile <- file.path(res.dir, folder, "selected_models.csv")
      
      if (first.run & file.exists(logfile)) {
        txt <- list.files(file.path(res.dir, folder), pattern = '.txt')
        first.run <- FALSE
        file.remove(logfile)
        
        for (j in seq_along(txt)) {
          file.remove(file.path(res.dir, folder, txt[j]))
        }
      }
      
      for (i in seq_along(partitions)) {
        
        cat("##################\n")
        cat("Analysing:\n")
        cat("Experiment - ", base_folder, "\n")
        cat("Bandwidth - ", bandwidth[bw], "\n")
        cat("Region - ", region$NAME[1], "\n")
        cat("Partition - ", partition.folders[[i]], "\n")
        cat("##################\n")
        
        maxent_calculate(res.dir,
                         year.range = year.range,
                         region,
                         env_,
                         occ_,
                         folder = folder,
                         partition = partitions[[i]],
                         partition.folder = partition.folders[[i]],
                         a.g = ag[[i]],
                         algorithm = 'maxent.jar',
                         bg = bg_,
                         n.bg = n.bg,
                         fc = fc,
                         rm = rm,
                         thin = thin,
                         parallel = parallel,
                         bandwidth = bandwidth[bw])
      }
      extract_best_model(res.dir, folder)
      res_path <- save_best_results(res.dir, folder)
      path[[region$NAME[1]]] <- res_path
    }
    
  }
  return(path)
  }