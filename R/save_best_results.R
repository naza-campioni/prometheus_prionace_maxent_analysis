save_best_results <- function(res.dir, folder) {
  # extract files from best folder
  filepath <- file.path(res.dir,folder)
  lf <- list.files(filepath)
  lfolds <- list.dirs(filepath, recursive = FALSE)
  
  best_model <- lf[grepl('.txt', lf)]
  best_model <- strsplit(best_model, '.txt')[[1]][1]
  
  best_folder <- lfolds[grepl(best_model, lfolds, fixed = TRUE)][1]
  
  best_files <- list.files(best_folder, full.names = TRUE)
  best_files <- best_files[endsWith(best_files, '.jpg') |
                             endsWith(best_files, '.csv') |
                             endsWith(best_files, '.rda') |
                             endsWith(best_files, '.tif')]
  
  dir_to_create <- file.path(res.dir, folder, "best")
  if (!dir.exists(dir_to_create)) {
    dir.create(dir_to_create, recursive = TRUE)
  }
  
  file.copy(best_files,
            file.path(dir_to_create, basename(best_files)),
            overwrite = TRUE)
  
  return(dir_to_create)
}