copy_best <- function(folder, subfolder, region, path) {
  dir_to_create <- file.path(folder, subfolder, region)
  
  if (!dir.exists(dir_to_create)) {
    dir.create(dir_to_create, recursive = TRUE)
  }
  
  best_files <- list.files(path, full.names = TRUE)
  
  file.copy(best_files,
            dir_to_create,
            overwrite = TRUE)
}