extract_best_model <- function(res.dir, folder) {

  fp <- file.path(res.dir, folder, "selected_models.csv")
  df <- read.csv(fp, sep = ';', header = TRUE)
  
  best <- df[df$score == min(df$score), ]
  
  # in the unlikely event there are multiple minimum scores
  if (nrow(best) > 1) {
    best <- best[best$auc.val.avg == max(best$auc.val.avg), ]
  }
  
  writeLines("", file.path(res.dir, folder, paste(best$partition, '.txt', sep = '')))
  
  write.table(df, fp, sep = ';', row.names = FALSE)
  
}
