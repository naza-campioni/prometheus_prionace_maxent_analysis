filter_results <- function(e.mx, save_folder, res.dir, folder, partition.folder) {
  # filter results based on:
  # 1) AUC diff 2) score on or.10p.avg and or.10p.sd 3) AICc as tie-breaker
  res <- e.mx@results
  
  # 1) AUC diff
  keep1 <- res$auc.diff.avg <= 0.1
  idx <- which(keep1)
  res <- res[idx, ]
  
  if (length(idx) == 0) {
    writeLines(
      "No model passed AUC difference threshold",
      file.path(save_folder, "no_model_selected.txt")
    )
    return(NULL)
  }
  
  # 2) score
  # calculate score to minimize distance from target 0.10 OR
  res$score <- abs(res$or.10p.avg - 0.1) + res$or.10p.sd # avg distance + stability
  
  # keep models within 10% of lowest score
  keep2 <- res$score <= min(res$score) * 1.1
  
  idx <- which(keep2)
  res <- res[keep2, ]
  
  # 3) select filtered results based on lowest AICc
  keep3 <- which.min(res$AICc)
  best <- res[keep3, ]
  # in the unlikely event AICc is equal across models
  if (nrow(best) > 1) {
    best <- best[best$auc.val.avg == max(best$auc.val.avg), ]
  }
  idx <- idx[keep3]
  
  best_model <- e.mx@models[[idx]]
  best$partition <- partition.folder
  
  return(list(best_model, best))

}