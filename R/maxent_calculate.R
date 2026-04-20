maxent_calculate <- function(res.dir, year.range, med_poly, env, occ, folder,
                             partition.folder, partition, a.g = NULL, algorithm,
                             bg, n.bg, fc, rm, parallel = FALSE, thin = FALSE,
                             numCores = 6, bandwidth, pred.type) {
  
  # environmental thinning (not used after correct bias creation)
  if (isTRUE(thin) & (nrow(occ) >= 100)) {
    env_vals <- terra::extract(env, occ, ID = FALSE)
    clusters <- kmeans(scale(env_vals), centers = (nrow(env_vals)-1))
    occ <- occ[!duplicated(clusters$cluster), ]
  }
  
  save_folder <- file.path(res.dir, folder, partition.folder)
  
  if (!dir.exists(save_folder)) {
    dir.create(save_folder, recursive = TRUE, showWarnings = FALSE)
  }
  
  
  if (!is.null(bg)) {
  # plot partitioned background and inter-partition env similarity
  if (partition == 'block') {
    block <- get.block(occ, bg, orientation = "lat_lon")
    
    # background partitions
    jpeg(file.path(save_folder,"occ_background.jpg"), res = 300, width = 16,
         height = 16, units = "cm")
    
    p <-evalplot.grps(pts = bg, pts.grp = block$bg.grp, envs = env) +
      ggplot2::ggtitle(paste(partition ,": background", sep = ''))
    print(p)
    
    dev.off()
    if (!is.null(dev.list())) graphics.off()
    
    # partition-wise environmental differences with remaining partitions
    occs.z <- cbind(occ, terra::extract(env, occ, ID = FALSE))
    bg.z <- cbind(bg, terra::extract(env, bg, ID = FALSE))
    
    jpeg(file.path(save_folder,"partition_env_similarity.jpg"),res = 300,
         width = 16, height = 16, units = "cm")
    
    p <- evalplot.envSim.hist(occs.z = occs.z, bg.z = bg.z,
                              occs.grp = block$occs.grp, bg.grp = block$bg.grp,
                              ref.data = "occ")
    print(p)
    
    dev.off()
    if (!is.null(dev.list())) graphics.off()
    
  } else if (partition == 'checkerboard') {
    cb1 <- get.checkerboard(occ, env, bg, aggregation.factor = a.g)
    
    # background partitions
    jpeg(file.path(save_folder,"occ_background.jpg"), res = 300, width = 16,
         height = 16, units = "cm")
    
    p <- evalplot.grps(pts = bg, pts.grp = cb1$bg.grp, envs = env) +
      ggplot2::ggtitle(paste(partition ,": background", sep = ''))
    print(p)
    
    dev.off()
    if (!is.null(dev.list())) graphics.off()
    
    # partition-wise environmental differences with remaining partitions
    occs.z <- cbind(occ, terra::extract(env, occ, ID = FALSE))
    bg.z <- cbind(bg, terra::extract(env, bg, ID = FALSE))
    
    
    jpeg(file.path(save_folder,"partition_env_similarity.jpg"),
         res = 300, width = 16, height = 16, units = "cm")
    
    p <- evalplot.envSim.hist(occs.z = occs.z, bg.z = bg.z, occs.grp = cb1$occs.grp, 
                              bg.grp = cb1$bg.grp, ref.data = "occ")
    print(p)
    
    dev.off()
    if (!is.null(dev.list())) graphics.off()
  } else {
    # jackknife
  }
  }
  # check maxent_outputs folder
  outdir <- file.path(save_folder, "maxent_outputs")
  
  if (dir.exists(outdir)) {
    unlink(outdir, recursive = TRUE, force = TRUE)
  }
  
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  
  
  # run ENMevaluate
  if (partition == "checkerboard") {
    
    e.mx <- ENMevaluate(occs = occ,
                        envs = env,
                        bg = bg, 
                        n.bg = n.bg,
                        algorithm = algorithm,
                        partitions = partition, 
                        partition.settings = list(aggregation.factor = a.g),
                        tune.args = list(
                          fc = fc,
                          rm = rm),
                        other.settings = list(
                          path = file.path(save_folder,"maxent_outputs"),
                          pred.type = pred.type),
                        parallel = parallel,
                        numCores = numCores)
    
  } else {
    
    e.mx <- ENMevaluate(occs = occ,
                        envs = env,
                        bg = bg, 
                        algorithm = algorithm,
                        partitions = partition, 
                        tune.args = list(
                          fc = fc,
                          rm = rm),
                        other.settings = list(
                          path = file.path(save_folder,"maxent_outputs"),
                          pred.type = pred.type),
                        parallel = parallel,
                        numCores = numCores)
    
  }
  # sometimes the maxent_outputs folder created problems so we close connections
  closeAllConnections()
  gc()
  Sys.sleep(0.5)
  
  if(length(e.mx@models) == 0){
    stop("No valid models returned by ENMevaluate")
  }
  
  # save results
  save(e.mx, file = file.path(save_folder, "res.rda"))
  
  write.table(e.mx@results,
              file.path(save_folder, "res.csv"), sep = ";",
              row.names = FALSE)
  
  # filter results and write them
  filtered_results <- filter_results(e.mx, save_folder, res.dir, folder,
                                     partition.folder)
  best_model <- filtered_results[[1]]
  details <- filtered_results[[2]]
  
  if (is.null(filtered_results)) {return(NULL)}
  
  write_selected_models(details, res.dir, folder)
  
  # plot response curves
  nvars <- length(names(env))
  if (is.integer(sqrt(nvars))) {
    len = sqrt(nvars)
    ps <- TRUE
  } else {
    len = ceiling(nvars/2)
    ps <- FALSE
  }
  
  jpeg(file.path(save_folder, "response.jpg"), res = 300, width = 10, height = 8,
       units = "in")
  
  # for a nice panel
  if (ps) {
    par(mfrow = c(len, len))
  } else {
    par(mfrow = c(2, len))
  }
  
  
  for (name in names(env)) {
    
    pr <- predicts::partialResponse(best_model, var = name, plot = FALSE)
    
    x <- pr[[1]][1][[1]]
    y <- pr[[1]][2][[1]]
    
    plot(x, y, type = 'l', lwd = 2, xlab = name, ylab = 'p')
    
  }
  
  dev.off()
  
  if (!is.null(dev.list())) graphics.off()
  
  # predictions
  pred <- predict(best_model, env, args = paste("outputformat=", pred.type, sep=''))
  
  terra::writeRaster(
    pred,
    filename = file.path(save_folder, "predictions.tif"),
    overwrite = TRUE
  )
  
  
  jpeg(file.path(save_folder, "predictions.jpg"), res = 300, width = 10,
       height = 8, units = "in")
  
  plot(pred, main = paste('Partition: ', partition, ', Details: ', details$tune.args,
                          ", AICc =", round(details$AICc, digits = 0), ", years =",
                          year.range, "pred type: ", pred.type, sep=" "))
  
  dev.off()
  
  if (!is.null(dev.list())) graphics.off()
  
  # contribution
  jpeg(file.path(save_folder, "contributions.jpg"), res = 300, width = 16,
       height = 16, units = "cm")
  
  plot(best_model)
  
  dev.off()
  
  if (!is.null(dev.list())) graphics.off()
  
  # partitions
  observations <- as.data.frame(e.mx@occs)
  observations$parts <- e.mx@occs.grp
  
  jpeg(file.path(save_folder, "occ_partitions.jpg"), res = 300, width = 16,
       height = 16, units = "cm")
  
  plot(med_poly, main = paste(partition, ': occurrences', sep = ''))
  points(observations$dd.long, observations$dd.lat,
         col = observations$parts,
         pch = 19)
  
  dev.off()
  
  if (!is.null(dev.list())) graphics.off()
  
  # null model
  mod.null <- ENMnulls(e.mx, mod.settings = list(fc = details$fc, rm = details$rm),
                       no.iter = 100)
  
  jpeg(file.path(save_folder,"null_test.jpg"), res = 300, width = 16,
       height = 16, units = "cm")
  
  p <- evalplot.nulls(mod.null, stats = c("auc.train","auc.val", "auc.diff",
                                          "or.10p"),
                      plot.type = "histogram") +
    ggplot2::ggtitle("Null model test")
  
  print(p)
  
  dev.off()
  if (!is.null(dev.list())) graphics.off()

  }
