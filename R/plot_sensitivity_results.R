plot_sensitivity_results <- function(bw_dir) {
  
  bws <- list.dirs(bw_dir, recursive = FALSE)
  first.run <- TRUE
  
  for (bw_name in bws) {
    list_f <- list.files(bw_name, recursive = TRUE)
    df_name <- list_f[[grep("selected_models.csv", list_f)]]
    df <- read.csv(file.path(bw_name, df_name), sep = ';', header = TRUE)
    
    df_block <- df[df$partition == 'block', ]
    df_cb <- df[df$partition == 'checkerboard', ]
    df_hcb <- df[df$partition == 'hierarchical_checkerboard', ] 
    
    bw_split <- strsplit(basename(bw_name), '_')[[1]][2]
    bw_split <- as.integer(bw_split)
    
    df_block$bandwidth <- bw_split
    df_cb$bandwidth <- bw_split
    df_hcb$bandwidth <- bw_split
    
    if (first.run) {
      logfile <- list.files(bw_dir)
      logfile <- logfile[grepl(".csv", logfile)]
      
      first.run <- FALSE
      file.remove(file.path(bw_dir, logfile))
    }
    
    df_block <- df_block[,c('bandwidth','score','partition')]
    write.table(df_block,
                file = file.path(bw_dir, 'block_score.csv'),
                sep = ';',
                append = TRUE,
                row.names = FALSE,
                col.names = !file.exists(file.path(bw_dir, "block_score.csv")))
    
    df_cb <- df_cb[,c('bandwidth','score','partition')]
    write.table(df_cb,
                file = file.path(bw_dir, 'checkerboard_score.csv'),
                sep = ';',
                append = TRUE,
                row.names = FALSE,
                col.names = !file.exists(file.path(bw_dir,
                                                   "checkerboard_score.csv")))
    
    df_hcb <- df_hcb[,c('bandwidth','score','partition')]
    write.table(df_hcb,
                file = file.path(bw_dir, 'hierarchical_cb_score.csv'),
                sep = ';',
                append = TRUE,
                row.names = FALSE,
                col.names = !file.exists(file.path(bw_dir,
                                                   "hierarchical_cb_score.csv")))
    
  }
  
  
  
  block <- read.csv(file.path(bw_dir, "block_score.csv"), sep = ';', header = TRUE)
  
  checkerboard <- read.csv(file.path(bw_dir, "checkerboard_score.csv"), sep = ';',
                           header = TRUE)
  
  hierarchical <- read.csv(file.path(bw_dir, "hierarchical_cb_score.csv"), sep = ';',
                                     header = TRUE)
  
  df <- bind_rows(block, checkerboard, hierarchical)
  
  jpeg(file.path(bw_dir, "bw_analysis.jpg"), 
       res = 300,
       width = 8,
       height = 8,
       units = "in")
  
  p <- ggplot(df, aes(x = bandwidth/1000, y = score)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ partition, nrow = 3, scales = 'free_y') +
    scale_x_continuous(breaks = df$bandwidth / 1000) +
    labs(
      title = "Bandwidth sensitivity analysis",
      x = "Bandwidth [km]",
      y = "Score"
    ) +
    theme_classic()
  
  print(p)
  
  dev.off()
  if (!is.null(dev.list())) graphics.off()
}
