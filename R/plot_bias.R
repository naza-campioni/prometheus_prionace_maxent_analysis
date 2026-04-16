for (bw in seq(10000,100000,10000)) {
  vectors <- create_bias(occ, env, bw)
  bg <- vectors[[1]]
  jpeg(file.path("experiments/bandwidth_sensitivity/bw_plots",
                 paste(bw,'.jpg',sep='')), res = 300, 
       width = 10, height = 8, units = "in")
  plot(bg)
  dev.off()
}