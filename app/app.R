library(shiny)
library(terra)

source("../R/load_all.R")
source("../R/load_shapefiles.R")
source("../R/load_packages.R")

ui <- fluidPage(
  
  titlePanel("BS_MAXENT App"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("occ", "Upload occurrences CSV", accept = ".csv"),
      
      textInput("env_path", "Env raster folder", value = "../aligned_mean_rasters_2025"),
      
      textInput("shp_path", "Shapefile folder", value = "../data/shapefiles"),
      
      textInput("base_folder", "Base folder", value = "general"),
      
      checkboxGroupInput("partitions", "Partitions",
                         choices = c("block", "checkerboard",
                                     "hierarchical_checkerboard"),
                         selected = c("block", "checkerboard",
                                      "hierarchical_checkerboard")),
      
      numericInput("bandwidth", "Bandwidth", value = 20000),
      
      actionButton("run", "Run pipeline")
      
    ),
    
    mainPanel(
      textOutput("status"),
      imageOutput("prediction"),
      downloadButton("download_results", "Download best results")
    )
  )
)

server <- function(input, output) {
  
  occ_path <- reactiveVal(NULL)
  result_path <- reactiveVal(NULL)
  
  observeEvent(input$occ, {
    occ_path(input$occ$datapath)
  })
  
  observeEvent(input$run, {
    
    req(occ_path(), input$env_path, input$shp_path)
    
    output$status <- renderText("Loading data...")
    
    # load env exactly as your script
    env <- load_env(input$env_path)
    env_polished <- calculate_vif(env)
    env <- env_polished$env
    
    # load shapefiles exactly as your script
    shapefiles <- load_shapefiles(input$shp_path)
    med_poly <- shapefiles$med
    
    output$status <- renderText("Running model...")
    
    config <- list(
      
      # paths
      res.dir = file.path(tempdir(), "results"),
      base_folder = input$base_folder,
      
      # data
      occ_file = occ_path(),
      env = env,
      regions = med_poly,
      
      # model settings
      partitions = c("block", "checkerboard", "checkerboard"),
      partition.folders = c("block", "checkerboard", "hierarchical_checkerboard"),
      ag = list(NULL, 10, c(10,10)),
      
      fc = c('L','Q','P','LQ','H'),
      rm = seq(1,5,0.5),
      
      # metadata
      year.range = "app_run",
      thin = FALSE,
      parallel = FALSE,
      
      # bias
      bandwidth = c(input$bandwidth)
    )
    
    res <- run_pipeline(config)
    # save_path <- save_best_result(res)
    
    result_path(save_path)
    
    output$status <- renderText("Done")
  })
}

shinyApp(ui, server)