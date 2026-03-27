library(shiny)
library(terra)

source("../R/load_packages.R")
load_packages()
source("../R/load_all.R")
source("../R/load_shapefiles.R")

ui <- fluidPage(
  
  titlePanel("BS_MAXENT App"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("occ", "Upload occurrences CSV", accept = ".csv"),
      
      textInput("env_path", "Env raster folder", value =
                  "../aligned_mean_rasters_2025"),
      
      textInput("shp_path", "Shapefile folder", value = "../data/shapefiles"),
      
      textInput("base_folder", "Base folder", value = "../general"),
      
      checkboxGroupInput("partitions", "Partitions",
                         choices = c("block", "checkerboard",
                                     "hierarchical_checkerboard"),
                         selected = c("block", "checkerboard",
                                      "hierarchical_checkerboard")),
      
      checkboxGroupInput("fc", "Feature classes",
                         choices = c('L','Q','P','LQ','H'),
                         selected = c('L','Q','P','LQ','H')),
      
      sliderInput("rm", "Regularization multiplier",
                  min = 0.5, max = 5, value = c(1,5), step = 0.5),
      
      textInput("year.range", "Year range", value = '2015-2025'),
      
      numericInput("bandwidth", "Bandwidth", value = 20000),
      
      actionButton("run", "Run pipeline")
      
    ),
    
    mainPanel(
      textOutput("status"),
      imageOutput("prediction"),
      downloadButton("download_results", "Download results")
    )
  )
)

server <- function(input, output, session) {
  
  occ_path <- reactiveVal(NULL)
  result_path <- reactiveVal(NULL)
  status <- reactiveVal(NULL)
  
  output$status <- renderText({
    status()
  })
  
  observeEvent(input$occ, {
    occ_path(input$occ$datapath)
  })
  
  observeEvent(input$run, {
    
    req(occ_path(), input$env_path, input$shp_path)
    
    withProgress(message = 'Running pipeline', value = 0, {
      
      incProgress(0.1, detail = "Loading occurrences")
      
      # load env exactly as script
      env <- load_env(input$env_path)
      env_polished <- calculate_vif(env)
      env <- env_polished$env
      
      incProgress(0.2, detail = "Loading shapefiles")
      
      # load shapefiles exactly as script
      shapefiles <- load_shapefiles(input$shp_path)
      med_poly <- shapefiles$med
      
      incProgress(0.3, detail = "Preparing configuration")
      
      session_dir <- file.path(tempdir(), session$token)
      
      config <- list(
        
        # paths
        res.dir = file.path(session_dir),
        base_folder = input$base_folder,
        
        # data
        occ_file = occ_path(),
        env = env,
        regions = med_poly,
        
        # model settings
        partitions = c(unlist(lapply(input$partitions, function(p) {
          if (p == 'block') return('block')
          if (p == 'checkerboard') return('checkerboard')
          if (p == 'hierarchical_checkerboard') return('checkerboard')
        }))),
        partition.folders = input$partitions,
        ag = lapply(input$partitions, function(p) {
          if (p == 'block') return(NULL)
          if (p == 'checkerboard') return(10)
          if (p == 'hierarchical_checkerboard') return(c(10,10))
        }),
        
        fc = input$fc,
        rm = input$rm,
        
        # metadata
        year.range = input$year.range,
        thin = FALSE,
        parallel = FALSE,
        
        # bias
        bandwidth = c(input$bandwidth)
      )
      
      incProgress(0.4, detail = "Running MaxEnt models")
      
      best_path <- run_pipeline(config)
      
      incProgress(0.6, detail = "Collecting outputs")
      
      result_path(best_path[[1]])
      
      incProgress(1, detail = "Done!")
      
    })
    status("Done!")
  })
  
  output$prediction <- renderImage({
    path <- result_path()
    req(!is.null(path), dir.exists(path))
    # print(path)
    
    img <- list.files(
      result_path(),
      pattern = 'predictions.jpg',
      recursive = TRUE,
      full.names = TRUE)[1]
    
    list(src = img,
         contentType = 'image/jpg',
         width = "100%",
         height = "500px")
  }, deleteFile = FALSE)
  
  output$download_results <- downloadHandler(
    
    filename = function() {
      paste(input$base_folder, ".zip", sep = '')
    },
    
    content = function(file) {
      
      req(result_path())
      
      files <- list.files(result_path(), full.names = TRUE)
      
      oldwd <- getwd()
      setwd(result_path())
      zip(file, files = basename(files))
      setwd(oldwd)
    }
  )
  
}

shinyApp(ui, server)