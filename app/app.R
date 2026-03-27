library(shiny)
library(bslib)
library(terra)

source("../R/load_packages.R")
load_packages()
source("../R/load_all.R")
# source("../R/load_shapefiles.R")

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",   # clean, neutral
  primary = "#2C3E50"
)

ui <- page_sidebar(
  theme = theme,
  
  title = div(
    class = "d-flex justify-content-between align-items-center",
    span("MaxEnt pipeline"),
    span(class = "badge bg-secondary", textOutput("status"))
  ),
  
  sidebar = sidebar(
    
    card(
      card_header("Inputs"),
      
      fileInput("occ", "Upload occurrences CSV", accept = ".csv"),
      
      textInput("env_path", "Env raster folder", value =
                  "../aligned_mean_rasters_2025"),
    
    # textInput("shp_path", "Shapefile folder", value = "../data/shapefiles"),
    
      textInput("base_folder", "Experiment folder", value = "general"),
    
      checkboxGroupInput(
        "regions_selected",
        "Select regions",
        choices = c("Mediterranean", "West", "Central", "East", "Central_east")
      ),
      
      uiOutput("region_paths"),
    ),
    
    accordion(
      accordion_panel(
        "Model settings",
        
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
        
      )
    ),
    
    div(
      class = "d-grid mt-3",
      actionButton("run", "Run pipeline", class = "btn-primary")
    )
  ),
  
  card(
    class = "mt-3",
    card_header("Predictions"),
    imageOutput("prediction", height = "500px", width = "600px")
  ),
  
  card(
    class = "mt-3",
    card_header("Download"),
    downloadButton("download_results", "Download results")
  )
)
    
   
  
  

server <- function(input, output, session) {
  
  # dynamically create textInputs
  output$region_paths <- renderUI({
    
    req(input$regions_selected)
    
    tagList(
      lapply(input$regions_selected, function(reg) {
        
        card(
          class = "mt-2",
          card_header(reg),
          
          textInput(
            inputId = paste0("path_", reg),
            label = "Shapefile path"
          )
        )
        
      })
    )
  })
  
  # build regions list
  regions_reactive <- reactive({
    
    req(input$regions_selected)
    
    regions <- list()
    
    for (reg in input$regions_selected) {
      
      path <- input[[paste0("path_", reg)]]
      
      if (is.null(path) || path == "") next
      
      shp <- terra::vect(path)
      shp$NAME <- toupper(reg)
      
      regions[[reg]] <- shp
    }
    
    req(length(regions) > 0)
    
    return(regions)
  })
  
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
    
    req(occ_path(), input$env_path, regions_reactive())
    
    withProgress(message = 'Running pipeline', value = 0, {
      
      incProgress(0.1, detail = "Loading occurrences")
      
      # load env as script
      env <- load_env(input$env_path)
      env_polished <- calculate_vif(env)
      env <- env_polished$env
      
      incProgress(0.2, detail = "Loading shapefiles")
      
      incProgress(0.3, detail = "Preparing configuration")
      
      session_dir <- file.path(tempdir(), session$token)
      
      config <- list(
        
        # paths
        res.dir = file.path(session_dir),
        base_folder = input$base_folder,
        
        # data
        occ_file = occ_path(),
        env = env,
        regions = regions_reactive(),
        
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