library(shiny)
library(raster)
library(rgeos)
source("funcs.R")

# Parameters for the NPP interpolation
nbuf <- 7 # number of cells to buffer on each side
interp_d <- 25000 # max distance to interpolate from, in meters
ninterp_max <- 3 # max number of cells to use in interpolation

# Read in NPP data and mask layers
npp_brick <- brick("data/prod_brick.grd")
depth30 <- raster("data/depth30_final.grd")
interp <- raster("data/interp_final.grd")

# Read in reef and land masks
reefs <- raster("data/reefs_500_dd.tif")
land <- raster("data/GSHHS_rast.tif")

    
shinyServer(function(input, output) {
    
    # NPP
    
    npp_res <- reactiveValues(df = NULL)
    
    observe({
        pts_file <- input$npp_file
        if (is.null(pts_file)) return(NULL)
        npp_res$df <- read.csv(pts_file$datapath)
    })
    
    observeEvent(input$npp_compute, {
        pts <- npp_res$df[, c("long", "lat")]
        pts$long <- pts$long + (pts$long < 0) * 360
        stat <- input$npp_stat
        withProgress(message = "Processing...", value = 0, {
            if (stat == "mean") func <- function(long, lat) {
                incProgress(1/nrow(pts))
                get_prod_mean(long, lat)
            }
            res <- mapply(func, pts$long, pts$lat)
        })
        npp_res$df[[stat]] <- res
    })

    output$npp_out <- renderDataTable(npp_res$df, option = list(pageLength = 10))
    
    output$npp_download <- downloadHandler(
        filename = "npp_out.csv",
        content = function(file) write.csv(npp_res$df, file, row.names = FALSE)
    )
    
    
    # LAND AND REEF AREA
    
    area_res <- reactiveValues(df = NULL)
    
    observe({
        pts_file <- input$area_file
        if (is.null(pts_file)) return(NULL)
        area_res$df <- read.csv(pts_file$datapath)
    })
    
    observeEvent(input$area_compute, {
        pts <- area_res$df[, c("long", "lat")]
        pts$long <- pts$long + (pts$long < 0) * 360
        dist <- input$dist * 1000
        which_area <- input$which_area
        withProgress(message = "Processing...", value = 0, {
            func <- function(...) {
                incProgress(1/nrow(pts))
                get_lr_area(...)
            }
            res <- do.call(rbind, 
                    Map(func, pts$long, pts$lat, dist, which_area))
        })
        area_res$df[colnames(res)] <- res
    })
    
    output$area_out<- renderDataTable(area_res$df, option = list(pageLength = 10))
    
    output$area_download <- downloadHandler(
        filename = "area_out.csv",
        content = function(file) write.csv(area_res$df, file, row.names = FALSE)
    )
})

