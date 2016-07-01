library(shiny)
library(raster)
source("funcs.R")

# Parameters for the NPP interpolation
nbuf <- 7 # number of cells to buffer on each side
interp_d <- 25000 # max distance to interpolate from, in meters
ninterp_max <- 3 # max number of cells to use in interpolation

# Read in NPP data and mask layers
npp_brick <- brick("data/prod_brick.grd")
depth30 <- raster("data/depth30_final.grd")
interp <- raster("data/interp_final.grd")


shinyServer(function(input, output) {
    
    npp_res <- reactiveValues(df = NULL)
    
    observe({
        pts_file <- input$npp_file
        if (is.null(pts_file)) return(NULL)
        npp_res$df <- read.csv(pts_file$datapath)
    })
    
    observeEvent(input$npp_compute, {
        pts <- npp_res$df[, c("lat", "long")]
        pts$long <- pts$long + (pts$long < 0) * 360
        stat <- input$npp_stat
        withProgress(message = "Processing...", value = 0, {
            if (stat == "mean") func <- function(lat, long) {
                incProgress(1/nrow(pts))
                get_prod_mean(lat, long)
            }
            res <- mapply(func, pts$lat, pts$long)
        })
        
        # res <- mapply(func, pts$lat, pts$long)
        npp_res$df[[stat]] <- res
    })

    output$npp_out<- renderDataTable(npp_res$df, option = list(pageLength = 10))
    
    output$npp_download <- downloadHandler(
        filename = "npp_out.csv",
        content = function(file) write.csv(npp_res$df, file, row.names = FALSE)
    )
})

