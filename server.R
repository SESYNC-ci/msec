source("funcs.R")
source("load_data.R")


function(input, output, session) {
    
    # Common container for input points and results
    pts <- reactiveValues(df = data.frame(long = numeric(0), lat = numeric(0)))

    # Maximum number of points (general, and reduced ones for radius-based variables)  
    max_pts_default <- 10000
    maxpts_radius <- function(dist) {
        if (dist > 100) { 
            return(25) 
        } else if (dist > 50) { 
            return(50)
        } else if (dist > 25) { 
            return(100) 
        } else { 
            return(200) 
        }
    }
    
    #### Net primary productivity ####
    
    callModule(addPoints, "npp_in", pts)
    callModule(outputPoints, "npp_out", pts)
    
    observeEvent(input$npp_compute, {
        if(length(input$npp_stat) == 0) {
            showNotification("Please choose one or more statistics.", type = "error")
        } else if (nrow(pts$df) > max_pts_default) {
            showNotification("Number of points exceeds limit.", type = "error")
        } else {
            coords <- pts$df[, c("long", "lat")]
            coords$long <- coords$long + (coords$long < 0) * 360
            stat <- c(input$npp_stat, "flag")
            res <- extract(subset(npp_stats, stat), coords, method = "simple")
            colnames(res) <- paste0("npp_", colnames(res))
            pts$df[, colnames(res)] <- res
        }
    })
    
    #### Land and reef area ####
    
    callModule(addPoints, "area_in", pts)
    callModule(outputPoints, "area_out", pts)
    
    observeEvent(input$area_compute, {
        max_pts <- maxpts_radius(input$area_dist)
        if (nrow(pts$df) > max_pts) {
            showNotification("Number of points exceeds limit.", type = "error")
            return(NULL)
        }
        dist <- input$area_dist * 1000
        withProgress(message = "Processing...", value = 0, {
            res <- lapply(1:nrow(pts$df), function(i) {
                incProgress(1/nrow(pts$df), detail = paste0(i, "/", nrow(pts$df)))
                long = pts$df$long[i]
                lat = pts$df$lat[i]
                switch(input$which_area,
                       both = c(land_area = land_area(long, lat, dist), 
                                reef_area = reef_area(long, lat, dist)),
                       land = c(land_area = land_area(long, lat, dist)),
                       reef = c(reef_area = reef_area(long, lat, dist))
                )
            })
        })
        res <- do.call(rbind, res)
        colnames(res) <- paste0(colnames(res), "_", input$area_dist, "km")
        pts$df[, colnames(res)] <- res
    })
    
    #### Wave energy ####

    callModule(addPoints, "wave_in", pts)
    callModule(outputPoints, "wave_out", pts)
    
    observeEvent(input$wave_compute, {
        if(length(input$wave_stat) == 0) {
            showNotification("Please choose one or more statistics.", type = "error")
        } else if (nrow(pts$df) > max_pts_default) {
            showNotification("Number of points exceeds limit.", type = "error")
        } else {
            coords <- pts$df[, c("long", "lat")]
            coords$long <- coords$long + (coords$long < 0) * 360
            stat <- c(input$wave_stat, "wind_fetch", "ww3_res")
            res <- extract(subset(wave_stats, stat), coords, method = "simple")
            colnames(res) <- paste0("wave_", colnames(res))
            pts$df[, colnames(res)] <- res
        }
    })

    #### Human population ####
    
    callModule(addPoints, "pop_in", pts)
    callModule(outputPoints, "pop_out", pts)
    
    observeEvent(input$pop_compute, {
        max_pts <- maxpts_radius(input$pop_dist)
        if (nrow(pts$df) > max_pts) {
            showNotification("Number of points exceeds limit.", type = "error")
            return(NULL)
        }
        dist <- input$pop_dist * 1000
        years <- input$pop_year
        withProgress(message = "Processing...", value = 0, {
            res <- lapply(1:nrow(pts$df), function(i) {
                incProgress(1/nrow(pts$df), detail = paste0(i, "/", nrow(pts$df)))
                human_pop(pts$df$long[i], pts$df$lat[i], dist, years)
            })
        })
        res <- do.call(rbind, res)
        colnames(res) <- paste0(colnames(res), "_", input$pop_dist, "km")
        pts$df[, colnames(res)] <- res
    })
    
    #### Distance to market ####
    
    callModule(addPoints, "mark_in", pts)
    callModule(outputPoints, "mark_out", pts)
    
    observeEvent(input$mark_compute, {
        if (nrow(pts$df) > max_pts_default) {
            showNotification("Number of points exceeds limit.", type = "error")
        } else {
            coords <- pts$df[, c("long", "lat")]
            coords$long <- coords$long - (coords$long > 180) * 360
            dist_market <- apply(coords, 1, 
                                 function(x) min(distGeo(x, capitals))/1000)
            pts$df$dist_market <- dist_market
        }
    })
    
}

