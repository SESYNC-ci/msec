source("funcs.R")
source("load_data.R")


#### App modules ####

addPointUI <- function(id) {
    ns <- NS(id)
    tagList(
        p(tags$b("Location data")),
        fluidRow(column(6,
            numericInput(ns("long"), "long", value = 0, min = -180, max = 180)
        ), column(6, 
            numericInput(ns("lat"), "lat", value = 0, min = -90, max = 90)          
        )),
        actionButton(ns("add_pt"), "Add point"),
        br(), br(),
        fileInput(ns("file"), "Upload file (.csv)", 
                  accept = c("text/csv", "text/comma-separated-values")),
        actionButton(ns("add_pt_file"), "Add points from file"),
        helpText("File should include 'long' and 'lat' columns in decimal degrees.")
    )
}

addPoints <- function(input, output, session, pts) {
    observeEvent(input$add_pt, {
        if (is.numeric(input$long) && is.numeric(input$lat) &&
                input$long >= -180 && input$long <= 360 &&
                 input$lat >= -90  &&  input$lat <= 90) {
            new_pt <- data.frame(long = input$long, lat = input$lat)
            other_cols = setdiff(colnames(pts$df), c("long", "lat"))
            new_pt[, other_cols] <- NA
            pts$df <- rbind(pts$df, new_pt)
        } else {
            showNotification("Input error: invalid long/lat values.",
                             type = "error")
        }
    })
    
    observeEvent(input$add_pt_file, {
        req(input$file)
        tryCatch({
            csv_data = read.csv(input$file$datapath)
            if (all(c("long", "lat") %in% colnames(csv_data))) {
                new_pts <- csv_data[, c("long", "lat")]
                if (is.numeric(new_pts$long) && is.numeric(new_pts$lat) &&
                    all(new_pts$long >= -180) && all(new_pts$long <= 360) &&
                    all(new_pts$lat >= -90)  && all(new_pts$lat <= 90)) {
                    other_cols <- setdiff(colnames(pts$df), c("long", "lat"))
                    new_pts[, other_cols] <- NA
                    pts$df <- rbind(pts$df, new_pts)
                } else {
                    showNotification("Input error: invalid long/lat values.",
                                     type = "error")
                }
            } else {
                showNotification("Input file missing long, lat columns.",
                                 type = "error")
            }
        }, error = function(e) {
            showNotification("Error reading input file.", type = "error")
        })
    })
}

pointOutputUI <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(10, align = "right",
                downloadButton(ns("download"), "Download Output", class = "btn-primary")
            ),
            column(2, align = "right",
                actionButton(ns("clear"), "Clear Data", class = "btn-danger")
            )
        ),
        tags$br(),
        dataTableOutput(ns("dtab"))
    )
}

outputPoints <- function(input, output, session, pts) {
    output$dtab <- renderDataTable(signif(pts$df, digits = 6), 
                                   option = list(pageLength = 20, searching = FALSE))
    
    output$download <- downloadHandler(
        filename = "msec_out.csv",
        content = function(file) write.csv(pts$df, file, row.names = FALSE)
    )
    
    observeEvent(input$clear, {
        pts$df = data.frame(long = numeric(0), lat = numeric(0))       
    })
}


#### UI specification ####

ui <- htmlTemplate("template.html", 
    content = navbarPage("Marine Socio-Environmental Covariates", 
    
    tabPanel("About",
        p("The Marine Socio-Environmental Covariates (MSEC) data set",
          "is a set of high-resolution, global data layers of environmental", 
          "and anthropogenic impact variables. The variables were calculated",
          "to represent ecologically-relevant measurements of biophysical",
          "conditions, landscape context and human impacts for marine", 
          "ecosystems and were selected to complement existing data",
          "products (e.g.,", 
          a("MARSPEC", href = "http://www.marspec.org", target = "_blank"), "or", 
          a("Bio-ORACLE", href = "http://www.oracle.ugent.be/", target = "_blank"),
          "). Variables were derived based on remote-sensing products",
          "and open-access global data products. Full documentation", 
          "and metadata is included the article listed below."),
        h2("How to use this site"),
        p("The MSEC website is intended to increase accessibility of MSEC",
          "data to potential end-users by offering an platform for querying",
          "data layers based on point locations. Data may be extracted for",
          "individual study sites based on geographic coordinates.",  
          "Geographic coordinates (latitude / longitude) can be entered",
          "manually in the website or uploaded in the form of a .csv file.",
          "Users then select the desired variable to be queried on the", 
          "corresponding data tab and extracted data can then be",
          "downloaded into a .csv file. Note that each variable has a", 
          "limitation to the number of points that can be extracted at a",
          "time as well as the maximum search radius (when applicable)."),
        h2("Citation"),
        p("If you use these data products, please cite the following article:"),
        p("Yeager, L.A., Marchand, P., Gill, D.A., Baum, J.K., and",
          "McPherson, J.M. (2017) Queryable global layers of environmental and",
          "anthropogenic variables for marine ecosystem studies.", 
          tags$i("Ecology."), "In Press. doi: 10.1002/ecy.1884."),
        h2("Terms of use"),
        p("MSEC is released is an open-access data product distributed",
          "freely. We have made every effort to ensure the accuracy of the", 
          "provided data product, but provide no guarantee of any kind.", 
          "The user assumes all risk associated with using these data.")
    ),
    
    tabPanel("Net Primary Productivity",
        sidebarLayout(
            sidebarPanel(
                addPointUI("npp_in"),
                helpText("There is a limit of 10,000 points for this variable."),
                tags$hr(),
                checkboxGroupInput("npp_stat", "Statistic",
                    choices = list("General mean" = "mean", 
                                   "Mean annual minimum" = "min",
                                   "Mean annual maximum" = "max",
                                   "Intra-annual standard deviation" = "sd",
                                   "Inter-annual standard deviation" = "interann_sd"),
                    selected = "mean"),
                p("Output values are in mg C /(m", tags$sup(2), "day)."),
                actionButton("npp_compute", "Compute", class = "btn-primary"),
                tags$hr(),
                p(tags$b("Flag legend")),
                p("0 = not interpolated", br(), 
                  "1 = interpolated from coastal/reef cells,", br(),
                  "2 = interpolated from other cells")
            ),
            mainPanel(
                pointOutputUI("npp_out")
            )
        )
    ),
    
    tabPanel("Land and Reef Area",
        sidebarLayout(
            sidebarPanel(
                addPointUI("area_in"),
                helpText("There is a limit 200 points up to 25km, 100 points up to", 
                         "50km, 50 points up to 100km and 25 points up to 200km."),
                tags$hr(),
                radioButtons("which_area", "Which area?", 
                             c("land", "reef", "both"), selected = "both"),
                sliderInput("area_dist", "Distance (in km)",
                             value = 20, min = 5, max = 200),
                p("Output values are in km", tags$sup(2), "."),
                actionButton("area_compute", "Compute", class = "btn-primary")
            ),
            mainPanel(
                pointOutputUI("area_out")
            )
        )
    ),
    
    tabPanel("Wave Energy",
        sidebarLayout(
            sidebarPanel(
                addPointUI("wave_in"),
                helpText("There is a limit of 10,000 points for this variable."),
                tags$hr(),
                checkboxGroupInput("wave_stat", "Statistic", 
                    choices = list("General mean" = "mean", 
                                   "Intra-annual standard deviation" = "sd",
                                   "Inter-annual standard deviation" = "interann_sd"),
                    selected = "mean"),
                p("Output values are in kW/m."),
                actionButton("wave_compute", "Compute", class = "btn-primary"),
                hr(),
                p(tags$b("Legend")),
                p("The", tags$i("wind_fetch"), "value is 1 for sheltered", 
                  "points, where the wave energy is calculated from local",
                  "wind conditions and fetch length."),
                p("The", tags$i("ww3_res"), "value indicates the resolution",
                  "of the original WAVEWATCH III grid at that point:", br(),
                  "1 = 30 arc-min, 2 = 10 arc-min, 3 = 4 arc-min.")
            ),
            mainPanel(
                pointOutputUI("wave_out")
            )
        )
    ),
    
    tabPanel("Human Population",
        sidebarLayout(
            sidebarPanel(
                addPointUI("pop_in"),
                helpText("There is a limit 200 points up to 25 km, 100 points up to", 
                         "50 km, 50 points up to 100 km and 25 points up to 200 km."),
                tags$hr(),
                checkboxGroupInput("pop_year", "Years", 
                                   seq(1990, 2020, 5), selected = 2015),
                sliderInput("pop_dist", "Distance (in km)",
                             value = 20, min = 5, max = 200),
                actionButton("pop_compute", "Compute", class = "btn-primary")
            ),
            mainPanel(
                pointOutputUI("pop_out")
            )
        )
    ),
    
    tabPanel("Distance to Market",
        sidebarLayout(
            sidebarPanel(
                addPointUI("mark_in"),
                helpText("There is a limit of 10,000 points for this variable."),
                tags$hr(),
                p("Output value is the distance (in km) to the nearest provincial capital."),
                actionButton("mark_compute", "Compute", class = "btn-primary")
            ),
            mainPanel(
                pointOutputUI("mark_out")
            )
        )
    )
))


#### Server function ####

server <- function(input, output, session) {
    
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
        } else if (nrow(pts$df) == 0) {
            showNotification("Please first input point coordinates.", type = "error")
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
        } else if (nrow(pts$df) == 0) {
            showNotification("Please first input point coordinates.", type = "error")
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
        } else if (nrow(pts$df) == 0) {
            showNotification("Please first input point coordinates.", type = "error")
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
        } else if (nrow(pts$df) == 0) {
            showNotification("Please first input point coordinates.", type = "error")
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
        } else if (nrow(pts$df) == 0) {
            showNotification("Please first input point coordinates.", type = "error")
        } else {
            coords <- pts$df[, c("long", "lat")]
            coords$long <- coords$long - (coords$long > 180) * 360
            withProgress(message = "Processing...", value = 0, {
                dist_market <- vapply(1:nrow(coords), function(i) {
                    incProgress(1/nrow(coords), detail = paste0(i, "/", nrow(coords)))
                    min(distGeo(coords[i, ], capitals)) / 1000
                }, 0)
            })
            pts$df$dist_market <- dist_market
        }
    })
    
}

shinyApp(ui, server)