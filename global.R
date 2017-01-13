
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
    output$dtab <- renderDataTable(pts$df, option = list(pageLength = 10))
    
    output$download <- downloadHandler(
        filename = "msec_out.csv",
        content = function(file) write.csv(pts$df, file, row.names = FALSE)
    )
    
    observeEvent(input$clear, {
        pts$df = data.frame(long = numeric(0), lat = numeric(0))       
    })
}
