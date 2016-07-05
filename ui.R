library(shiny)

shinyUI(navbarPage("Marine Data",

    tabPanel("Net Primary Productivity",
        sidebarLayout(
            sidebarPanel(
                fileInput("npp_file", "Location data (.csv)", 
                          accept = c("text/csv", "text/comma-separated-values")),
                tags$hr(),
                radioButtons("npp_stat", "Statistic", c("mean")),
                actionButton("npp_compute", "Compute")
            ),
            mainPanel(
                    fluidRow(column(12, align = "right",
                        downloadButton("npp_download", "Download Output")
                    )),
                    tags$br(),
                    dataTableOutput("npp_out")
            )
        )
    ),
    
    tabPanel("Land and Reef Area",
        sidebarLayout(
            sidebarPanel(
                fileInput("area_file", "Location data (.csv)", 
                          accept = c("text/csv", "text/comma-separated-values")),
                tags$hr(),
                radioButtons("which_area", "Which area?", 
                             c("land", "reef", "both"), 
                             selected = "both"),
                numericInput("dist", "Distance (in km)",
                             value = 75, min = 10, max = 200),
                actionButton("area_compute", "Compute")
            ),
            mainPanel(
                fluidRow(column(12, align = "right",
                    downloadButton("area_download", "Download Output")
                )),
                tags$br(),
                dataTableOutput("area_out")
            )
        )
    )
))
