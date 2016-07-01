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
    
    tabPanel("Reef Area",
        textOutput("reef_area")
    )
))
