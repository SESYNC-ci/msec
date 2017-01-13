source("funcs.R")

navbarPage("Marine Socio-Environmental Covariates", 
    theme = shinytheme("cerulean"), # cerulean or cosmo
    
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
        h3("How to use this site"),
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
        h3("Citation"),
        p("If you use these data products, please cite the following article:"),
        p("Yeager, L.A., Marchand, P., Gill, D.A., Baum, J.K., and",
          "McPherson, J.M. In review. Queryable global layers of environmental", 
          "and anthropogenic variables for marine ecosystem studies."),
        h3("Terms of use"),
        p("MESC is released is an open-access data product distributed",
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
)
