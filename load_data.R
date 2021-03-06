library(raster)
library(rgdal)
library(leaflet)

# Location of data, accessible to shiny user
data_dir <- "/nfs/coralreef-public-data/msec/data"

# NPP data and flag layers
npp_files <- file.path(data_dir,
                       paste0("NPP/prod_",
                              c("mean", "min", "max", "sd", "interann_sd", "flag.grd")))
npp_stats <- stack(as.list(npp_files))
names(npp_stats) <- c("mean", "min", "max", "sd", "interann_sd", "flag")

# Reef and land rasters
reefs <- raster(file.path(data_dir, "ReefLandArea/reef_500"))
projection(reefs) <- "+proj=cea +lon_0=-160 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
gshhs <- raster(file.path(data_dir, "ReefLandArea/gshhs_rast.grd"))
gshhs_rotate <- raster(file.path(data_dir, "ReefLandArea/gshhs_rotated.grd"))

# Wave energy data and flag layers
wave_files <- file.path(data_dir, "WaveEnergy",
                        c(paste0("wave_", c("mean", "sd", "interann_sd"), ".grd"),
                          "wind_fetch_mask.grd", "ww3_resolution.grd"))
wave_stats <- stack(as.list(wave_files))
names(wave_stats) <- c("mean", "sd", "interann_sd", "wind_fetch", "ww3_res")

# Human population
pop90 <- brick(file.path(data_dir, "HumanPop/SEDAC_90_brick.grd"))
pop90_rotate <- brick(file.path(data_dir, "HumanPop/SEDAC_90_rot_brick.grd"))
names(pop90) <-  paste0("pop", c(1990, 1995))
names(pop90_rotate) <- names(pop90)
pop00 <- brick(file.path(data_dir, "HumanPop/SEDAC_00_brick.grd"))
pop00_rotate <- brick(file.path(data_dir, "HumanPop/SEDAC_00_rot_brick.grd"))
names(pop00) <- paste0("pop", seq(2000, 2020, 5))
names(pop00_rotate) <- names(pop00)

# Provincial capitals (for distance to market)
capitals <- readOGR(file.path(data_dir, "DistMark"), layer = "Provincial_capitals")

# Pre-load leaflet map layers
leaflet_map <- leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>%
    setView(0, 0, zoom = 3) %>%
    addRasterImage(raster(file.path(data_dir, "Leaflet/web_npp_mean.grd")),
                   colors = "Greens", opacity = 0.5, group = "Mean NPP",
                   project = FALSE) %>%
    addRasterImage(raster(file.path(data_dir, "Leaflet/web_reefarea_15km.grd")),
                   colors = "Blues", opacity = 0.5, group = "Reef Area w/i 15 km",
                   project = FALSE) %>%
    addRasterImage(raster(file.path(data_dir, "Leaflet/web_wave_mean.grd")), 
                   colors = "Purples", opacity = 0.5, group = "Mean Wave Energy", 
                   project = FALSE) %>%
    addRasterImage(raster(file.path(data_dir, "Leaflet/web_humanpop_50km_2015.grd")),
                   colors = "Reds", opacity = 0.5,
                   group = "Human Pop. w/i 50 km (2015)", project = FALSE) %>%
    addLayersControl(baseGroups = c("Mean NPP", "Reef Area w/i 15 km", 
                                    "Mean Wave Energy", "Human Pop. w/i 50 km (2015)"))
