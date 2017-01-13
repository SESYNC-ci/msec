library(raster)
library(rgdal)

# NPP data and flag layers
npp_files <- c(paste0("data/NPP/prod_", c("mean", "min", "max", "sd", "interann_sd")),
               "data/NPP/prod_flag.grd")
npp_stats <- stack(as.list(npp_files))
names(npp_stats) <- c("mean", "min", "max", "sd", "interann_sd", "flag")

# Reef and land rasters
reefs <- raster("data/ReefLandArea/reef_500")
projection(reefs) <- "+proj=cea +lon_0=-160 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
gshhs <- raster("data/ReefLandArea/gshhs_rast.grd")
gshhs_rotate <- raster("data/ReefLandArea/gshhs_rotated.grd")

# Wave energy data and flag layers
wave_files <- file.path("data/WaveEnergy",
                        c(paste0("wave_", c("mean", "sd", "interann_sd"), ".grd"),
                          "wind_fetch_mask.grd", "ww3_resolution.grd"))
wave_stats <- stack(as.list(wave_files))
names(wave_stats) <- c("mean", "sd", "interann_sd", "wind_fetch", "ww3_res")

# Human population
pop90 <- brick("data/HumanPop/SEDAC_90_brick.grd")
pop90_rotate <- brick("data/HumanPop/SEDAC_90_rot_brick.grd")
names(pop90) <-  paste0("pop", c(1990, 1995))
names(pop90_rotate) <- names(pop90)
pop00 <- brick("data/HumanPop/SEDAC_00_brick.grd")
pop00_rotate <- brick("data/HumanPop/SEDAC_00_rot_brick.grd")
names(pop00) <- paste0("pop", seq(2000, 2020, 5))
names(pop00_rotate) <- names(pop00)

# Provincial capitals (for distance to market)
capitals <- readOGR("data/DistMark", layer = "Provincial_capitals")
