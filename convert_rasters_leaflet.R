library(raster)
library(leaflet)

# Convert a raster rast for displaying in leaflet
#  fact is the aggregation factor
convert_raster <- function(rast, outfile, fact = 3) {
    rast <- rotate(aggregate(rast, fact = fact))
    rast <- projectRasterForLeaflet(rast)
    writeRaster(rast, outfile)
    NULL
}
    
out_dir <- "/nfs/coralreef-public-data/msec/data/Leaflet"

# Convert rasters with single layers
layers <- c("npp_mean", "npp_sd", "npp_max", "npp_min", "npp_sdinter",
            "distmarket", "landarea_15km", "landarea_50km", "reefarea_15km",
            "reefarea_200km", "wave_mean", "wave_sd", "wave_sdinter")
in_files <- paste0("../msec_", layers, ".nc")
out_files <- file.path(out_dir, paste0("web_", layers, ".grd"))

for (i in seq_along(layers)) {
    convert_raster(raster(in_files[i]), out_files[i])
}

# Convert multi-layer population rasters
pop20 <- brick("../msec_humanpop_20km.nc")
pop50 <- brick("../msec_humanpop_50km.nc")
years <- pop20@z[[1]]
for (i in seq_along(years)) {
    convert_raster(pop20[[i]], 
             file.path(out_dir, paste0("web_humanpop_20km_", years[i], ".grd")))
    convert_raster(pop50[[i]], 
             file.path(out_dir, paste0("web_humanpop_50km_", years[i], ".grd")))
}

