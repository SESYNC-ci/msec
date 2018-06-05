#### Load/update packages ####

pkgs <- data.frame(
    name = c("geosphere", "raster", "rgdal", "rgeos", "leaflet"),
    version = c("1.5.5", "2.5.8", "1.1.10", "0.3.20", "1.1.0"),
    stringsAsFactors = FALSE
)

for (i in 1:nrow(pkgs)) {
    if (!(pkgs$name[i] %in% installed.packages()) || 
        packageVersion(pkgs$name[i]) < pkgs$version[i]) {
        install.packages(pkgs$name[i], repos="http://cran.us.r-project.org")
    }
    library(pkgs$name[i], character.only = TRUE)
}


# Convert polygon from (-180, 180) to (0, 360) longitude range
#  Assumes input is a SpatialPolygons with a single Polygon
rotate_poly <- function(poly) {
    poly
    coords <- poly@polygons[[1]]@Polygons[[1]]@coords
    coords[coords[, 1] < 0, 1] <- coords[coords[, 1] < 0, 1] + 360
    SpatialPolygons(
        list(Polygons(list(Polygon(coords, hole = FALSE)), 1)),
        proj4string = CRS(proj4string(poly))
    )
}

# Calculate reef area within dist of point (long, lat)
reef_area <- function(long, lat, dist) {
    pt <- SpatialPoints(cbind(0, 0), 
                        proj = CRS(paste0("+proj=aeqd +lon_0=", long, 
                                          " +lat_0=", lat, " +unit=m")))
    buf <- gBuffer(pt, width = dist, quadsegs = 20)
    buf <- spTransform(buf, projection(reefs))
        
    reef_crop <- crop(reefs, buf, snap = "out")
    cell_area <- 0.25
    suppressWarnings(
        extract(reef_crop, buf, fun = sum, na.rm = TRUE) * cell_area
    )
}

# Calculate land area within dist of point (long, lat)
land_area <- function(long, lat, dist) {
    pt <- SpatialPoints(cbind(0, 0), 
                        proj = CRS(paste0("+proj=aeqd +lon_0=", long, 
                                          " +lat_0=", lat, " +unit=m")))
    buf <- gBuffer(pt, width = dist, quadsegs = 20)
    buf <- spTransform(buf, projection(gshhs))
    
    if (abs(abs(long) - 180) < 3) {
        buf <- rotate_poly(buf)
        land_crop <- crop(gshhs_rotate, buf, snap = "out")
    } else {
        land_crop <- crop(gshhs, buf, snap = "out")
    }
    cell_areas <- area(land_crop)
    area_mask <- mask(cell_areas, land_crop, maskvalue = 0)
    suppressWarnings(
        extract(area_mask, buf, fun = sum, na.rm = TRUE)
    )
}

# Calculate human population within dist of point (long, lat) for given years
human_pop <- function(long, lat, dist, years) {
    pt <- SpatialPoints(cbind(0, 0), 
                        proj = CRS(paste0("+proj=aeqd +lon_0=", long, 
                                          " +lat_0=", lat, " +unit=m")))
    buf <- gBuffer(pt, width = dist, quadsegs = 20)
    buf <- spTransform(buf, projection(pop00))
    yrs90 <- years[years < 2000]
    yrs00 <- years[years >= 2000]
        
    # For points close to 180 meridian, used rotated (0-360) longitude
    if (abs(abs(long) - 180) < 3) {
        buf <- rotate_poly(buf)
        if (length(yrs90) > 0) {
            pop90_crop <- crop(subset(pop90_rotate, paste0("pop", yrs90)), 
                               buf, snap = "out")
        }
        if (length(yrs00) > 0) {
            pop00_crop <- crop(subset(pop00_rotate, paste0("pop", yrs00)), 
                               buf, snap = "out")
        }
    } else {
        if (length(yrs90) > 0) {
            pop90_crop <- crop(subset(pop90, paste0("pop", yrs90)), 
                               buf, snap = "out")
        }
        if (length(yrs00) > 0) {
            pop00_crop <- crop(subset(pop00, paste0("pop", yrs00)), 
                               buf, snap = "out")
        }
    }
    if (length(yrs90) > 0) {
        ext90 <- suppressWarnings(extract(pop90_crop, buf, fun = sum, na.rm = TRUE))
    } else {
        ext90 <- NULL
    }
    if (length(yrs00) > 0) {
        ext00 <- suppressWarnings(extract(pop00_crop, buf, fun = sum, na.rm = TRUE))
    } else {
        ext00 <- NULL
    }
    res <- round(cbind(ext90, ext00))
    colnames(res) <- paste0("pop", years)
    res
}
