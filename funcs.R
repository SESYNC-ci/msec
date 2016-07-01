get_prod_mean <- function(lat,long) {
    # Find cell corresponding to target location and calculate crop extent
    irow <- rowFromY(depth30, lat)
    icol <- colFromX(depth30, long)
    crop_area <- extent(depth30, irow-nbuf, irow+nbuf, icol-nbuf, icol+nbuf)
    # Crop productivity brick and depth/interpolation masks
    prod_crop <- crop(npp_brick, crop_area)
    depth30_crop <- crop(depth30, crop_area)
    interp_crop <- crop(interp, crop_area)
    
    # Compute mean prod. and number of non-NA values by cell
    prod_mean <- mean(prod_crop, na.rm=TRUE)
    prod_counts <- calc(prod_crop, function(x) {sum(!is.na(x))})
    # Filter out shallow cells
    prod_mean[depth30_crop == 1] <- NA
    if(is.na(prod_mean[nbuf+1,nbuf+1])) {
        # Find candidate cells for interpolation
        # Keep only those within distance interp_d of target point
        prod_mean[interp_crop == 0] <- NA
        dists <- distanceFromPoints(prod_mean, c(long,lat))
        dists[is.na(prod_mean)] <- NA
        dists[dists > interp_d] <- NA
        ninterp <- min(sum(!is.na(getValues(dists))), ninterp_max)
        if (ninterp == 0) {
            # If no cell available for interpolation, return NA
            return(NA)
        }
        else {
            # Get indices of ninterp closest cells (interp_cells)
            # Return their mean, as well as the distances and prod.counts for those ninterp cells 
            interp_cells <- order(getValues(dists))[1:ninterp]
            close_dists <- getValues(dists)[interp_cells]
            close_values <- getValues(prod_mean)[interp_cells]
            interp_mean <- mean(close_values)
            close_counts <- getValues(prod_counts)[interp_cells]
            #return(list(interp_mean = interp_mean, 
            #            close_dists = close_dists, close_counts = close_counts))
            return(interp_mean)
        }
    }
    else {
        # If there's good data in target cell, just report mean and count
        #return(list(mean = prod_mean[nbuf+1,nbuf+1], count = prod_counts[nbuf+1,nbuf+1]))
        return(prod_mean[nbuf + 1, nbuf + 1])
    }
}