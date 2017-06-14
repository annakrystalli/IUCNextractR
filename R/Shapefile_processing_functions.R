

#____________________________________________________________________________
#.....FUNCTIONS

#____________________________________________________________________________

get_spp_e_stats <- function(spp_name, spp_shp, env_name, env_raster, 
                              env_layers = NULL, presence = 1:2, seasonal = 1:4, 
                              e_statistics = c("weighted.mean", "max", "min", "weighted.var", "weighted.median"),
                              fixholes = T){
    
    if(nrow(spp_shp@data) == 0){
        return(error_out(spp_name, env_name, e_statistics, error = "no polys"))
    }
    
    # subset shapefile
    names(spp_shp@data) <- tolower(names(spp_shp@data))
    spp_shp <- spp_shp[spp_shp$presence %in% presence,]
    if(nrow(spp_shp@data) == 0){
        return(error_out(spp_name, env_name, e_statistics, error = "no presence"))
    }
    spp_shp<- spp_shp[spp_shp$seasonal %in% seasonal,]
    if(nrow(spp_shp@data) == 0){
        return(error_out(spp_name, env_name, e_statistics, error = "no seasonal"))
    }
    if(fixholes){spp_shp <- fix.holes(spp_shp)}
    
    
    # ---- mask environmental raster ----
    # project to epsg:4326
    crs(env_raster) <- crs("+init=epsg:4326")
    crs(spp_shp) <- crs("+init=epsg:4326")
    # subset raster stack layers
    if(class(env_raster) == "RasterStack" & !is.null(env_layers)){
        env_raster <- subset(env_raster, env_layers)
    }

    # Use polygons to mask raster. Use mask to get pixel info and relative pixel width weights
    env_mask <- raster::mask(env_raster[[1]], spp_shp)
    pix_info <- tibble(pix_id = raster::Which(!is.na(env_mask), cells=T)) 
    pix_info <- bind_cols(pix_info, as.tibble(xyFromCell(env_mask, pix_info$pix_id))) %>%
        mutate(wts = latWts(y, xres(env_raster), yres(env_raster)))
    # if no non NA pixels can be extracted, return data row with error
    if(nrow(pix_info) == 0){
        return(error_out(spp_name, env_name, e_statistics, error = "no non-NA data extracted"))
    }

    out <- as.tibble(raster::extract(env_raster, pix_info$pix_id)) %>%
        map(map_e_statistics, wts = pix_info$wts, e_statistics) %>% as.tibble() %>% t() %>% as.tibble()
    names(out) <- e_statistics
    out <- add_column(out,
                      species = spp_name,
                      env_name = env_name,
                      layer_id = env_layers, 
                      layer_name = names(env_layers), 
                      area = env_mask %>% raster::area(na.rm=TRUE) %>% cellStats("sum"),
                      area_lost = area_lost(env_raster, spp_shp),
                      .before = 1) %>% add_column(error = NA)
  return(out)}

# ---- Helpers ----
# create output folder to save outputs in.
create_output_folders <- function(out.dir) {
    dir.create(path = paste(out.dir, "processed_shp/", sep = ""), showWarnings = F)
    dir.create(path = paste(out.dir, "range_data/", sep = ""), showWarnings = F)
    dir.create(path = paste(out.dir, "error_reports/", sep = ""), showWarnings = F)
}

# fixholes in Spatial Polygon (SP) object
fix.holes <- function(sp.obj) {
    require(rgeos)
    require(stringr)
    if(!inherits(sp.obj, "SpatialPolygons")) stop("Input object must be of class SpatialPolygons")
    pls = slot(sp.obj, "polygons")
    pls1 = lapply(pls, checkPolygonsHoles)
    slot(sp.obj, "polygons") = pls1
    return(sp.obj)
}


# Calculate latitudinal weights for environmental variables. Used to correct
# calculation of region wide statistics
latWts <- function(lats, xres, yres){
    l <- cos((lats-yres/2)*pi/180)^2 *cos(xres*pi/180)
    l0 <- cos(mean(lats-yres/2)*pi/180)^2 *cos(xres*pi/180)
    l/l0}


error_out <- function(spp_name, env_name, e_statistics, error) {
    out_names <- c("species", "env_name", "layer_id", "layer_name", "area", "area_lost", e_statistics, "error")
    out <- as.tibble(matrix(NA, nrow = 1, ncol = length(out_names))) %>%
        setNames(out_names)
    out[, c("species", "env_name", "error")] <- c(spp_name, env_name, error)
    return(out)
}


area_lost <- function(env_raster, spp_shp) {
    area_rast <- env_raster[[1]]
    values(area_rast) <- 1
    env_mask <- raster::mask(env_raster[[1]], spp_shp)
    lost <- env_mask %>% raster::area(na.rm=TRUE) %>% cellStats("sum") / raster::mask(area_rast, spp_shp) %>% raster::area(na.rm=TRUE) %>% cellStats("sum") 
    if(lost == 1){return(NA)}else{return(lost)}
}


# map_e_statistics function
map_e_statistics <- function(e.vals, wts, e_statistics){
    f <- lapply(e_statistics, get) %>% setNames(e_statistics)
    sapply(e_statistics, FUN = function(x, f, e.vals, wts){
        if(grepl("weight", x)){f[[x]](e.vals, wts)}else{f[[x]](e.vals)}},
        f, e.vals, wts = wts)
}