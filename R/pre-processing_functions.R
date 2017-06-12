

#' Stack raster stacks
#'
#' The function loads the specified stacks and merges them into one, retaining original layer names.
#' @param files character vector of paths to raster stacks to be stacked
#' @param out_dir character string of path to output directory to which merged stack to be written. 
#' If NULL, function returns the merged stack.  
#' @param fname name of the rasterStack file to be written. Ignored if out_dir = NULL. 
#'
#' @return a rasterStack file
#' @export
#'
#' @examples
stack.stk <- function(files, out_dir = NULL, fname){
    r<-list()
    for(i in 1:length(files)){
        r[[i]] <- stack(files[i])
    }
    rs <- do.call(stack, r)
    if(is.null(out_dir)){return(rs)}else{
        writeRaster(rs, paste0(out_dir, fname, ".grd"), format="raster")
    }
    
}


#' Stack SpatialPolygonDataframes into single SPDF file
#' 
#' Function with load and bind SPDFs which can include layers with overlapping polygons.
#'
#' @param shp_list list of SpatialPolygonDataFrames to be stacked.
#' @param files character vector of paths to shapefile to be stacked.
#' @param out_dsn caharacter string. Path to output write directory.
#' @param out_layer character string. Output write layer name.
#'
#' @return if out_dsn = NULL, returns stacked SPDF. Else writes file to out_dsn 
#' and returns nothing.
#' @export
#'
#' @examples
stack.shp <- function(shp_list = NULL, files = NULL, out_dsn = NULL, out_layer = NULL) {
    if(all(is.null(shp_list), is.null(files))){
        stop("no shp_list or vector of .shp file paths supplied")
    }
    if(!is.null(shp_list)){
        shp <- do.call(rbind, shp_list)
    }else{
        for(file in files){
            cat("____________________________________________________________", "\n")
            cat("processing shapefile: ", which(files == file), "\n")
            
            if(file == files[1]){
                shp <- readOGR(dsn = file, 
                               gsub(".shp", "", 
                                    basename(as.character(file))))
                df_temp <- shp@data[0,]
            }else{
                add.shp <-  readOGR(dsn = file, 
                                    gsub(".shp", "", 
                                         basename(as.character(file))))
                add.shp <- add.shp[,names(add.shp) %in% names(df_temp)]
                add.shp@data <- suppressWarnings(bind_rows(df_temp, add.shp@data))
                shp <- rbind(shp, add.shp)
            }
            cat("-----------------------------------------------------", "\n")
        }
    }
    row.names(shp@data) <- 1:nrow(shp@data)
    if(is.null(out_dsn)){
        return(shp)}else{
            writeOGR(shp, dsn = out_dsn, driver="ESRI Shapefile", layer= out_layer,
                     overwrite_layer=TRUE)
        }
}
