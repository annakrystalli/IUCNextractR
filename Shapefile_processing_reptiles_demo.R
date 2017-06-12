rm(list=ls())

# ---- install-depend ----
depend <- c("rgeos", "rgdal","sp","maptools", "spatstat",
            "data.table", "stringr", "ggplot2", "scales",
            "compiler", "parallel", "plyr", "foreach", 
            "doParallel", "doMC", "Hmisc", "raster", "ncdf4", "chron")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(depend, character.only = T)

# ---- functions ----
#UPDATE:setwd to folder in which scripts have been saved
setwd("~/Documents/workflows/Sex Roles in Birds/birds/")
source("R/Shapefile_processing_functions.R")
source("R/custom_functions.R")


# ---- extract-setup ----
# UPDATE if not checked out from the repo
options(stringsAsFactors = F)
input.folder <- "data/"
spp.folder <- "shp/REPTILES/"
env.folder <-"bil/"
output.folder <- "outputs/"
wd.spp <-paste(input.folder, spp.folder, sep = "")
wd.env <-paste(input.folder, env.folder, sep = "")
wd.output <- paste(input.folder, output.folder, sep = "")
dir.create(path = wd.output, showWarnings = F)

create_output_folders(out.dir)

## ---- single-shp ----
# Because polygon data for all species is in a single file, load it as master.shp
dsn <- path.expand(wd.spp)
# layer <- "REPTILES" # name of the master.shp without .shp
# writeOGR(master.shp, dsn = dsn, paste0(layer, "_demo"), driver="ESRI Shapefile")
layer <- "REPTILES" # name of the master.shp without .shp
master.shp <- readOGR(dsn = dsn, layer)
# check the names of the @data slot within the .shp (MANUAL) and define master.shp_spp.names 
names(master.shp@data)
master.shp_spp.names <- "binomial"
# PROVIDE master.shp AND master.shp_spp.names AS ARGUMENTS TO getSppRow()
spp.files <- unique(master.shp@data[,master.shp_spp.names])

## ---- multiple-shp ----
# if individual polygons are in individual files, these will be loaded within 
# getSppRow() function, spp.files should be the names of the .shp files contained 
# in the shapefile folder
# spp.files <- list.files(wd.spp, pattern = ".shp")
 
env.files <- list.files(wd.env, pattern = ".bil")

# make vector of variable names
bios <- gsub(".bil", "", env.files)
# sort alphanumeric codes
bios <- bios[order(nchar(bios), bios)][1:2] # currently set to tmean1 & 2. 
# ensure matching .hdr files are also in the bil folder.
bios_corr <- bios

# ---- register-cluster ----
cores <- detectCores()  
# set up cluster
cl <- makeCluster(cores - 2)
# register cores
registerDoMC(cores=cores)

# ---- run-parallel ----
# Run the function getSppRow across all species in analysis. 
# - spp.file:
#   a) names of shapefiles in "data/shp/" if species spatial data are in individual shapefiles.
#   b) spp.files as the name of a species in a master.shp shapefile where each row 
# refers to a polygon area associated with a species. 
#     - master.shp: The master shapefile 
#     - master.shp_spp.names: The name of the column in shp@data in which species names 
# - overwrite: logical. if T, all extractions are rerun and previous outputs overwritten. If F,
#   function will load any existing outputs and extract data.
# - bios_corr: many of the bioclim temp variables are in units of C*10 so need 
# to be corrected to C by `/10`. In your case all will need correcting so set to bios
# - presence: presence IUCN categories to be included in analysis
# - trim.to.data: logical. If T, seasonal & presence are trimmed to values which also exist in 
#   shp@data$seasonal & shp@data$presence respectively. Only applies if master.shp is supplied.
min <- 1
max <- 3
spp.dat.parallel <- foreach(x = spp.files[min:max], .combine = rbind,
                             .inorder = F, .errorhandling = "remove") %dopar%{
                               depend <- c("rgeos", "rgdal","sp","maptools", "spatstat",
                                           "data.table", "stringr", "ggplot2", "scales",
                                           "compiler", "parallel", "plyr", "foreach", 
                                           "doParallel")
                               lapply(depend, require, character.only = TRUE)
                               
                               getSppRow(spp.file = x, wd.spp, wd.env, wd.output, bios, 
                                         input.folder, overwrite = F,
                                         master.shp, master.shp_spp.names,
                                         fixholes = F, bios_corr = bios_corr,
                                         trim.to.data = T)}

stopCluster(cl)