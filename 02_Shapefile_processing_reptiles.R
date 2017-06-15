#rm(list=ls())

# ---- install-depend ----
source("R/dependencies.R")

if (!require("pacman")) install.packages("pacman")
pacman::p_unload()
pacman::p_load(depend, character.only = T)

# ---- functions ----
#UPDATE:setwd to folder in which scripts have been saved
#setwd("~/Documents/workflows/Sex Roles in Birds/birds/")
source("R/Shapefile_processing_functions.R")
source("R/custom_functions.R")


# ---- extract-setup ----
# UPDATE if not checked out from the repo
#output.folder <- "outputs/"
dir.create(path = "outputs/data/" , showWarnings = F)
#create_output_folders(out.dir)

## ---- single-shp ----
# Because polygon data for all species is in a single file, load it as master.shp
dsn <- "data/inputs/shp/REPTILES/"
layer <- "REPTILES" # name of the master.shp without .shp
master.shp <- readOGR(dsn = dsn, layer)
# check the names of the @data slot within the .shp (MANUAL) and define master.shp_spp.names 
names(master.shp@data)
master.shp_spp.names <- "binomial"
master.shp@data[,master.shp_spp.names] <- gsub(" ", "_", master.shp@data[,master.shp_spp.names])
# PROVIDE master.shp AND master.shp_spp.names AS ARGUMENTS TO getSppRow()
species <- unique(master.shp@data[,master.shp_spp.names])

## ---- multiple-shp ----
# if individual polygons are in individual files, these will be loaded within 
# getSppRow() function, spp.files should be the names of the .shp files contained 
# in the shapefile folder
# spp.files <- list.files(wd.spp, pattern = ".shp")

# make vector of variable names


# ---- select data ----
if(length(commandArgs(trailingOnly = TRUE)) == 0){
    env_name <- "tmp"}else{
        env_name <- as.character(commandArgs(trailingOnly = TRUE)[2])
        cat("env_name: ", env_name, "\n")
    }

env_raster <- stack(paste0("data/inputs/raster/", env_name,".grd"))
env_dat <- mm_yyyy_df.rs(env_raster) %>% 
    filter(year >= 1960 & year <= 1990)
env_layers <- setNames(env_dat$layer, env_dat$code)


# ---- run-parallel ----
# Run the function getSppRow across all species in analysis. 
# - spp_name: the name of a species in a master.shp shapefile where each row 
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
# ---- register-cluster ----

if(length(commandArgs(trailingOnly = TRUE)) == 0){
    cores <- detectCores() - 1}else{
        cores <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
        cat("ncores: ", cores)
    }
# register cores
registerDoParallel(cores = cores)

spp.dat.parallel <- foreach(x = species, .combine = rbind,
                            .inorder = F, .errorhandling = "remove") %dopar%{
                                #pacman::p_load(depend, character.only = T)
                                get_spp_e_stats(spp_name = x, spp_shp = master.shp[master.shp@data[,master.shp_spp.names] == x,], env_name, env_raster, 
                                                env_layers, presence = 1:5, seasonal = 1:2, 
                                                e_statistics = c("weighted.mean", "max", "min", "weighted.var", "spatstat::weighted.median"),
                                                fixholes = F)
                            }

write_csv(paste0("outputs/data/", Sys.Date(), "_", layer,"_", env_name, ".csv"))