# The script extracts statistical summaries of environmental parameters, given 
# as a raster stack, across IUCN species ranges, loaded from a SINGLE MASTER 
# SHAPEFILE. 
# 
# It designed to be run both locally and on a cluster. To parametarise analysis 
# from a shell script, provide parameter values for `ncores` (the number of cores 
#  to parallelise across), `env_name` (the name of the environmental raster stack 
#   to load) and `WD` (the working directory on the cluster) in a call to Rscript. 
#  (See 02_batch_shapefile_processing_reptiles.sge for example).

# NOTE: The cru decadal NetCDF files have been stacked into a single raster stack
# which is subset to the required period before analysis. Used function `stack.stk()`
# from R/pre-processing_functions.R 

#rm(list=ls())
sessionInfo()
# ---- install-depend ----
source("R/dependencies.R")
cat("dependencies: ", depend, "\n", "\n")

if (!require("pacman")) install.packages("pacman")
pacman::p_unload()
pacman::p_load(depend, character.only = T)

## ---- extract-cmd ----
# extract command line arguments (if applicable)
if(length(commandArgs(trailingOnly = TRUE)) == 0){
    cores <- detectCores() - 1
    env_name <- "tmp"
    wd <- ""
    }else{
        cores <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
        env_name <- as.character(commandArgs(trailingOnly = TRUE)[2])
        wd <- as.character(commandArgs(trailingOnly = TRUE)[3])
    }
cat("*** Analysis paramenters ***", "\n",
    "ncores: ", cores, "\n", 
    "env_name: ", env_name, "\n",
    "WD: ", wd, "\n",
    "_____________________________________________", "\n")

# ---- functions ----
source("R/Shapefile_processing_functions.R")
source("R/custom_functions.R")


# ---- extract-setup ----
## ---- single-shp ----
# Because polygon data for all species is in a single file, load it as master.shp
dsn <- "data/inputs/shp/REPTILES/"
layer <- "REPTILES" # name of the master.shp without .shp
master.shp <- readOGR(dsn = dsn, layer)
# check the names of the @data slot within the .shp (MANUAL) and define master.shp_spp.names 
# (ie the column containing species names) 
names(master.shp@data)
master.shp_spp.names <- "binomial"
# make sure species names are separated by "_".
master.shp@data[,master.shp_spp.names] <- gsub(" ", "_", master.shp@data[,master.shp_spp.names])
# PROVIDE master.shp AND master.shp_spp.names AS ARGUMENTS TO get_spp_e_stats()
species <- unique(master.shp@data[,master.shp_spp.names])

# ---- load environmental raster data ----
env_raster <- stack(paste0(wd, "data/inputs/raster/", env_name,".grd"))
if(exists("env_raster")){cat("env_raster file", 
    paste0(wd, "data/inputs/raster/", env_name,".grd"), " loaded successfully", "\n")}else{
    cat("ERROR: env_raster not loaded", "\n")
    }
# get dates from layer names and subset raster stack to only the time period of interest
env_dat <- mm_yyyy_df.rs(env_raster) %>% 
    filter(year >= 1960 & year <= 1990)
env_layers <- setNames(env_dat$layer, env_dat$code)

# ---- extract-parallel ----
# register cores
registerDoParallel(cores = cores)
# Launch parallel extraction: Run the function getSppRow across all species in analysis. 
#   - spp_name: the name of a species in a master.shp shapefile 
#   - spp.shp: The master shapefile 
#   - master.shp_spp.names: The name of the column in master.shp@data containing species names 
#   - env_name: name of the environmental variable being extracted 
#   - env_raster: the environmental raster stack to be extracted
#   - env_layers: named vector of raster stack layer indices to be included, 
#       named with the layer code.
#   - presence: presence IUCN categories to be included in analysis
#   - seasonal: seasonal IUCN categories to be included in analysis
#   - e_statistics: what summary statistics to calculated on from the extracted 
#       environmental values.
#          
cat("---------++++ LAUNCH PARALLEL WORKFLOW ++++----------", "\n")
spp.dat.parallel <- foreach(x = species, .combine = rbind,
                            .inorder = F, .errorhandling = "remove") %dopar%{
                                #pacman::p_load(depend, character.only = T)
                                get_spp_e_stats(spp_name = x, 
                                                spp_shp = master.shp[master.shp@data[,master.shp_spp.names] == x,], 
                                                dsn = NULL, 
                                                env_name, env_raster, 
                                                env_layers, presence = 1:5, seasonal = 1:2, 
                                                e_statistics = c("weighted.mean", "max", "min", "weighted.var", "weighted.median"),
                                                fixholes = F)
                            }
cat("--- COMPLETE ---", "\n")
print(spp.dat.parallel)
write_csv(spp.dat.parallel, paste0(wd, "data/outputs/data/", Sys.Date(), "_", layer,"_", env_name, ".csv"))
cat("$$$_____**** OUTPUTS SAVED as", paste0(wd, "data/outputs/data/", Sys.Date(), "_", layer,"_", env_name, ".csv")
    ," ****_____$$$", "\n")

# NEED TO RE-RUN ANALYSIS SEPARATELY FOR EACH ENVIRONMENTAL PARAMETER
