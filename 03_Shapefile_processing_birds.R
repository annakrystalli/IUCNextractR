#rm(list=ls())
print(sessionInfo())
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
    "_____________________________________________", "\n", "\n")

# ---- functions ----
source("R/Shapefile_processing_functions.R")
source("R/custom_functions.R")

## ---- single-shp ----
# Because polygon data for all species is in a single file, load it as master.shp
layer <- "birds"
dsn <- paste0(wd, "data/inputs/pre-processed/shp/birds/")
bird.files <- gsub(".shp", "", list.files(dsn, pattern = ".shp", full.names = F) %>%
    as.character() %>% basename())
bird.files <- setNames(bird.files, gsub("_[0-9].*$", "", bird.files))

## ---- single-shp ----
# Because polygon data for all species is in a single file, load it as master.shp
dsn <- paste0(wd, "data/inputs/shp/birds/")
layer <- "birds" # name of the master.shp without .shp
master.shp <- readOGR(dsn = dsn, layer)
# check the names of the @data slot within the .shp (MANUAL) and define master.shp_spp.names 
# (ie the column containing species names) 
names(master.shp@data)
master.shp_spp.names <- "SCINAME"
# make sure species names are separated by "_".
master.shp@data[,master.shp_spp.names] <- gsub(" ", "_", master.shp@data[,master.shp_spp.names])
# PROVIDE master.shp AND master.shp_spp.names AS ARGUMENTS TO get_spp_e_stats()
species <- unique(master.shp@data[,master.shp_spp.names])

# subset from batch. Only run if start & end arguments supplied in commandArgs pos 4 & 5.
if(length(commandArgs(trailingOnly = TRUE)) == 5){
    start <- as.numeric(commandArgs(trailingOnly = TRUE)[4])
    end <- as.numeric(commandArgs(trailingOnly = TRUE)[5])
    bird.files <- bird.files[start:end]
    seq_id <- paste0("_", start, "-", end)
}else{
    seq_id <- ""
}

# ---- load environmental raster data ----
env_raster <- stack(paste0(wd, "data/inputs/raster/", env_name,".grd"))
if(exists("env_raster")){cat("env_raster file", 
                             paste0(wd, "data/inputs/raster/", env_name,".grd"), 
                             " loaded successfully", "\n", "\n")}else{
                                 cat("ERROR: env_raster not loaded", "\n", "\n")
                             }
env_dat <- mm_yyyy_df.rs(env_raster) %>% 
    filter(year >= 1996 & year <= 2015)
env_layers <- setNames(env_dat$layer, env_dat$code)

# ---- run-parallel ----
# Run the function getSppRow across all species in analysis. 
# - spp_name: the name of a species 
#     - master.shp: The master shapefile 
#     - master.shp_spp.names: The name of the column in shp@data in which species names 

# - presence: presence IUCN categories to be included in analysis
# ---- register-cluster ----
registerDoParallel(cores = cores)
cat("---------++++ LAUNCH PARALLEL WORKFLOW ++++----------", "\n")
t0 <- Sys.time()
spp.dat.parallel <- foreach(x = names(bird.files), .combine = rbind,
                            .inorder = F, .errorhandling = "remove") %dopar%{
                                #pacman::p_load(depend, character.only = T)
                                get_spp_e_stats(spp_name = x, 
                                                spp_shp = master.shp[master.shp@data[,master.shp_spp.names] == x,], 
                                                dsn = NULL,   
                                                env_name, env_raster, env_layers, 
                                                presence = 1:2, seasonal = 1:2, 
                                                e_statistics = c("weighted.mean", 
                                                                 "max", "min", 
                                                                 "weighted.var", 
                                                                 "weighted.median"),
                                                fixholes = F)
                            }
cat("--- COMPLETE ---", "\n", "\n")
cat("TIME ELAPSED:", Sys.time() - t0, "\n", "\n")
print(spp.dat.parallel)
write_csv(spp.dat.parallel, paste0(wd, "data/outputs/data/", Sys.Date(), 
                                   "_", layer,"_", env_name, seq_id,".csv"))
cat("$$$_____**** OUTPUTS SAVED as", paste0(wd, "data/outputs/data/", Sys.Date(), 
                                            "_", layer,"_", env_name, seq_id,".csv")
    ," ****_____$$$", "\n")
