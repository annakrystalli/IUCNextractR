#rm(list=ls())
sessionInfo()
# ---- install-depend ----
source("R/dependencies.R")

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


# register cores
registerDoParallel(cores = cores)
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
