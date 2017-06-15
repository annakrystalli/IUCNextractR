# Pre-processing script to stack all bird.file SPDFs into one SPDF. Workflow parallelised 
# to improve speed

# ---- install-depend ----
source("R/dependencies.R")

if (!require("pacman")) install.packages("pacman")
pacman::p_unload()
pacman::p_load(depend, character.only = T)

source("R/custom_functions.R")
source("R/pre-processing_functions.R")

bird.files <- list.files("data/inputs/pre-processed/shp/birds/", 
                         pattern = ".shp", full.names = T)

# ---- register-cluster ----

if(length(commandArgs(trailingOnly = TRUE)) == 0){
    cores <- detectCores() - 1}else{
        cores <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
    }


# ---- split-bird-files-vec ----
par_files <- split(bird.files, cut(seq_along(bird.files), breaks = cores))

# set up cluster
registerDoParallel(cores = cores)

# ---- stack-birdfiles-parallel
par_shp <- foreach(par_file = par_files) %dopar% stack.shp(files = par_file, out_dsn = NULL)
stack.shp(shp_list = par_shp, out_dsn = "data/inputs/shp/birds/",
          out_layer = "birds")

