# Pre-processing script to stack all bird.file SPDFs into one SPDF. Workflow parallelised 
# to improve speed

# ---- install-depend ----
depend <- c("tidyverse", "rgeos", "rgdal","sp","maptools", "spatstat",
            "data.table", "stringr", "scales",
            "compiler", "parallel", "foreach", 
            "doParallel", "doMC", "Hmisc", "raster", "ncdf4", "chron")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(depend, character.only = T)

source("R/custom_functions.R")
source("R/pre-processing_functions.R")

bird.files <- list.files("data/inputs/pre-processed/shp/", 
                         pattern = ".shp", full.names = T)[1:11]

# ---- register-cluster ----
cores <- detectCores() - 2

# ---- split-bird-files-vec ----
par_files <- split(bird.files, cut(seq_along(bird.files), breaks = cores))

# set up cluster
registerDoParallel(cores = cores)

# ---- stack-birdfiles-parallel
par_shp <- foreach(par_file = par_files) %dopar% stack.shp(files = par_file, out_dsn = NULL)
stack.shp(shp_list = par_shp, out_dsn = "data/inputs/shp/birds/",
          out_layer = "birds")

