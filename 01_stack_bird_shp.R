# Pre-processing script to stack all bird.file SPDFs into one SPDF. Workflow parallelised 
# to improve speed
print(sessionInfo())
# ---- install-depend ----
source("R/dependencies.R")

if (!require("pacman")) install.packages("pacman")
pacman::p_unload()
pacman::p_load(depend, character.only = T)

source("R/custom_functions.R")
source("R/pre-processing_functions.R")

# ---- register-cluster ----

if(length(commandArgs(trailingOnly = TRUE)) == 0){
    cores <- detectCores() - 1
    wd <- ""}else{
        cores <- as.numeric(commandArgs(trailingOnly = TRUE)[1])
        wd <- as.character(commandArgs(trailingOnly = TRUE)[2])
    }
out_dir <- paste0(wd,"data/inputs/shp/birds/")

cat("*** Analysis paramenters ***", "\n",
    "ncores: ", cores, "\n", 
    "WD: ", wd, "\n",
    "out_dir: ", out_dir, "\n",
    "_____________________________________________", "\n", "\n")

t2 <- Sys.time()
if(!file.exists(paste0(out_dir,"par_shp.RData"))){
    ## ---- list-files ----
    bird.files <- list.files("data/inputs/pre-processed/shp/birds/", 
                             pattern = ".shp", full.names = T)
    # ---- split-bird-files-vec ----
    par_files <- split(bird.files, cut(seq_along(bird.files), breaks = cores))
    # set up cluster
    registerDoParallel(cores = cores)
    # ---- stack-birdfiles-parallel
    par_shp <- foreach(par_file = par_files) %dopar% stack.shp(files = par_file, out_dsn = NULL)
    cat("parallel file loading successfull: ", exists("par_shp"),"\n","\n")
    save(par_shp, file = paste0(out_dir,"par_shp.RData"))
}else{
    cat("## -----+++ par_shp loaded from disk ++++-----##", "\n", "\n")
    load(file = paste0(out_dir,"par_shp.RData"))  
}

# stack and writeOGR to single master SPDF
stack.shp(shp_list = par_shp, out_dsn = out_dir, out_layer = "birds")

cat("##______________$$ .SHP STACK COMPLETE $$____________##", "\n")
cat("TIME ELAPSED:", Sys.time() - t2, "\n", "\n")
