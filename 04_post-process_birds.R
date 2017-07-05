rm(list=ls())
sessionInfo()
# ---- install-depend ----
source("R/dependencies.R")
cat("\n", "dependencies: ", depend, "\n", "\n")

if (!require("pacman")) install.packages("pacman")
pacman::p_unload()
pacman::p_load(c("tidyverse", "SDMTools"), character.only = T)


expand_dates <- function(df) {
    df <- separate(df, layer_name, into = c("year", "month"), sep = "-")
    df$year <- as.numeric(df$year)
    df$month <- as.numeric(df$month)
    df
}

br_month_ids <- function(species, start, end) {
    m_id <- rep(1:12, 2)
    x_m_id <- m_id[which(m_id == floor(start))[1]:length(m_id)]
    if(is.na(end)){x_m_id <- x_m_id[1:4]}else{
        x_m_id <- x_m_id[1:which(x_m_id == floor(end))[1]]
    }
    b_months <- tibble(species = species, month = 1:12, wt = 0)
    b_months$wt[b_months$month %in% x_m_id] <- 1
    b_months$wt[b_months$month == x_m_id[1]] <- 1 - (start - floor(start))
    if(length(x_m_id) > 1){
        if(is.na(end)){
            b_months$wt[b_months$month == tail(x_m_id,1)] <- 1 - (start - floor(start))
        }else{
            b_months$wt[b_months$month == tail(x_m_id, 1)] <- end - floor(end)}   
    }
    return(b_months)
}

get_months_df <- function(b_s) {
    months <- NULL
    for(i in 1:nrow(b_s)){
        months <- br_month_ids(b_s$species[i], b_s$breed.season0[i], b_s$breed.season1[i]) %>%
            rbind(months)
    }
    return(months)
}

add_monthly_wts <- function(df, months) {
    df <- expand_dates(df) %>%
        filter(is.na(error)) %>%
        left_join(months, by = c("species", "month")) %>%
        mutate(br_season = !is.na(wt))
    df$wt[is.na(df$wt)] <- 1
    return(df)
}

get_e_summaries <- function(var, months) {
    df <- add_monthly_wts(df = get(var, envir = .GlobalEnv), months)
    df$area_lost <- 1- df$area_lost
    yr_gp <- group_by(df, species, year) %>% summarise(yr_wt.mean = wt.mean(weighted.mean, wt),
                                                       yr_wt.sd = wt.sd(weighted.mean, wt), 
                                                       season_length = sum(wt),
                                                       br_season = unique(br_season),
                                                       area = unique(area),
                                                       area_lost = unique(area_lost)) %>%
        summarise(br_season = unique(br_season), 
                  season_length = unique(season_length),
                  area = unique(area),
                  area_lost = unique(area_lost),
                  mean = mean(yr_wt.mean), 
                  w_seas.sd = mean(yr_wt.sd))
    
    mm_gp <- group_by(df, species, month) %>% 
        summarise(yr_wt.sd = wt.sd(weighted.mean, wt)) %>%
        filter(!is.nan(yr_wt.sd)) %>%
        summarise(betw_yr.sd = mean(yr_wt.sd))
    
    
    
    out <- left_join(yr_gp, mm_gp, by = "species") %>% add_column(env_name = var, .before = 1)
    return(out)
}

# read renvironmental files
tmp <- read_csv("data/outputs/data/2017-06-21_birds_tmp.csv")
pre <- read_csv("data/outputs/data/2017-06-22_birds_pre.csv")
all <- rbind(tmp, pre)

# read breeding period and match files
match <- read_csv("data/inputs/matched_BioClim.csv")
b_s <- read_csv("data/inputs/breed_periods.csv")
b_s$species <- match$synonyms[match(b_s$species, match$species)]
b_s <- b_s[!is.na(b_s$breed.season0),]
b_s <- b_s[!duplicated(b_s$species),]
# add 1 to align b_s months with month indices in data
b_s[which(b_s == 12, arr.ind = T)] <- 0
b_s[,2:3] <- b_s[,2:3] + 1
months <- get_months_df(b_s)


out.tmp <- get_e_summaries("tmp", months) 
out.pre <- get_e_summaries("pre", months)
out <- rbind(out.tmp, out.pre)
out$species <- match$species[match(out$species, match$synonyms)]
write_csv(out, path = "data/outputs/summaries.csv")
