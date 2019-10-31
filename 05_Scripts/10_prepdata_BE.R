
# Step 0. Basic setup: libraries & working directory ----------------------

if (!require("foreign")) install.packages("foreign", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("plyr")) install.packages("plyr", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tidyverse")) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)

if (!require("sf")) install.packages("sf", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tidycensus")) install.packages("tidycensus", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tigris")) install.packages("tigris", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("devtools")) install.packages("devtools")
library(devtools)
if (!require("lehdr")) devtools::install_github("jamgreen/lehdr")

if (!require("mapview")) install.packages("mapview", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tmap")) install.packages("tmap", repos = "http://cran.us.r-project.org", dependencies = TRUE)

if (!require("psych")) install.packages("psych", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tableone")) install.packages("tableone", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("MatchIt")) install.packages("MatchIt", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("optmatch")) install.packages("optmatch", repos = "http://cran.us.r-project.org", dependencies = TRUE)

# detach(package:  )
# detach(package:plyr)
# detach(package:dplyr)
# detach(package:tidyverse)

library(foreign)
library(plyr)
library(tidyverse)

library(sf)
library(tidycensus)
library(tigris)
library(lehdr)
library(mapview)
library(tmap)

library(psych)
library(tableone)
library(MatchIt)
library(optmatch)
library(survival)

options(stringsAsFactors = FALSE)
# check the integer max value 
#.Machine$integer.max 

if (dir.exists("C:/Users/Yongs/")){
  setwd("C:/Users/Yongs/Dropbox (GaTech)/3a_ResearchCEE/09_Uber_NHTS/RH_VO")
} else {
  setwd("C:/Users/ylee366/Dropbox (GaTech)/3a_ResearchCEE/09_Uber_NHTS/RH_VO")
} 

if (dir.exists("C:/Users/Yongs/")){
  filepath <- "C:/Users/Yongs/Dropbox (GaTech)/3a_ResearchCEE/09_Uber_NHTS"
} else {
  filepath <- "C:/Users/ylee366/Dropbox (GaTech)/3a_ResearchCEE/09_Uber_NHTS"
} 
    
options(tigris_use_cache = TRUE)
tigris_cache_dir(paste0(filepath, "/05_Census/tigris"))

# If tigris_cache_dir(paste0(filepath, "/05_Census/tigris")) doesn't work, 
# Run below scripts line by line from the function.
# path <- paste0(filepath, "/05_Census/tigris")
# home <- Sys.getenv("HOME")
# renv <- file.path(home, ".Renviron")
# if (!file.exists(renv)) {
#   file.create(renv)
# }
# check <- readLines(renv)
# if (isTRUE(any(grepl("TIGRIS_CACHE_DIR", check)))) {
#   oldenv <- read.table(renv, stringsAsFactors = FALSE)
#   newenv <- oldenv[-grep("TIGRIS_CACHE_DIR", oldenv), 
#                    ]
#   write.table(newenv, renv, quote = FALSE, sep = "\n", 
#               col.names = FALSE, row.names = FALSE)
# }
# var <- paste0("TIGRIS_CACHE_DIR=", "'", path, "'")
# write(var, renv, sep = "\n", append = TRUE)
# message(sprintf("Your new tigris cache directory is %s. \nTo use now, restart R or run `readRenviron('~/.Renviron')`", 
#                 path))

readRenviron("~/.Renviron")
Sys.getenv('TIGRIS_CACHE_DIR')

# https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf
if (Sys.getenv("CENSUS_API_KEY") != "3b1d8912e33aa2d4c01bf1abc84729cfeb7cd6cd"){
  census_api_key("3b1d8912e33aa2d4c01bf1abc84729cfeb7cd6cd", install = TRUE, overwrite=TRUE)
  readRenviron("~/.Renviron") # First time, reload your environment so you can use the key without restarting R.
} 
Sys.getenv("CENSUS_API_KEY")


# Step 1. Read in input files ------------------------------------------------

## Task 1-1. Prepare a data frame of block group FIPS codes in top 50 UA (by popoulation in 2013-2017 ACS) ----

# start.time <- Sys.time() # https://stackoverflow.com/questions/6262203/measuring-function-execution-time-in-r
# end.time <- Sys.time()
# end.time - start.time

st      <- states(cb = TRUE, resolution = "20m", year = 2017) 
st_proj <- st %>% st_as_sf() %>% st_transform(crs = 3857) %>% select(GEOID, NAME)
ua      <- urban_areas(cb = FALSE, year = 2017)  
ua_proj <- ua %>% st_as_sf() %>% st_transform(crs = 3857) %>% select(UACE10, NAME10, ALAND10)

# var.acs <- load_variables(2017, "acs5", cache = TRUE)
ua_pop_raw <- get_acs(geography = "urban area", variables = c("B01001_001"), year = 2017, output ="wide") 

ua_pop <-ua_pop_raw %>%
  filter(grepl("San Juan, PR", NAME) == FALSE) %>%
  mutate(pop2017 = B01001_001E, 
         rank_pop = min_rank(desc(B01001_001E))) %>%
  select(-NAME, -B01001_001E, -B01001_001M) %>%
  arrange(rank_pop) %>%
  filter(rank_pop<=50)

ua50_proj <- ua_proj %>%
  right_join(ua_pop, by = c("UACE10" = "GEOID")) %>%
  arrange(rank_pop)

list_st <- vector("list", 50)
for (i in 1:50){
  list_st[[i]] <- st_proj[ua50_proj[i, ], ]$GEOID
}

download_bg_sp <- function(x){
  map(x, ~block_groups(state = ., cb = FALSE, year = 2017))
}
list_bg <- map(list_st, download_bg_sp) # this task took 14.57996 mins

map_lgl(list_bg, is.null) 
map(list_bg, ~class(.[[1]]))
map_lgl(list_bg, ~class(.[[1]]) == "SpatialPolygonsDataFrame")

list_bg_rb <- vector("list", 50)
for (i in 1:50){
  temp <- 
    rbind_tigris(list_bg[[i]]) %>%
    st_as_sf() %>%
    select(GEOID, ALAND) %>% 
    st_transform(crs = 3857)
  rownames(temp) <- NULL 
  list_bg_rb[[i]] <- temp 
}

write_rds(list_bg_rb, paste0(filepath, "/11_Scratch/list_bg_rb.rds"))
# list_bg_rb <- read_rds(paste0(filepath, "/11_Scratch/list_bg_rb.rds"))

list_bg_within <- vector("list", 50)
for (i in 1:50){
  list_bg_within[[i]] <- list_bg_rb[[i]] %>%
    st_point_on_surface() %>% 
    st_join(ua50_proj[i, ]) %>%  
    filter(is.na(UACE10) == FALSE)
}

# a <- map_int(list_bg_rb, nrow)
# b <- map_int(list_bg_within, nrow) 
# (a - b)/a *100 

write_rds(list_bg_within, paste0(filepath, "/11_Scratch/list_bg_within_centroid.rds")) # first converted to centroids 
# list_bg_within <- read_rds(paste0(filepath, "/11_Scratch/list_bg_within_centroid.rds"))

shorten <- function(x){
  temp <- x[c("GEOID", "UACE10", "NAME10")]
  st_geometry(temp) <- NULL
  return(temp)
}

list_bg_df <- 
  map(list_bg_within, shorten) %>%
  bind_rows() %>%
  as_tibble() %>%
  mutate(NAME10 = gsub("--", "-", NAME10))

write_rds(list_bg_df, paste0(filepath, "/11_Scratch/list_bg_centroid_df.rds")) # first converted to centroids 
# list_bg_df <- read_rds(paste0(filepath, "/11_Scratch/list_bg_centroid_df.rds")) 

## check if the same block groups belong to different UAs 
# bg_multi <- list_bg_df %>%
#   group_by(GEOID) %>%
#   summarize(n = n()) %>%
#   filter(n>1)
# 
# list_bg_df %>%
#   semi_join(bg_multi) %>%
#   arrange(GEOID)


bgd <- list_bg_df %>%
  select(-NAME10) %>%
  mutate(
    HHSTFIPS = substr(GEOID, 1, 2),
    HHCNTYFP = substr(GEOID, 3, 5),
    HHCT     = substr(GEOID, 6, 11),
    HHBG     = as.integer(substr(GEOID, 12, 12)),
    tr       = substr(GEOID, 1, 11)
    ) %>%
  select(HHSTFIPS, HHCNTYFP, HHCT, HHBG, GEOID, UACE10, tr)

write_rds(bgd, paste0(filepath, "/11_Scratch/bgd.rds"))
# bgd <- read_rds(paste0(filepath, "/11_Scratch/bgd.rds"))

## check if a census tract is part of two UAs: two cases found 
# bgd %>%
#   group_by(tr) %>%
#   summarize(nUA = n_distinct(UACE10)) %>%
#   filter(nUA > 1)
# # 1 25005611202     2
# # 2 36119012502     2
# 
# quick_check <- function(x, y){
#   x %>% filter(substr(GEOID, 1, 11) == y) %>% nrow()
# }
# 
# c(1:50)[map_int(list_bg_within, function(x) quick_check(x, "25005611202")) > 0]
# c(1:50)[map_int(list_bg_within, function(x) quick_check(x, "36119012502")) > 0]
# 
# ua50_proj$NAME10[map_int(list_bg_within, function(x) quick_check(x, "25005611202")) > 0]
# ua50_proj$NAME10[map_int(list_bg_within, function(x) quick_check(x, "36119012502")) > 0]

trd <- bgd %>%
  group_by(tr, UACE10) %>%
  summarize() # include two duplicates 
  
write_rds(trd, paste0(filepath, "/11_Scratch/trd.rds"))
# trd <- read_rds(paste0(filepath, "/11_Scratch/trd.rds"))



# Step 2. prepare BE/neighborhood-level IV attributes -------------------------------------------

## Task 2-1. Download ACS 2013-2017 for 300 st-cnty combinations ---- 

var.acs <- load_variables(2017, "acs5", cache = TRUE)
# var.acs %>%
#   filter(grepl("^YEAR STRUCTURE BUILT", concept))

list_stcnty <- trd %>%
  mutate(
    ST = substr(tr, 1, 2), 
    CNTY = substr(tr, 3, 5)
  ) %>%
  group_by(ST, CNTY) %>%
  summarize() 

varlist <- c("B01001_001", "B11016_001",                                           # # of people, # of households  
             "B08301_001", "B08301_010", "B08301_018", "B08301_019",               # MEANS OF TRANSPORTATION TO WORK
             "B25024_001", "B25024_002", "B25024_003", "B25024_004", "B25024_005", # UNITS IN STRUCTURE
             "B25024_006", "B25024_007", "B25024_008", "B25024_009",               # UNITS IN STRUCTURE
             "B25003_001", "B25003_002", "B25003_003",                             # owner/renter occupied units 
             "B25034_001", "B25034_002", "B25034_003", "B25034_004", "B25034_005", # structure built year
             "B25034_006", "B25034_007", "B25034_008", "B25034_009", "B25034_010", # structure built year
             "B25034_011")                                                         # structure built year

# https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
# https://stackoverflow.com/questions/46104176/how-to-run-a-function-multiple-times-and-write-the-results-to-a-list

get_acs_tract <- function(i){
  tryCatch(
    {
      get_acs(geography = "tract", state = list_stcnty$ST[i], county = list_stcnty$CNTY[i],
              variables = varlist, year=2017, output = "wide", cache_table = TRUE)
    }, 
    error=function(cond){
      # message("Here's the original error message:")
      message(cond)
      return(NA)
    }
  )
}

tract_acs2017_raw <- map(1:nrow(list_stcnty), get_acs_tract) # took about 40 minutes 
get_acs_tract_okay <- c(1:300)[map_lgl(tract_acs2017_raw, is.list)]        
get_acs_tract_fail <- c(1:300)[map_lgl(tract_acs2017_raw, ~!is.list(.))]  

map(get_acs_tract_fail, function(x) tract_acs2017_raw[[x]] <<- get_acs_tract(x))

write_rds(tract_acs2017_raw, paste0(filepath, "/11_Scratch/tract_acs2017_raw.rds"))
# tract_acs2017_raw <- read_rds(paste0(filepath, "/11_Scratch/tract_acs2017_raw.rds"))

tract_acs2017_rb <- tract_acs2017_raw %>%
  bind_rows() %>% 
  select(-NAME, -ends_with("M")) %>%
  mutate(
    pop = B01001_001E,
    hh  = B11016_001E,
    hu_total = B25024_001E,
    hu_sfh   = B25024_002E + B25024_003E,
    hu_mfh   = B25024_004E + B25024_005E + B25024_006E + B25024_007E + B25024_008E + B25024_009E,
    pctsfh   = ifelse(B25024_001E>0, hu_sfh/B25024_001E*100, 0),
    pctmfh   = ifelse(B25024_001E>0, hu_mfh/B25024_001E*100, 0),
    hu_occ   = B25003_001E,
    pctowner  = ifelse(B25003_001E>0, B25003_002E/B25003_001E*100, 0),
    pctrenter = ifelse(B25003_001E>0, B25003_003E/B25003_001E*100, 0),
    commuter_all = B08301_001E,
    commuter_pct_tr = ifelse(B08301_001E>0, B08301_010E/B08301_001E*100, 0),
    commuter_pct_bk = ifelse(B08301_001E>0, B08301_018E/B08301_001E*100, 0),
    commuter_pct_wk = ifelse(B08301_001E>0, B08301_019E/B08301_001E*100, 0),
    pctbuilt_bf50 = ifelse(B25034_001E>0, (B25034_010E + B25034_011E)/B25034_001E*100, 0),
    pctbuilt_5069 = ifelse(B25034_001E>0, (B25034_008E + B25034_009E)/B25034_001E*100, 0),
    pctbuilt_7089 = ifelse(B25034_001E>0, (B25034_006E + B25034_007E)/B25034_001E*100, 0),
    pctbuilt_9009 = ifelse(B25034_001E>0, (B25034_004E + B25034_005E)/B25034_001E*100, 0),
    pctbuilt_10af = ifelse(B25034_001E>0, (B25034_002E + B25034_003E)/B25034_001E*100, 0)
  ) %>%
  select(-ends_with("E"))

# sum(is.na(tract_acs2017_rb)) no missing 



## Task 2-2. Download sp objects for 300 st-cnty via tigris::tracts ----

tigris_cache_dir(paste0(filepath, "/05_Census/tigris"))
readRenviron('~/.Renviron')
options(tigris_use_cache=TRUE)

# https://www.census.gov/quickfacts/fact/note/US/LND110210
# unit: square meter -> square mile

get_tiger_tract <- function(i){
  a <- Sys.time()
  tryCatch(
    {
      temp1 <- tracts(list_stcnty$ST[i], list_stcnty$CNTY[i], cb = TRUE, year=2017)
      temp2 <- temp1@data %>%
        as_tibble() %>%
        select(GEOID, ALAND) %>%
        mutate(sqmile = as.double(ALAND)/1000000*0.386102159) %>%
        select(-ALAND)
      b <- Sys.time()
      print(paste0(as.character(i), "th iteration out of 300 is just finished."))
      print(b - a) 
      return(temp2)
    }, 
    error=function(cond){
      # message("Here's the original error message:")
      message(cond)
      b <- Sys.time()
      print(paste0(as.character(i), "th iteration out of 300 is just finished."))
      print(b - a) 
      return(NA)
    }
  )
}

tract_area_raw <- map(1:nrow(list_stcnty), get_tiger_tract)
# map(tract_area_raw, class) %in% c("tbl_df", "tbl", "data.frame") 
write_rds(tract_area_raw, paste0(filepath, "/05_Census/tract_area_raw.rds"))
# tract_area_raw <- read_rds(paste0(filepath, "/05_Census/tract_area_raw.rds"))

tract_area_rb <- dplyr::bind_rows(tract_area_raw)
head(tract_area_rb)
write_rds(tract_area_rb,  paste0(filepath, "/05_Census/tract_area_rb.rds"))
# tract_area_rb <- read_rds(paste0(filepath, "/05_Census/tract_area_rb.rds"))

# sum(is.na(tract_area_rb)) # sqmile: no missing
# n_distinct(tract_area_rb$GEOID) # 38,517 tracts in 300 counties
# nrow(tract_area_rb)

tract_be <- tract_acs2017_rb %>%                         # 38,571 tracts in 300 counties   (via tidycensus::get_acs)
  right_join(trd, by = c("GEOID" = "tr")) %>%            # 32,939 tracts in the top 50 UAs (via tigris::block_groups, aggregated for tracts)
  left_join(tract_area_rb) %>%                           # 38,517 tracts in 300 counties   (via tigris::tracts)
  select(GEOID, UACE10, sqmile, pop, hh, hu_total, hu_sfh, hu_mfh, everything())

write_rds(tract_be, paste0(filepath, "/05_Census/tract_be.rds"))
# tract_be <- read_rds(paste0(filepath, "/05_Census/tract_be.rds"))

# nrow(tract_be) # 32,939 tracts in top 50 UAs
# n_distinct(tract_be$GEOID) # two duplicates 
# sum(is.na(tract_be)) # no missing 



## Task 2-3. Download job counts at the block level for 37 states via lehdr::grab_lodes ---- 

# https://lehd.ces.census.gov/doc/help/onthemap/OnTheMapDataOverview.pdf
# https://github.com/jamgreen/lehdr

## make a character vector of 37 state abbr.  
list_st_fips <- 
  list_stcnty$ST %>%
  table() %>%
  names() %>% 
  as.data.frame()

colnames(list_st_fips) <- "fips_st"

list_st_lookup <- read_csv(paste0(filepath, "/05_Census/list_fips_st.csv"))
list_st_names <- left_join(list_st_fips, list_st_lookup)$st  

## define a function that collects work-area job counts at the tract level 
get_lodes_wac <- function(x){
  grab_lodes(state = x, year = 2015, lodes_type = "wac", job_type = "JT00", 
             segment = "S000", state_part = "main", agg_geo = "tract", 
             download_dir = paste0(filepath, "/05_Census/lodes"))
}

## run a map function that iterates over 37 states 
lodes_wac_st <- map(list_st_names, get_lodes_wac)
# map_lgl(lodes_wac_st, ~class(.)[[1]] %in% c("tbl_df", "tbl", "data.frame")) %>% sum()

lodes_wac_df <- lodes_wac_st %>%
  bind_rows() %>%
  mutate(
    jobs = C000, 
    E5_Ret15 = CNS07,                                                  # retail 
    E5_Off15 = CNS09 + CNS10 + CNS11 + CNS13 + CNS20,                  # office
    E5_Ind15 = CNS01 + CNS02 + CNS03 + CNS04 + CNS05 + CNS06 + CNS08,  # industrial 
    E5_Svc15 = CNS12 + CNS14 + CNS15 + CNS16 + CNS19,                  # service
    E5_Ent15 = CNS17 + CNS18                                           # entertainment 
  ) %>%
  select(w_tract, jobs, E5_Ret15, E5_Off15, E5_Ind15, E5_Svc15, E5_Ent15)

## define a function that collects home-area job counts (in fact, worker counts) at the tract level 
get_lodes_rac <- function(x){
  grab_lodes(state = x, year = 2015, lodes_type = "rac", job_type = "JT00", 
             segment = "S000", state_part = "main", agg_geo = "tract", 
             download_dir = paste0(filepath, "/05_Census/lodes"))
}

lodes_rac_st <- map(list_st_names, get_lodes_rac)

lodes_rac_df <- lodes_rac_st %>%
  bind_rows() %>% 
  select(h_tract, workers = C000)

## merge back to the previous outcome, tract_be 
tract_be2 <- tract_be %>% 
  left_join(lodes_wac_df, by = c("GEOID" = "w_tract")) %>%
  left_join(lodes_rac_df, by = c("GEOID" = "h_tract")) %>%
  mutate(
    den_pop = pop/sqmile, 
    den_hh  = hh/sqmile, 
    den_hu  = hu_total/sqmile, 
    den_job = jobs/sqmile, 
    job_per_hh     = ifelse(hh >0,        jobs/hh,       0),  
    job_per_hu     = ifelse(hu_total > 0, jobs/hu_total, 0), 
    job_per_worker = ifelse(workers>0,    jobs/workers,  0)
    ) %>%
  select(-pop, -hh, -hu_total, -hu_sfh, -hu_mfh, -hu_occ) %>%
  select(GEOID, UACE10, sqmile, den_pop, den_hh, den_hu, den_job, job_per_hh, job_per_hu, job_per_worker, everything())

tract_entropy <- tract_be2 %>%
  select(GEOID, E5_Ret15:E5_Ent15) %>% 
  gather(E5_Ret15:E5_Ent15, key = "industry", value = "count") %>%
  filter(count>0) %>%
  arrange(GEOID) %>%
  group_by(GEOID) %>%
  mutate(
    n = n(), 
    job_total = sum(count), 
    sh = count/job_total, 
    lnsh = log(sh),  
    sh_lnsh = sh*lnsh
  ) %>%
  summarize(
    sum_sh = sum(sh_lnsh),
    mean_n = mean(n), 
    ln_n = log(mean_n), 
    entropy = ifelse(mean_n>1, -sum_sh/ln_n, 0) 
  ) %>%
  select(GEOID, entropy)

tract_be3 <- tract_be2 %>%
  left_join(tract_entropy) %>% 
  mutate(entropy = ifelse(is.na(entropy) == TRUE, 0, entropy)) %>%
  select(-jobs, -E5_Ret15, -E5_Off15, -E5_Ind15, -E5_Svc15, -E5_Ent15, -workers, -commuter_all) %>% 
  mutate(den_job        = ifelse(is.na(den_job)        == TRUE, 0, den_job), 
         job_per_hh     = ifelse(is.na(job_per_hh)     == TRUE, 0, job_per_hh),
         job_per_hu     = ifelse(is.na(job_per_hu)     == TRUE, 0, job_per_hu),
         job_per_worker = ifelse(is.na(job_per_worker) == TRUE, 0, job_per_worker)
  )

write_rds(tract_be3, paste0(filepath, "/05_Census/tract_be3.rds"))
# tract_be3 <- read_rds(paste0(filepath, "/05_Census/tract_be3.rds"))





## Task 2-4. Read in the US EPA Smart Location Database ---- 

sld <- read.dbf(paste0(filepath, "/04_SLD/SmartLocationDb.dbf"), as.is = TRUE) # read in character as character

tract_sld <- sld %>% 
  as.tibble() %>%
  mutate(GEOID = substr(GEOID10, 1, 11)) %>%
  semi_join(tract_be3) %>%
  group_by(GEOID) %>%
  summarize(
    nblkgrp = n(), 
    D3a.w    = weighted.mean(D3a,    TOTPOP10, na.rm = TRUE), 
    D3aao.w  = weighted.mean(D3aao,  TOTPOP10, na.rm = TRUE), 
    D3amm.w  = weighted.mean(D3amm,  TOTPOP10, na.rm = TRUE), 
    D3apo.w  = weighted.mean(D3apo,  TOTPOP10, na.rm = TRUE), 
    D3b.w    = weighted.mean(D3b,    TOTPOP10, na.rm = TRUE), 
    D3bao.w  = weighted.mean(D3bao,  TOTPOP10, na.rm = TRUE), 
    D3bmm3.w = weighted.mean(D3bmm3, TOTPOP10, na.rm = TRUE), 
    D3bmm4.w = weighted.mean(D3bmm4, TOTPOP10, na.rm = TRUE), 
    D3bpo3.w = weighted.mean(D3bpo3, TOTPOP10, na.rm = TRUE), 
    D3bpo4.w = weighted.mean(D3bpo4, TOTPOP10, na.rm = TRUE),
    D5ar.w   = weighted.mean(D5ar,   TOTPOP10, na.rm = TRUE) 
  )

tract_be4 <- tract_be3 %>% left_join(tract_sld)
write_rds(tract_be4, paste0(filepath, "/05_Census/tract_be4.rds"))
# tract_be4 <- read_rds(paste0(filepath, "/05_Census/tract_be4.rds"))



## Task 2-5. Run exploratory factor analysis on five LU measures, standardized by UA ---- 

### Task 2-5-1 prep data: log-transform ----
tract_be5 <- tract_be4[complete.cases(tract_be4), ]

# library("car")
# scatterplotMatrix(tract_be5[, c(3:12)])
# scatterplotMatrix(tract_be5[, c(13:22)])
# scatterplotMatrix(tract_be5[, c(23:32)])
# detach(package:car)

# install.packages("Hmisc", dependencies = TRUE)
# library(Hmisc, attach.required = TRUE)
# detach(package:Hmisc)

# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# install.packages("PerformanceAnalytics", dependencies = TRUE)
# library(PerformanceAnalytics, attach.required = TRUE)
# chart.Correlation(tract_be5[, c(3:7)], histogram = TRUE, pch = 19)
# detach(package:PerformanceAnalytics)

# install.packages("ggpubr", dependencies = TRUE)
# library(ggpubr, attach.required = TRUE)  
# ggdensity(tract_be5[[ ]])
# detach(package:ggpubr)

log.transform <- function(x){
  if(typeof(x) != "character"){
    x <- log(x+1)
  } else {
    x 
  }
}

tract_be6 <- map_df(tract_be5, log.transform)

map_chr(tract_be5, typeof)
names(tract_be5)

summary(tract_be5[3:20])
summary(tract_be6[3:20])

map(tract_be5[, c(3:35)], ~shapiro.test(sample(., size = 5000))$p.value) %>% unlist()
map(tract_be6[, c(3:35)], ~shapiro.test(sample(., size = 5000))$p.value) %>% unlist()


### Task 2-5-2 run factor analysis ----

# prep packages: http://dwoll.de/rexrepos/posts/multFA.html
wants <- c("GPArotation", "mvtnorm", "psych")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has], dependencies = TRUE)
# library(psych) # already loaded in the beginning 


# https://cran.r-project.org/web/packages/psych/psych.pdf
# http://personality-project.org/r/psych/HowTo/factor.pdf
# fit1 <- fa(beall5[, 2:6], nfactors=1, rotate = "oblimin", scores="Bartlett", fm="pa", SMC=TRUE, covar=FALSE)
# fit2 <- fa(beall5[, 2:6], nfactors=1, rotate = "oblimin", scores="Bartlett", fm="ml", SMC=TRUE, covar=FALSE)

names(tract_be6)
# dropped variables: 
# sqmile, pctbuilt_10af, entropy, nblkgrp, 
# commuter_pct_bk, D3aao.w, D3bao.w
# den_job
# D3amm.w, D3bmm3.w, D3bmm4.w 
# commuter_pct_tr  
# pctmfh, pctrenter, D5ar.w 

be.input <- tract_be6[, c(4:6, 8:11, 13, 17:21, 25, 28:29, 33:34)] 

# fa.parallel(x = be.input, fm = "ml", fa = "fa", cor="cor", SMC=TRUE)

fa.preset <- function(x){
  fa(r = be.input, nfactors = x, rotate = "oblimin", scores="Bartlett", 
     SMC=TRUE, warnings=TRUE, fm= "ml") #, oblique.scores=TRUE)
}

# fa3 <- fa.preset(3)
# fa4 <- fa.preset(4)
fa5 <- fa.preset(5)
# fa6 <- fa.preset(6)
# fa7 <- fa.preset(7)
# fa8 <- fa.preset(8)
# fa9 <- fa.preset(9)
# fa10 <- fa.preset(10)
# fa11 <- fa.preset(11)

# c(fa3$RMSEA[1], fa4$RMSEA[1], fa5$RMSEA[1], fa6$RMSEA[1], fa7$RMSEA[1],
#   fa8$RMSEA[1], fa9$RMSEA[1], fa10$RMSEA[1], fa11$RMSEA[1]) # below 0.05
# c(fa3$rms, fa4$rms, fa5$rms, fa6$rms, fa7$rms,
#   fa8$rms, fa9$rms, fa10$rms, fa11$rms)
# # if the residuals are particularly non-normal,
# # the rms value and the associated χ2 and RMSEA can differ substantially
# c(fa3$TLI, fa4$TLI, fa5$TLI, fa6$TLI, fa7$TLI,
#   fa8$TLI, fa9$TLI, fa10$TLI, fa11$TLI) # above 0.9

# https://www.promptcloud.com/blog/exploratory-factor-analysis-in-r/
# print(fa3$loadings, cutoff = 0.3)
# print(fa4$loadings, cutoff = 0.3)
print(fa5$loadings, cutoff = 0.3)
# print(fa6$loadings, cutoff = 0.3)
# print(fa7$loadings, cutoff = 0.3)
# print(fa8$loadings, cutoff = 0.3)
# print(fa9$loadings, cutoff = 0.3)
# print(fa10$loadings, cutoff = 0.3)
# print(fa11$loadings, cutoff = 0.3)

fa.diagram(fa5)
fa5 

fa5$scores[1:5, ]

be.5factors <- 
  fa5$scores %>% 
  as_tibble()

tract_be7 <- 
  tract_be6[, 1:2] %>% 
  dplyr::bind_cols(be.5factors)

write_rds(tract_be7, path = paste0(filepath, "/11_Scratch/tract_be7.rds"))
# tract_be7 <- read_rds(paste0(filepath, "/11_Scratch/tract_be7.rds"))

# install.packages("moments", dependencies = TRUE)
# library(moments)





## Task 2-6. Append instrument variables for the use of Uber, which captures *exogenous* variation in Uber supply ----  

# tract-level variables from 2008-2012 ACS 5-year estimates (mid-year 2010) 
# % college graduates, % young adults (25-34), % individuals without any vehicle

# LEHD 2010 
# count of jobs in food services, entertainment/arts, etc 
# standardize by UA

varlist2 <- c("B06009_001", "B06009_005", "B06009_006", "B06001_001", "B06001_005", 
              "B08014_001", "B08014_002") 

var2012 <- load_variables(2012, "acs5", cache = TRUE)
var2012 %>%
  filter(name %in% varlist2)

get_acs_tract2 <- function(i){
  start_time <- Sys.time()
  tryCatch(
    {
      get_acs(geography = "tract", state = list_st_fips[[1]][i],  
              variables = varlist2, year=2012, output = "wide", cache_table = TRUE)
    }, 
    error=function(cond){
      # message("Here's the original error message:")
      message(cond)
      return(NA)
    }
  )
  end_time <- Sys.time()
  end_time - start_time # Time difference  
}

# first-round api call 
# rm(tract_acs2012_raw)
start_time <- Sys.time()
tract_acs2012_raw <- map(1:37, get_acs_tract2) 
end_time <- Sys.time()
end_time - start_time # Time difference of 49.94235 mins

# second-round api call 
tract_acs2012_raw_okay <- c(1:37)[map_lgl(tract_acs2012_raw, is.list)]        
tract_acs2012_raw_fail <- c(1:37)[map_lgl(tract_acs2012_raw, ~!is.list(.))]   
write_rds(tract_acs2012_raw, path = paste0(filepath, "/11_Scratch/tract_acs2012_raw.rds"))
# tract_acs2012_raw <- read_rds(paste0(filepath, "/11_Scratch/tract_acs2012_raw.rds.rds"))

map(tract_acs2012_raw_fail, function(x) tract_acs2012_raw[[x]] <<- get_acs_tract2(x))  

# for loop 
# for ( c(1:37)[map_lgl(tract_acs2012_raw, ~!is.list(.))] %>% length() >= 1 ){
#   map(tract_acs2012_raw_fail, function(x) tract_acs2012_raw[[x]] <<- get_acs_tract2(x))  
#   tract_acs2012_raw_fail <- c(1:37)[map_lgl(tract_acs2012_raw, ~!is.list(.))]   
# }

st_tr_tb <- 
  trd %>%
  rename(GEOID = tr) %>%
  mutate(st = substr(GEOID, 1, 2)) %>%
  arrange(st, GEOID) %>%
  select(st, UACE10, GEOID) 

start_time <- Sys.time()
iv_acs00 <- vector("list", 37)
for (i in 1:37){
  iv_acs00[[i]] <- 
    st_tr_tb %>%
    filter(st == list_st_fips[[1]][i]) %>%
    left_join(tract_acs2012_raw[[i]], by = "GEOID") %>%
    mutate(
      pctcoll  = ifelse(B06009_001E>0, (B06009_005E + B06009_006E)/B06009_001E * 100, 0),
      pctyoung = ifelse(B06001_001E>0,  B06001_005E / B06001_001E * 100, 0),    
      pctxveh  = ifelse(B08014_001E>0,  B08014_002E / B08014_001E * 100, 0)  
    ) %>%
    select(st, UACE10, GEOID, pctcoll, pctyoung, pctxveh) 
  }

iv_acs01 <- iv_acs00 %>% bind_rows() # 32939 tracts 
# https://stackoverflow.com/questions/2851327/convert-a-list-of-data-frames-into-one-data-frame
end_time <- Sys.time()
end_time - start_time  

write_rds(iv_acs00, path = paste0(filepath, "/11_Scratch/iv_acs00.rds"))
# iv_acs00 <- read_rds(paste0(filepath, "/11_Scratch/iv_acs00.rds"))

write_rds(iv_acs01, path = paste0(filepath, "/11_Scratch/iv_acs01.rds"))
# iv_acs01 <- read_rds(paste0(filepath, "/11_Scratch/iv_acs01.rds"))

list_st_names_lowercase <- list_st_names %>% tolower()

start_time <- Sys.time()
iv_job00 <- vector("list", 37)
for(i in 1:37) {
  iv_job00[[i]] <- 
    grab_lodes(state=list_st_names_lowercase[i], year=2011, lodes_type="wac", agg_geo="tract") %>%
    select(w_tract, CNS09, CNS12, CNS17, CNS18)
  }
iv_job01 <- iv_job00 %>% bind_rows() 
# https://stackoverflow.com/questions/2851327/convert-a-list-of-data-frames-into-one-data-frame
end_time <- Sys.time()
end_time - start_time # Time difference of 39.05497 secs

write_rds(iv_job01, path = paste0(filepath, "/11_Scratch/iv_job01.rds"))
# iv_job01 <- read_rds(paste0(filepath, "/11_Scratch/iv_job01.rds"))


trd %>% filter(tr == "25005611202")
# A tibble: 2 x 2
# Groups:   tr [1]
# tr          UACE10
# <chr>       <chr> 
# 1 25005611202 09271 
# 2 25005611202 72505 

trd %>% filter(tr == "36119012502")
# A tibble: 2 x 2
# Groups:   tr [1]
# tr          UACE10
# <chr>       <chr> 
# 1 36119012502 10162 
# 2 36119012502 63217 


start_time <- Sys.time()
iv_job02 <- # 32939 tracts 
  trd %>% 
  group_by(tr) %>%
  left_join(iv_job01, by=c("tr" = "w_tract")) %>%
  mutate(
    GEOID = tr, 
    techjob = CNS09 + CNS12, 
    servjob = CNS17 + CNS18 
  ) %>%
  ungroup() %>% 
  select(GEOID, UACE10, techjob, servjob)
end_time <- Sys.time()
end_time - start_time # Time difference of 2.962098 mins


write_rds(iv_job02, path = paste0(filepath, "/11_Scratch/iv_job02.rds"))
# iv_job02 <- read_rds(paste0(filepath, "/11_Scratch/iv_job02.rds"))


start_time <- Sys.time()
iv_all00 <- 
  iv_acs01 %>%
  # group_by(GEOID) %>%
  # summarize(
  #   pctcoll = mean(pctcoll), 
  #   pctyoung = mean(pctyoung), 
  #   pctxveh = mean(pctxveh) 
  # ) %>% 
  # ungroup() %>%
  left_join(iv_job02, by=c("GEOID", "UACE10")) %>%
  left_join(tract_area_rb, by = "GEOID") %>%
  mutate(
    techden = techjob/sqmile, 
    servden = servjob/sqmile
  ) %>%
  select(GEOID, UACE10, pctcoll, pctyoung, pctxveh, techden, servden)
end_time <- Sys.time()
end_time - start_time # Time difference of 2.965564 mins

write_rds(iv_all00, paste0(filepath, "/11_Scratch/iv_all00.rds"))
# iv_all00 <- read_rds(paste0(filepath, "/11_Scratch/iv_all00.rds")) 


# standardize by UA 
start_time <- Sys.time()
iv_all01 <- 
  iv_all00 %>%
  mutate(
    ln.pctcoll  = log(pctcoll+1), 
    ln.pctyoung = log(pctyoung+1), 
    ln.pctxveh  = log(pctxveh+1), 
    ln.techden  = ifelse(is.na(techden), 0, log(techden+1)), 
    ln.servden  = ifelse(is.na(servden), 0, log(servden+1)) 
  ) %>% 
  group_by(UACE10) %>% 
  mutate(
    z.pctcoll =scale(ln.pctcoll), 
    z.pctyoung=scale(ln.pctyoung), 
    z.pctxveh =scale(ln.pctxveh), 
    z.techden =scale(ln.techden), 
    z.servden =scale(ln.servden)
  ) %>%
  ungroup()
end_time <- Sys.time()
end_time - start_time # Time difference of 0.07981992 secs

write_rds(iv_all01, paste0(filepath, "/11_Scratch/iv_all01.rds"))  # 32,939 tracts
# iv_all01 <- read_rds(paste0(filepath, "/11_Scratch/iv_all01.rds")) 


## Task 2-7. Scrape walkscore.com: using a different file of scripts ----

# trxy <- NULL 
# for (i in 1:37) {
#   temp1   <- tracts(ST[i], year=2016)
#   temp2   <- temp1@data[, c("GEOID", "INTPTLAT", "INTPTLON")]
#   temp3   <- merge(trd, temp2, by="GEOID")
#   trxy    <- rbind(trxy, temp3)
# }
# colnames(trxy) <- c("GEOID", "UACE10", "y", "x")
# trxy$x <- as.numeric(trxy$x)
# trxy$y <- as.numeric(trxy$y)
# rm("temp1", "temp2", "temp3")
# write.csv(trxy, "M:/Uber_NHTS/11_Scratch/trxy.csv")
# 
# 
# trxy <- read.csv("M:/Uber_NHTS/11_Scratch/trxy.csv")

#See another script that scrapes walkscores at the census tract level: To be updated


## Task 3-3. Uber suuply variable: Google Trends # of Searches (skip this time on 7/11/2019) ----

ua2016 <- urban_areas(cb=FALSE, year=2016)
ualist <- ua2016@data
ualist <- ualist[, c("UACE10", "NAME10")]
ualist$UACE10 <- as.integer(ualist$UACE10)
# rm("ua2016")

# data05 <- merge(data04, ualist, by="UACE10")
nhtsualist <- as.data.frame(table(data05$UACE10))
colnames(nhtsualist) <- c("UACE10", "cases")
nhtsualist$UACE10 <- as.integer(as.character(nhtsualist$UACE10))
nhtsualist <- merge(nhtsualist, ualist, by="UACE10")
nhtsualist <- nhtsualist[order(-nhtsualist$cases), ]
nrow(nhtsualist)

uber01 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber01.csv")
uber02 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber02.csv")
uber03 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber03.csv")
uber04 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber04.csv")
uber05 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber05.csv")
uber06 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber06.csv")
uber07 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber07.csv")
uber08 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber08.csv")
uber09 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber09.csv")
uber10 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber10.csv")
uber11 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber11.csv")
uber12 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber12.csv")
uber13 <- read.csv("M:/Uber_NHTS/09_Uber/GT_Uber13.csv")

ubersupply <- function(x) {
  x$TDAYDATE <- as.integer(paste(substr(x$Month, 1, 4), substr(x$Month, 6, 7), sep=""))
  varnam <- colnames(x)
  #https://www.r-bloggers.com/basic-text-string-functions-in-r/
  #https://stackoverflow.com/questions/6638072/escaped-periods-in-r-regular-expressions
  for (i in 2:6) {
    a <- gregexpr(pattern ='\\.', varnam[i])[[1]]
    b <- a[1]
    c <- a[length(a)-4]
    colnames(x)[i] <- substr(colnames(x)[i], b+1, c-1)
  }
  x <- x[, c(7, 2:6)]
} 

uberlist1 <- list(uber01, uber02, uber03, uber04, uber05, uber06, uber07, 
                  uber08, uber09, uber10, uber11, uber12)
uberlist2 <- lapply(uberlist1, ubersupply)
uber01 <- uberlist2[[1]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber02 <- uberlist2[[2]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber03 <- uberlist2[[3]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber04 <- uberlist2[[4]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber05 <- uberlist2[[5]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber06 <- uberlist2[[6]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber07 <- uberlist2[[7]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber08 <- uberlist2[[8]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber09 <- uberlist2[[9]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber10 <- uberlist2[[10]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber11 <- uberlist2[[11]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]
uber12 <- uberlist2[[12]][uberlist2[[1]]$TDAYDATE>=201604 & uberlist2[[1]]$TDAYDATE<=201704, ]

ubersupply13 <- function(x) {
  x$TDAYDATE <- as.integer(paste(substr(x$Month, 1, 4), substr(x$Month, 6, 7), sep=""))
  varnam <- colnames(x)
  #https://www.r-bloggers.com/basic-text-string-functions-in-r/
  #https://stackoverflow.com/questions/6638072/escaped-periods-in-r-regular-expressions
  for (i in 2:3) {
    a <- gregexpr(pattern ='\\.', varnam[i])[[1]]
    b <- a[1]
    c <- a[length(a)-4]
    colnames(x)[i] <- substr(colnames(x)[i], b+1, c-1)
  }
  x <- x[, c(4, 2:3)]
} 

uberlist3 <- list(uber13)
uberlist4 <- lapply(uberlist3, ubersupply13)
uber13 <- uberlist4[[1]][uberlist4[[1]]$TDAYDATE>=201604 & uberlist4[[1]]$TDAYDATE<=201704, ]
rm("uberlist1", "uberlist2", "uberlist3", "uberlist4")


gtrend <- function(x) {
  multiplier <- mean(as.numeric(uber01[, 2]))/mean(as.numeric(x[, 2]))
  temp1 <- x
  temp1[, 2] <- as.numeric(temp1[, 2])*multiplier 
  temp1[, 3] <- as.numeric(temp1[, 3])*multiplier 
  temp1[, 4] <- as.numeric(temp1[, 4])*multiplier 
  temp1[, 5] <- as.numeric(temp1[, 5])*multiplier 
  temp1[, 6] <- as.numeric(temp1[, 6])*multiplier 
  return(temp1[, c(1, 3:6)]) 
}

uber02 <- gtrend(uber02)
uber03 <- gtrend(uber03)
uber04 <- gtrend(uber04)
uber05 <- gtrend(uber05)
uber06 <- gtrend(uber06)
uber07 <- gtrend(uber07)
uber08 <- gtrend(uber08)
uber09 <- gtrend(uber09)
uber10 <- gtrend(uber10)
uber11 <- gtrend(uber11)
uber12 <- gtrend(uber12)

gtrend13 <- function(x) {
  multiplier <- mean(as.numeric(uber01[, 2]))/mean(as.numeric(x[, 2]))
  temp1 <- x
  temp1[, 2] <- as.numeric(temp1[, 2])*multiplier 
  temp1[, 3] <- as.numeric(temp1[, 3])*multiplier 
  return(temp1[, c(1, 3)]) 
}

uber13 <-gtrend13(uber13)

uber01[, 2] <- as.numeric(uber01[, 2])
uber01[, 3] <- as.numeric(uber01[, 3])
uber01[, 4] <- as.numeric(uber01[, 4])
uber01[, 5] <- as.numeric(uber01[, 5])
uber01[, 6] <- as.numeric(uber01[, 6])

temp1 <- uber01 
temp1 <- merge(temp1, uber02, by="TDAYDATE")
temp1 <- merge(temp1, uber03, by="TDAYDATE")
temp1 <- merge(temp1, uber04, by="TDAYDATE")
temp1 <- merge(temp1, uber05, by="TDAYDATE")
temp1 <- merge(temp1, uber06, by="TDAYDATE")
temp1 <- merge(temp1, uber07, by="TDAYDATE")
temp1 <- merge(temp1, uber08, by="TDAYDATE")
temp1 <- merge(temp1, uber09, by="TDAYDATE")
temp1 <- merge(temp1, uber10, by="TDAYDATE")
temp1 <- merge(temp1, uber11, by="TDAYDATE")
temp1 <- merge(temp1, uber12, by="TDAYDATE")
temp1 <- merge(temp1, uber13, by="TDAYDATE")
GTUBER <- temp1 

sapply(GTUBER, class)
rm("uber01", "uber02", "uber03", "uber04", "uber05", 
   "uber11", "uber12", "uber13", "temp1", "ualist")  

nhtsualist2 <- read.csv(file="M:/Uber_NHTS/11_Scratch/nhtsualist2.csv", header=TRUE, sep=",")

temp1 <- data.frame(TDAYDATE=integer(), GTscore=double(), GTNAME=character(), stringsAsFactors=FALSE) 
GTUBER2 <- data.frame(TDAYDATE=integer(), GTscore=double(), GTNAME=character(), stringsAsFactors=FALSE) 
for(i in 1:50){
  temp1 <- GTUBER[, c(1, i+1)]
  colnames(temp1)[2] <- "GTscore"
  temp1$GTscore <- as.numeric(as.character(temp1$GTscore))
  temp1$GTNAME <- colnames(GTUBER)[i+1]
  GTUBER2 <- rbind(GTUBER2, temp1)
}

GTUBER3 <- merge(GTUBER2, nhtsualist2[, c("UACE10", "GTNAME")], by="GTNAME")
GTUBER3 <- GTUBER3[, c(4, 2, 3)]
write.csv(GTUBER3, "M:/Uber_NHTS/09_Uber/GTUBER3.csv")


GTUBER4 <- read.csv(file="M:/Uber_NHTS/09_Uber/GTUBER3.csv", header=TRUE, sep=",")
GTUBER4$X <- NULL
sapply(GTUBER4, class)


data06 <- merge(data05, GTUBER4, by=c("UACE10", "TDAYDATE"))
data06$GTscore.std <- as.numeric(scale(data06$GTscore))
data06$GTscore <- NULL 


rm("nhtsualist", "nhtsualist2", "temp1", "GTUBER", "GTUBER2", "GTUBER3", "GTUBER4", "data04", "data05")
rm("uber06", "uber07", "uber08", "uber09", "uber10")




