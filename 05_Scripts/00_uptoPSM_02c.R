
# Step 0. Basic setup: libraries & working directory ----------------------

if (!require("foreign")) install.packages("foreign", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("plyr")) install.packages("plyr", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tidyverse")) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)

if (!require("sf")) install.packages("sf", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tidycensus")) install.packages("tidycensus", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tigris")) install.packages("tigris", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("devtools")) install.packages("devtools")
if (!require("lehdr")) devtools::install_github("jamgreen/lehdr")

if (!require("mapview")) install.packages("mapview", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("mapview")) install.packages("mapview")
if (!require("tmap")) install.packages("tmap", repos = "http://cran.us.r-project.org", dependencies = TRUE)

if (!require("psych")) install.packages("psych", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tableone")) install.packages("tableone", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("MatchIt")) install.packages("MatchIt", repos = "http://cran.us.r-project.org", dependencies = TRUE)


library(foreign)
library(plyr)
library(tidyverse)

library(sf)
library(tidycensus)
library(tigris)
library(lehdr)
library(tmap)

library(psych)
library(tableone)
library(MatchIt)

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
    path <- paste0(filepath, "/05_Census/tigris")
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if (!file.exists(renv)) {
      file.create(renv)
    }
    check <- readLines(renv)
    if (isTRUE(any(grepl("TIGRIS_CACHE_DIR", check)))) {
      oldenv <- read.table(renv, stringsAsFactors = FALSE)
      newenv <- oldenv[-grep("TIGRIS_CACHE_DIR", oldenv), 
                       ]
      write.table(newenv, renv, quote = FALSE, sep = "\n", 
                  col.names = FALSE, row.names = FALSE)
    }
    var <- paste0("TIGRIS_CACHE_DIR=", "'", path, "'")
    write(var, renv, sep = "\n", append = TRUE)
    message(sprintf("Your new tigris cache directory is %s. \nTo use now, restart R or run `readRenviron('~/.Renviron')`", 
                    path))

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


## Task 1-2. Read in and select variables of NHTS restricted-use data files ----

hh <- read_csv(paste0(filepath, "/03_NHTS/Csv/hhpub.csv"))
# colnames(hh)
# boxplot(HHVEHCNT ~ LIF_CYC, data=hh)
hh$NUMCHILD <- ifelse(hh$HHSIZE>=hh$NUMADLT, hh$HHSIZE-hh$NUMADLT, 0)
# summary(hh$YOUNGCHILD)
# summary(hh$NUMCHILD)
per <- read_csv(paste0(filepath, "/03_NHTS/Csv/perpub.csv"))

# the below files are not accessible from my laptop 
home <- read_csv('F:/3 Study/0 medium term backup/2017_NHTS_US/GEO/hhctbg.csv') # restricted-use home block group file 
work <- read_csv('F:/3 Study/0 medium term backup/2017_NHTS_US/GEO/workct.csv') # restricted-use work/school tract file

# Merge datasets  
hh2  <- hh[, c("HOUSEID", "TDAYDATE", "HH_RACE",  "HH_HISP", "LIF_CYC", "HHSIZE", "NUMADLT", "NUMCHILD",
               "YOUNGCHILD", "WRKCOUNT", "DRVRCNT", "HHFAMINC","HOMEOWN","HHVEHCNT",  
               "PC", "SPHONE", "TAB", "WEBUSE17", "TAXI", "SCRESP", "WTHHFIN")] #SCRESP = PERSONID
per2 <- per[, c("HOUSEID", "PERSONID", "R_RELAT", "R_SEX", "R_AGE", "R_HISP", "R_RACE",  
                "DRIVER", "EDUC", "DISTTOWK17", "NOCONG", "SCHTYP", "SCHTRN1", "WORKER", "WKFTPT", "WRKTRANS",
                "FLEXTIME", "GT1JBLWK", "OCCAT", "WKRMHM", "WKFMHMXX", "MEDCOND", 
                "NBIKETRP", "BIKE4EX", "NWALKTRP", "WALK4EX", "PTUSED", "RIDESHARE", "DELIVER", "YEARMILE", 
                "WTPERFIN")] 

hh3  <- left_join(hh2, home, by="HOUSEID") # home blockgroup no missing 
per3 <- left_join(per2, work[, c("HOUSEID", "PERSONID", "WKSTFIPS", "WKCNFIPS", "WORKCT")], 
              by=c("HOUSEID", "PERSONID"))
# per3 %>% filter(is.na(WORKCT) == TRUE) %>% nrow()
# per3 %>% filter(WORKCT == -1) %>% nrow() # 49.25189% individuals with missing WORKCT 
hhp <- left_join(hh3, per3, by=c("HOUSEID"))
hhpbg <- left_join(hhp, bgd, by=c("HHSTFIPS","HHCNTYFP","HHCT","HHBG")) %>%
  filter(is.na(UACE10) == FALSE) %>%
  filter(nchar(WORKCT) == 6)
# 37118/264234 0.140474 commuters in the top 50 UAs. 

hhpbg %>% group_by(HHSTFIPS, HHCNTYFP, HHCT, HHBG) %>% summarize() %>% nrow() # 16,916 unique block groups
hhpbg %>% select(tr) %>% n_distinct() # 11,424 unique tracts

# rm("hh", "per", "home", "work", "hh2", "per2", "hh3", "per3", "hhp")

# check annual miles driven & driver status 
# table(hhpbg[hhpbg$YEARMILE<0, ]$YEARMILE)   #-88 I don't know -77 I prefer not to answer -9 not ascertained -1 appropriate skip
# table(hhpbg[hhpbg$YEARMILE==-1, ]$DRIVER)   #-1 appropriate skip 2 non-driver
# table(hhpbg[hhpbg$DRIVER==-1, ]$R_AGE)      # all cases under 15



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

# a <- map_dbl(tract_be, ~sum(is.na(.))) 
# a[a>0] # no missing 

# a <- map_dbl(tract_be2, ~sum(is.na(.))) 
# a[a>0] # some missing b/c either no jobs or no workers

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

# a <- map_dbl(tract_be3, ~sum(is.na(.))) 
# a[a>0] # no missing 



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

# a <- map_dbl(tract_be4, ~sum(is.na(.))) 
# a[a>0] # 87 tracts missing: maybe, no population in 2010 US Census? thus, all tracts deleted when weighted by population?  



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
}

# first-round api call 
# rm(tract_acs2012_raw)
start_time <- Sys.time()
tract_acs2012_raw <- map(1:37, get_acs_tract2) # took about ?? minutes
end_time <- Sys.time()
end_time - start_time

# second-round api call 
tract_acs2012_raw_okay <- c(1:37)[map_lgl(tract_acs2012_raw, is.list)]        
tract_acs2012_raw_fail <- c(1:37)[map_lgl(tract_acs2017_raw, ~!is.list(.))]  
map(tract_acs2012_raw_fail, function(x) tract_acs2012_raw[[x]] <<- get_acs_tract2(x))




st_tr_tb <- 
  trd %>%
  rename(GEOID = tr) %>%
  mutate(st = substr(GEOID, 1, 2)) %>%
  arrange(st, GEOID) %>%
  select(st, UACE10, GEOID) 

iv_all <- NULL 





for (i in 1:37) {
  iv_st <- NULL
  iv_st <- st_tr_tb %>% filter(st == list_st_fips[[1]][i])
  for (j in 1:7) {
    temp00 <- NULL
    temp00 <- get_acs(
      geography = "tract", 
      state = list_st_fips[[1]][i], 
      variables = varlist2[j], 
      year=2012, # 2012 have many missing variables
      cache_table = TRUE
      )
    #Sys.sleep(0.75)
    temp01 <- temp00[, c("GEOID", "estimate")]
    colnames(temp01) <- c("GEOID", paste0(varlist2[j], "est"))  
    iv_st <- iv_st %>% left_join(temp01, by="GEOID")
  }
  iv_all <- rbind(iv_all, iv_st)
}

iv_all
# str(iv_all)

iv_all$pctcoll  <- ifelse(iv_all$B06009_001est>0, (iv_all$B06009_005est + iv_all$B06009_006est)/iv_all$B06009_001est *100, 0)
iv_all$pctyoung <- ifelse(iv_all$B06001_001est>0,  iv_all$B06001_005est/iv_all$B06001_001est*100, 0) 
iv_all$pctxveh  <- ifelse(iv_all$B08014_001est>0,  iv_all$B08014_002est/iv_all$B08014_001est *100, 0)
iv_all <- iv_all[, c("GEOID", "UACE10", "pctcoll", "pctyoung", "pctxveh")]  
head(iv_all)

iv_job <- NULL 
for(i in 1:37) {
  temp <- grab_lodes(state=ST[i], year=2011, lodes_type="wac", agg_geo="tract")
  temp <- temp[, c("w_tract", "CNS09", "CNS12", "CNS17", "CNS18")]  
  temp <- as.data.frame(temp)
  iv_job <- rbind(iv_job, temp)
}
colnames(ivjob)[1] <- "GEOID"
ivjob2 <- merge(trd, ivjob, by="GEOID")
ivjob2$UACE00<- NULL
ivjob2$techjob <- ivjob2$CNS09 + ivjob2$CNS12 
ivjob2$servjob <- ivjob2$CNS17 + ivjob2$CNS18 
ivjob3 <- ivjob2[, c("GEOID", "techjob", "servjob")]
ivall2 <- merge(ivall, ivjob3, by="GEOID")

ivall3 <- merge(ivall2, ALAND, by="GEOID")
ivall3$ALAND <- as.numeric(ivall3$ALAND)
ivall3$techden <- ifelse(ivall3$ALAND>0, ivall3$techjob*1000000/ivall3$ALAND, 0)
ivall3$servden <- ifelse(ivall3$ALAND>0, ivall3$servjob*1000000/ivall3$ALAND, 0)
ivall3$ALAND <- NULL
ivall3$techjob <- NULL
ivall3$servjob <- NULL

temp <- ivall3
temp <- ddply(temp, c("UACE10"), transform, pctcoll.std=scale(pctcoll))
temp <- ddply(temp, c("UACE10"), transform, pctyoung.std=scale(pctyoung))
temp <- ddply(temp, c("UACE10"), transform, pctxveh.std=scale(pctxveh))
temp <- ddply(temp, c("UACE10"), transform, techden.std=scale(techden))
temp <- ddply(temp, c("UACE10"), transform, servden.std=scale(servden))
ivall4 <- temp[, c(1:2,8:12)]

rm("iv_st", "ALAND", "ivall", "ivall2", "ivall3", "ivjob", "ivjob2", "ivjob3")
rm("temp", "temp2")

write.csv(ivall4, "M:/Uber_NHTS/11_Scratch/ivall4.csv")


iv_all4 <- read.csv("M:/Uber_NHTS/11_Scratch/ivall4.csv")
iv_all4[, 1] <- NULL
iv_all4$GEOID <- as.character(iv_all4$GEOID)
iv_all4$GEOID <- ifelse(nchar(iv_all4$GEOID)==10, paste("0", iv_all4$GEOID, sep=""), iv_all4$GEOID)



## Task 2-7. Scrape walkscore.com: using a different file of scripts ----

trxy <- NULL 
for (i in 1:37) {
  temp1   <- tracts(ST[i], year=2016)
  temp2   <- temp1@data[, c("GEOID", "INTPTLAT", "INTPTLON")]
  temp3   <- merge(trd, temp2, by="GEOID")
  trxy    <- rbind(trxy, temp3)
}
colnames(trxy) <- c("GEOID", "UACE10", "y", "x")
trxy$x <- as.numeric(trxy$x)
trxy$y <- as.numeric(trxy$y)
rm("temp1", "temp2", "temp3")
write.csv(trxy, "M:/Uber_NHTS/11_Scratch/trxy.csv")


trxy <- read.csv("M:/Uber_NHTS/11_Scratch/trxy.csv")

#See another script that scrapes walkscores at the census tract level: To be updated



# Step 3. Prepare HH/PERSON/individual-level IV variables -------------------------------------

## Task 3-1. Outcome variables: data01 ----

# n of vehicles, transit trips per week, non-motorized trips per week 
# treatment indicator 

quantile(hhpbg[hhpbg$PTUSED>0, ]$PTUSED, c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99)) 
# in the past 30 days, how many days you used public transportation? 
# 0-3, 4-7, 8-11, 12+ (4 levels)
nrow(hhpbg[hhpbg$PTUSED==0, ])/nrow(hhpbg[hhpbg$PTUSED>=0, ])*100
nrow(hhpbg[hhpbg$PTUSED>=1 & hhpbg$PTUSED<=3, ])/nrow(hhpbg[hhpbg$PTUSED>=0, ])*100
nrow(hhpbg[hhpbg$PTUSED>=4 & hhpbg$PTUSED<=11, ])/nrow(hhpbg[hhpbg$PTUSED>=0, ])*100
nrow(hhpbg[hhpbg$PTUSED>=12, ])/nrow(hhpbg[hhpbg$PTUSED>=0, ])*100

hhpbg$PTUSED2 <- ifelse(hhpbg$PTUSED>=0,  0, NA) # no use
hhpbg$PTUSED2 <- ifelse(hhpbg$PTUSED>=1,  1, hhpbg$PTUSED2) # 1-3 times (<1 times/week)
hhpbg$PTUSED2 <- ifelse(hhpbg$PTUSED>=4,  2, hhpbg$PTUSED2) # 4-11 times (1-2 times/week)
hhpbg$PTUSED2 <- ifelse(hhpbg$PTUSED>=12, 3, hhpbg$PTUSED2) # 12 or more times (3 or more times/week)
table(hhpbg$PTUSED2)
hhpbg$PTUSED2 <- as.factor(hhpbg$PTUSED2)

hhpbg$LNPTUSED <- ifelse(hhpbg$PTUSED>=0, log(hhpbg$PTUSED+1), NA)

hhpbg$NBIKEMODE <- NA
hhpbg$NBIKEMODE <- ifelse(hhpbg$NBIKETRP>=0 & hhpbg$BIKE4EX>=0 & hhpbg$NBIKETRP>=hhpbg$BIKE4EX, hhpbg$NBIKETRP-hhpbg$BIKE4EX, NA)
hhpbg$NBIKEMODE <- ifelse(hhpbg$NBIKETRP>=0 & hhpbg$BIKE4EX==-1, hhpbg$NBIKETRP, hhpbg$NBIKEMODE)
summary(hhpbg$NBIKEMODE)
table(hhpbg$NBIKEMODE)
# in the past 7 days, how many times did you ride a bicycle? 
# 0-3, 4-7, 8-11, 12+ (4 levels)

hhpbg$NWALKMODE <- NA
hhpbg$NWALKMODE <- ifelse(hhpbg$NWALKTRP>=0 & hhpbg$WALK4EX>=0 & hhpbg$NWALKTRP>=hhpbg$WALK4EX, hhpbg$NWALKTRP-hhpbg$WALK4EX, NA)
hhpbg$NWALKMODE <- ifelse(hhpbg$NWALKTRP>=0 & hhpbg$WALK4EX==-1, hhpbg$NWALKTRP, hhpbg$NWALKMODE)
summary(hhpbg$NWALKMODE)
table(hhpbg$NWALKMODE)
# In the past 7 days, how many times did [$YOU1] take a walk outside including walks 
# to exercise, go somewhere, or to walk the dog (e.g., walk to a friend’s house, 
# walk around the neighborhood, walk to the store, etc.)?

hhpbg$LNBIKEMODE <- ifelse(hhpbg$NBIKEMODE>=0, log(hhpbg$NBIKEMODE+1), NA)
hhpbg$LNWALKMODE <- ifelse(hhpbg$NWALKMODE>=0, log(hhpbg$NWALKMODE+1), NA)

hhpbg$NWBMODE <- hhpbg$NBIKEMODE + hhpbg$NWALKMODE
hhpbg$LNWBMODE <- ifelse(hhpbg$NWBMODE>=0, log(hhpbg$NWBMODE+1), NA)
summary(hhpbg$NWBMODE)
table(hhpbg$NWBMODE)

quantile(hhpbg[hhpbg$NWBMODE>=0, ]$NWBMODE, c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99), na.rm=TRUE) 
summary(hhpbg[hhpbg$NWBMODE>=0, ]$NWBMODE, c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99), na.rm=TRUE) 

nrow(hhpbg[hhpbg$NWBMODE==0, ])/nrow(hhpbg[hhpbg$NWBMODE>=0, ])*100
nrow(hhpbg[hhpbg$NWBMODE>=1 & hhpbg$NWBMODE<=4, ])/nrow(hhpbg[hhpbg$NWBMODE>=0, ])*100
nrow(hhpbg[hhpbg$NWBMODE>=5 & hhpbg$NWBMODE<=8, ])/nrow(hhpbg[hhpbg$NWBMODE>=0, ])*100
nrow(hhpbg[hhpbg$NWBMODE>=9, ])/nrow(hhpbg[hhpbg$NWBMODE>=0, ])*100

hhpbg$NWBMODE2 <- ifelse(hhpbg$NWBMODE>=0, 0, NA) # no use
hhpbg$NWBMODE2 <- ifelse(hhpbg$NWBMODE>=1, 1, hhpbg$NWBMODE2) # 1-2 round trips per week  
hhpbg$NWBMODE2 <- ifelse(hhpbg$NWBMODE>=5, 2, hhpbg$NWBMODE2) # 3-4 round trips per week  
hhpbg$NWBMODE2 <- ifelse(hhpbg$NWBMODE>=9, 3, hhpbg$NWBMODE2) # 5 or more round trips per week 
table(hhpbg$NWBMODE2)
hhpbg$NWBMODE2 <- as.factor(hhpbg$NWBMODE2)

data01 <- hhpbg[, c("HOUSEID", "PERSONID", "RIDESHARE", "HHVEHCNT", #"YEARMILE", # too many missing
                    "PTUSED2","NWBMODE2")]

data01$HHVEHCNT2 <- ifelse(data01$HHVEHCNT>2, 3, data01$HHVEHCNT)
data01$HHVEHCNT2 <- as.factor(data01$HHVEHCNT2)
#data01$LNVMD <- ifelse(temp$YEARMILE>=0, log(temp$YEARMILE+1), NA)
#data01$YEARMILE <- NULL
data01 <- data01[, c(1:4, 7, 5:6)] 
colnames(data01)
sapply(data01, class)



## Task 3-2. HH variables ---- 

# n of child, employees, and bicycles, income, homeownership, residence type, 
# residential/work density + accessibility
# Uber supply measure 

## cor(hhpbg[, c("NUMADLT", "WRKCOUNT")], use="complete.obs", method="kendall")
## cor(hhpbg[, c("NUMADLT", "DRVRCNT")], use="complete.obs", method="kendall")
## cor(hhpbg[, c("WRKCOUNT", "DRVRCNT")], use="complete.obs", method="kendall")

data02 <- hhpbg[, c("HOUSEID", "PERSONID", "TDAYDATE", "LIF_CYC", "NUMCHILD", "YOUNGCHILD", "WRKCOUNT", "DRVRCNT", 
                    "HHFAMINC", "HOMEOWN", "HHSTFIPS", "HHCNTYFP", "HHCT", "UACE10", 
                    "WKSTFIPS", "WKCNFIPS", "WORKCT")]

data02$LIF_CYC <- as.factor(data02$LIF_CYC)
data02$HHFAMINC <- ifelse(data02$HHFAMINC<0, NA, data02$HHFAMINC)
data02$HHFAMINC <- as.factor(data02$HHFAMINC)
summary(data02$HHFAMINC)
data02$HOMEOWN <- ifelse(data02$HOMEOWN<0, NA, data02$HOMEOWN)
data02$HOMEOWN <- ifelse(data02$HOMEOWN>1, 0, data02$HOMEOWN)
data02$HOMEOWN <- as.factor(data02$HOMEOWN) # 1-own, 0-rent or other arrangements 
summary(data02$HOMEOWN)


data02$GEOID <- data02$HHSTFIPS*1000000000+data02$HHCNTYFP*1000000+data02$HHCT
data02$GEOID <- ifelse(data02$GEOID<10000000000, paste("0", data02$GEOID, sep=""), as.character(data02$GEOID))
data03 <- merge(data02, beall7[, c("GEOID", "den")], by="GEOID") # lost 6 cases probably b/c home CT without ACS estimates 
a <- length(colnames(data03))
colnames(data03)[a] <- "homeden"
data03 <- merge(data03, ivall4[, c(1, 3:5)], by="GEOID")
data03$GEOID <- NULL


data03$GEOID <- ifelse(data03$WKSTFIPS>0, data03$WKSTFIPS*1000000000 + data03$WKCNFIPS*1000000 + data03$WORKCT, NA)
data03$GEOID <- ifelse(data03$GEOID<10000000000, paste("0", data03$GEOID, sep=""), as.character(data03$GEOID))
data03 <- merge(data03, ivall4[, c(1, 6:7)], by="GEOID")
data04 <- merge(data03, beall7[, c("GEOID", "den")], by="GEOID")
data04$WKSTFIPS <- NULL
data04$WKCNFIPS <- NULL
data04$WORKCT <- NULL
data04$GEOID <- NULL
a <- length(colnames(data04))
colnames(data04)[a] <- "workden"

rm("data02", "data03", "beall7", "ivall4")
 


# How to make ML1, ML1 in data04 change to homeden, workden?  
#https://stackoverflow.com/questions/27347548/r-assign-variable-labels-of-data-frame-columns
#install.packages("Hmisc")
#library(Hmisc)



## Task 3-3. Uber suuply variable: Google Trends # of Searches ----

ua2016 <- urban_areas(cb=FALSE, year=2016)
ualist <- ua2016@data
ualist <- ualist[, c("UACE10", "NAME10")]
ualist$UACE10 <- as.integer(ualist$UACE10)
rm("ua2016")

data05 <- merge(data04, ualist, by="UACE10")
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



## Task 3-4. Person variables, "non-workers removed" ----

# Age, commute distance, disability, educational attainment, gender, driver's license, occupation (1 among 3)
# n of commute days per week 

data08 <- hhpbg[, c("HOUSEID", "PERSONID", "R_SEX", "R_AGE", "R_RACE", "R_HISP", "DRIVER", "EDUC", 
                    "WORKER", "WKFTPT", "WRKTRANS", "FLEXTIME", "GT1JBLWK", "DISTTOWK17", "OCCAT", 
                    "WKRMHM", "WKFMHMXX", "MEDCOND", "PC", "SPHONE", "TAB", "WEBUSE17", "DELIVER")]

data08 <- data08[data08$WORKER==1, ] 
#data08 <- data08[data08$WORKER== 1 & data08$PERSONID==1, ] # restrict to the recruitment survey respondents 
data08$R_SEX <- ifelse(data08$R_SEX<0, NA, data08$R_SEX)
table(data08$R_SEX)
data08$R_SEX <- data08$R_SEX - 1
# -8=I don't know
# -7=I prefer not to answer
# 01=Male
# 02=Female
# After processing: 0-male, 1-female

data08$R_AGE <- ifelse(data08$R_AGE<0, NA, data08$R_AGE)
data08$R_RACE <- ifelse(data08$R_RACE<0, NA, data08$R_RACE)
data08$R_RACE <- as.factor(data08$R_RACE)
data08$R_HISP <- ifelse(data08$R_HISP<0, NA, data08$R_HISP)
data08$R_HISP <- ifelse(data08$R_HISP==2, 0, data08$R_HISP)
data08$R_HISP <- as.factor(data08$R_HISP)
data08$DRIVER <- ifelse(data08$DRIVER==2, 0, data08$DRIVER)

data08$EDUC <- ifelse(data08$EDUC<0, NA, data08$EDUC)
data08$EDUC <- as.factor(data08$EDUC)

quantile(data08[data08$DISTTOWK17>=0,]$DISTTOWK17, c(.95, .975, .99))
1- nrow(data08[data08$DISTTOWK17>=0,])/nrow(data08)
#network commute distance missing 18% of the worker subsample 

data08$OCCAT <- ifelse(data08$OCCAT<0, NA,data08$OCCAT)
data08$OCCAT <- as.factor(data08$OCCAT)
table(data08$OCCAT)
#-9=Not ascertained
#-8=I don't know
#-7=I prefer not to answer
#-1=Appropriate skip
#01=Sales or service
#02=Clerical or administrative support
#03=Manufacturing, construction, maintenance, or farming
#04=Professional, managerial, or technical
#97=Something else

table(data08$WKFTPT)
table(data08$FLEXTIME)
table(data08$GT1JBLWK)

table(data08[data08$WKRMHM==1, ]$WKFMHMXX)
#Count of Days Worked From Home in Last Month
#-8=I don't know
#-7=I prefer not to answer
#-1=Appropriate skip

table(data08$WKRMHM)
#Option of Working from Home
#-9=Not ascertained
#-8=I don't know
#-7=I prefer not to answer
#-1=Appropriate skip
#01=Yes
#02=No

quantile(data08[data08$WKRMHM==1, ]$WKFMHMXX, c(.25, .50, .75, .90, .95, .99))
quantile(data08[data08$WKRMHM==1&data08$WKFMHMXX>0, ]$WKFMHMXX, c(.25, .50, .75, .90, .95, .99))

data08$Telecommute <- ifelse(data08$WKFMHMXX>=1, 1, 0)
data08$Telecommute <- ifelse(data08$WKFMHMXX>=4, 2, data08$Telecommute)
data08$Telecommute <- ifelse(data08$WKFMHMXX>=8, 3, data08$Telecommute)
data08$Telecommute <- ifelse(data08$WKFMHMXX>=12, 4, data08$Telecommute)
data08$Telecommute <- as.factor(data08$Telecommute)


# Frequency of ICT device use  
# sapply(data08[, c(18:22)], table)

table(data08$MEDCOND)
#-8=I don't know
#-7=I prefer not to answer
data08$medcon <- ifelse(data08$MEDCOND==1, 1, 0)

data08$DELIVER <- ifelse(data08$DELIVER<0, NA, data08$DELIVER)
#data08$DELIVER <- as.factor(data08$DELIVER)
#-9=Not ascertained
#-8=I don't know
#-7=I prefer not to answer
#01 = Daily
#02 = A few times a week
#03 = A few times a month
#04 = A few times a year
#05 = Never


data09 <- data08[, c("HOUSEID", "PERSONID", "R_SEX", "R_AGE", "R_RACE", "R_HISP", 
                     "DRIVER", "EDUC", "WORKER", "OCCAT", "Telecommute","medcon",  "DELIVER")]
rm("data08")



## Task 3-5. Merge three dataframes ----

colnames(data01)
colnames(data06)
colnames(data09)

temp1 <- merge(data01, data06, by=c("HOUSEID", "PERSONID"))
data10 <- merge(temp1, data09, by=c("HOUSEID", "PERSONID"))
rm("temp1")
nrow(data10)




# Step 4. Estimate binary logit/probit for balancing the final sample -----

## Task 4-1. PSM estimation ----

#4.1. Compute a new categorical variable, RS 
# 0 - no use in the last 30 days 
# 1 - less than once a week in the last 30 days 
# 2 - less than twice a week in the last 30 days 
# 3 - at least twice a week in the last 30 days 

table(data10$RIDESHARE)
#hist(data10[data10$RIDESHARE>0, ]$RIDESHARE)
quantile(data10[data10$RIDESHARE>0, ]$RIDESHARE, c(.9, .95, .975, .99, .999))

data10$RS <- ifelse(
  data10$RIDESHARE<0, NA, ifelse(
    data10$RIDESHARE==0, 0, ifelse( # non-user
      data10$RIDESHARE<4, 1, ifelse( # less than once a week 
        data10$RIDESHARE<8, 2, 3 # less than twice a week vs. at least twice a week 
      )
    )
  )
)
data10$RS <- as.integer(as.character(data10$RS))
class(data10$RS)

summary(data10$RIDESHARE)
table(data10[data10$RIDESHARE>=0, ]$RIDESHARE)

a <- as.data.frame(table(data10$RS)) 
b <- nrow(data10[data10$RS >=0, ])
a$pct <- a[,2]/b*100
sum(a$Freq)
a



## Task 4-2. PSM estimation ----

## (1- nrow(data10[is.na(data10$RS)==FALSE, ])/nrow(data10))*100 
## table(data10$RS)

data11 <- data10[(data10$RS==0 | data10$RS==1) & (is.na(data10$RS)==FALSE), ] # choose *two* user types 
table(data11$RS)

(1- nrow(data11)/nrow(data10[is.na(data10$RS)==FALSE, ]))*100  
data11$RS <- ifelse(data11$RS>0, 1, data11$RS)
data11$RS <- as.integer(data11$RS)


data11$LIF_CYC01 <- ifelse(data11$LIF_CYC==1, 1, 0)
data11$LIF_CYC02 <- ifelse(data11$LIF_CYC==2, 1, 0)
data11$LIF_CYC03 <- ifelse(data11$LIF_CYC==3, 1, 0)
data11$LIF_CYC04 <- ifelse(data11$LIF_CYC==4, 1, 0)
data11$LIF_CYC05 <- ifelse(data11$LIF_CYC==5, 1, 0)
data11$LIF_CYC06 <- ifelse(data11$LIF_CYC==6, 1, 0)
data11$LIF_CYC07 <- ifelse(data11$LIF_CYC==7, 1, 0)
data11$LIF_CYC08 <- ifelse(data11$LIF_CYC==8, 1, 0)
data11$LIF_CYC09 <- ifelse(data11$LIF_CYC==9, 1, 0)
data11$LIF_CYC10 <- ifelse(data11$LIF_CYC==10, 1, 0)


data11$HHFAMINC01 <- ifelse(data11$HHFAMINC==1, 1, 0)
data11$HHFAMINC01 <- ifelse(is.na(data11$HHFAMINC)==TRUE, NA, data11$HHFAMINC01)
data11$HHFAMINC02 <- ifelse(data11$HHFAMINC==2, 1, 0)
data11$HHFAMINC03 <- ifelse(data11$HHFAMINC==3, 1, 0)
data11$HHFAMINC04 <- ifelse(data11$HHFAMINC==4, 1, 0)
data11$HHFAMINC05 <- ifelse(data11$HHFAMINC==5, 1, 0)
data11$HHFAMINC06 <- ifelse(data11$HHFAMINC==6, 1, 0)
data11$HHFAMINC07 <- ifelse(data11$HHFAMINC==7, 1, 0)
data11$HHFAMINC08 <- ifelse(data11$HHFAMINC==8, 1, 0)
data11$HHFAMINC09 <- ifelse(data11$HHFAMINC==9, 1, 0)
data11$HHFAMINC10 <- ifelse(data11$HHFAMINC==10, 1, 0)
data11$HHFAMINC11 <- ifelse(data11$HHFAMINC==11, 1, 0)


data11$R_RACE01 <- ifelse(data11$R_RACE==1, 1, 0)
data11$R_RACE01 <- ifelse(is.na(data11$R_RACE)==TRUE, NA, data11$R_RACE01)
data11$R_RACE02 <- ifelse(data11$R_RACE==2, 1, 0)
data11$R_RACE03 <- ifelse(data11$R_RACE==3, 1, 0)
data11$R_RACE04 <- ifelse(data11$R_RACE==4, 1, 0)
data11$R_RACE05 <- ifelse(data11$R_RACE==5, 1, 0)
data11$R_RACE06 <- ifelse(data11$R_RACE==6, 1, 0)
data11$R_RACE97 <- ifelse(data11$R_RACE==97, 1, 0)


data11$EDUC01 <- ifelse(data11$EDUC==1, 1, 0)
data11$EDUC01 <- ifelse(is.na(data11$EDUC)==TRUE, NA, data11$EDUC01)
data11$EDUC02 <- ifelse(data11$EDUC==2, 1, 0)
data11$EDUC03 <- ifelse(data11$EDUC==3, 1, 0)
data11$EDUC04 <- ifelse(data11$EDUC==4, 1, 0)
data11$EDUC05 <- ifelse(data11$EDUC==5, 1, 0)


data11$OCCAT01 <- ifelse(data11$OCCAT==1, 1, 0)
data11$OCCAT01 <- ifelse(is.na(data11$OCCAT)==TRUE, NA, data11$OCCAT01)
data11$OCCAT02 <- ifelse(data11$OCCAT==2, 1, 0)
data11$OCCAT03 <- ifelse(data11$OCCAT==3, 1, 0)
data11$OCCAT04 <- ifelse(data11$OCCAT==4, 1, 0)
data11$OCCAT97 <- ifelse(data11$OCCAT==97, 1, 0)


data11$Telecommute00 <- ifelse(data11$Telecommute==0, 1, 0)
data11$Telecommute01 <- ifelse(data11$Telecommute==1, 1, 0)
data11$Telecommute02 <- ifelse(data11$Telecommute==2, 1, 0)
data11$Telecommute03 <- ifelse(data11$Telecommute==3, 1, 0)
data11$Telecommute04 <- ifelse(data11$Telecommute==4, 1, 0)

colnames(data11)
nrow(data11)

#data11 <- data11[data11$UACE10==63217, ] #NY; 3817-Atlanta 
#data11$RS <- as.factor(data11$RS)
usedvars <- c("HOUSEID", "PERSONID", "RIDESHARE", "HHVEHCNT", "HHVEHCNT2", "PTUSED2", "NWBMODE2", 
              "RS", "LIF_CYC01", "LIF_CYC02", "LIF_CYC03", "LIF_CYC04", "LIF_CYC05", "LIF_CYC06", 
              "LIF_CYC07", "LIF_CYC08", "LIF_CYC09", "LIF_CYC10", "WRKCOUNT", "DRVRCNT", "NUMCHILD", "YOUNGCHILD", 
              "HHFAMINC01", "HHFAMINC02", "HHFAMINC03", "HHFAMINC04", "HHFAMINC05", "HHFAMINC06", 
              "HHFAMINC07", "HHFAMINC08", "HHFAMINC09", "HHFAMINC10", "HHFAMINC11", "HOMEOWN", 
              "homeden", "workden", "pctcoll.std", "pctyoung.std", "pctxveh.std", "techden.std", "servden.std", 
              "R_SEX", "R_AGE", "R_RACE01", "R_RACE02", "R_RACE03", "R_RACE04", "R_RACE05", "R_RACE06", "R_RACE97", 
              "R_HISP", "DRIVER", "EDUC01", "EDUC02", "EDUC03", "EDUC04", "EDUC05", 
              "OCCAT01", "OCCAT02", "OCCAT03", "OCCAT04", "OCCAT97", 
              "Telecommute00", "Telecommute01", "Telecommute02", "Telecommute03", "Telecommute04", 
              "medcon", "DELIVER", "GTscore.std", "UACE10")
data12 <- data11[, usedvars]
data12 <- data12[complete.cases(data12), ]

sapply(data12, class)
nrow(data12)
sum(is.na(data12))

summary(data12$HHVEHCNT2)
table(data12$RS)
summary(data12$PTUSED2)
summary(data12$NWBMODE2)

head(data12$UACE10)
head(nhtsualist)

temp <- merge(data12, nhtsualist, by.x=c("UACE10"), by.y=c("UACE10"))

nhtsualist <- nhtsualist[order(nhtsualist$UACE10), ]

for (i in 1:50){
  new <- ifelse(temp$UACE10==nhtsualist$UACE10[i], 1, 0)
  ## https://stackoverflow.com/questions/45439918/create-names-for-new-columns-within-a-loop
  if (i<10) {
    temp[paste0("UA0", i)] <- new  
  } else {
    temp[paste0("UA", i)] <- new  
  }
}

data12 <- temp

a <- ddply(data12, .(UACE10), summarize, 
           Raw=mean(cases), 
           User=sum(is.na(HOUSEID)==FALSE & RS==1), 
           NonUser=sum(is.na(HOUSEID)==FALSE & RS==0))
a$pctUser <- round(a$User/a$Raw*100, 1)
a$pctNonUser <- round(a$NonUser/a$Raw*100, 1)
a <- merge(a, nhtsualist[, c("UACE10", "NAME10")], by="UACE10")
a[order(-a$User, -a$pctUser), ]



## https://lists.gking.harvard.edu/pipermail/matchit/2017-June/000728.html
## https://www.kdnuggets.com/2018/01/propensity-score-matching-r.html 

## install.packages("stargazer")
## library(stargazer)

psm <- glm(RS ~ LIF_CYC02 + LIF_CYC03 + LIF_CYC04 + LIF_CYC05 + LIF_CYC06 + LIF_CYC07 + LIF_CYC08 + LIF_CYC09 + 
             LIF_CYC10 + WRKCOUNT + DRVRCNT + NUMCHILD + YOUNGCHILD + HHFAMINC02 + HHFAMINC03 + HHFAMINC04 +
             HHFAMINC05 + HHFAMINC06 + HHFAMINC07 + HHFAMINC08 + HHFAMINC09 + HHFAMINC10 + HHFAMINC11 + 
             HOMEOWN + homeden + workden + pctcoll.std + pctyoung.std + pctxveh.std + techden.std + servden.std + 
             R_SEX + R_AGE + R_RACE02 + R_RACE03 + R_RACE04 + 
             R_RACE06 + R_RACE97 + R_HISP + DRIVER + EDUC02 + EDUC03 + EDUC04 + EDUC05 + 
             OCCAT02 + OCCAT03 + OCCAT04 + Telecommute01 + Telecommute02 + Telecommute03 +
             Telecommute04 + medcon + DELIVER + GTscore.std, 
           family=binomial(link="probit"), control = list(maxit = 100), data=data12)
summary(psm)
## stargazer(psm, type="text")


xvars <- c("RIDESHARE",     "HHVEHCNT",      "HHVEHCNT2",     "PTUSED2",       "NWBMODE2",      "RS",      
           "LIF_CYC01",     "LIF_CYC02",     "LIF_CYC03",     "LIF_CYC04",     "LIF_CYC05",     
           "LIF_CYC06",     "LIF_CYC07",     "LIF_CYC08",     "LIF_CYC09",     "LIF_CYC10",    
           "WRKCOUNT",      "DRVRCNT",       "NUMCHILD",      "YOUNGCHILD",    "HHFAMINC01",    "HHFAMINC02",    
           "HHFAMINC03",    "HHFAMINC04",    "HHFAMINC05",    "HHFAMINC06",    "HHFAMINC07",    "HHFAMINC08",    
           "HHFAMINC09",    "HHFAMINC10",    "HHFAMINC11",    "HOMEOWN",       "homeden",       "workden",       
           "pctcoll.std",   "pctyoung.std",  "pctxveh.std",   "techden.std",   "servden.std",   
           "R_SEX",         "R_AGE",         "R_RACE01",      "R_RACE02",      "R_RACE03",      "R_RACE04",      
           "R_RACE05",      "R_RACE06",      "R_RACE97",      "R_HISP",        "DRIVER",        
           "EDUC01",        "EDUC02",        "EDUC03",        "EDUC04",        "EDUC05",        
           "OCCAT01",       "OCCAT02",       "OCCAT03",       "OCCAT04",       "OCCAT97",       
           "Telecommute00", "Telecommute01", "Telecommute02", "Telecommute03", "Telecommute04",
           "medcon",        "DELIVER",       "GTscore.std")

a <- length(xvars)+1 
xvars2 <- xvars
xvars2[a] <- "distance"
xvars
xvars2

summary.unmatched <-CreateTableOne(vars=xvars, strata="RS", data=data12, test=TRUE)
print(summary.unmatched, smd=TRUE)
## write.csv(print(summary.unmatched, smd=TRUE), file="M:/Uber_NHTS/31_Conference/BeforeMatching03.csv")
## ExtractSmd(summary.unmatched)

nhtsualist2 <- read.csv(file="M:/Uber_NHTS/11_Scratch/nhtsualist2.csv", header=TRUE, sep=",")


## How to deal with perfect separation in logistic regression?
## https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
## https://stats.stackexchange.com/questions/40876/what-is-the-difference-between-a-link-function-and-a-canonical-link-function
## https://cran.r-project.org/web/packages/logistf/logistf.pdf
## install.packages("logistf")
## library(logistf)

## install.packages("optmatch")
library(optmatch)

## PSM for each UA separately 
## https://cran.r-project.org/web/packages/MatchIt/vignettes/matchit.pdf
matchit.UA <- NULL
## pooled.matchit <- NULL
match.data.UA <- NULL 
match.data.all <- NULL 
for (i in 1:50) {
  ## glm.UA <- glm(RS ~ LIF_CYC02 + LIF_CYC03 + LIF_CYC04 + LIF_CYC05 + LIF_CYC06 + LIF_CYC07 + LIF_CYC08 +
  ##                 LIF_CYC09 + LIF_CYC10 + WRKCOUNT + DRVRCNT  + NUMCHILD + YOUNGCHILD + HHFAMINC02 + 
  ##                 HHFAMINC03 + HHFAMINC04 + HHFAMINC05 + HHFAMINC06 + HHFAMINC07 + HHFAMINC08 + 
  ##                 HHFAMINC09 + HHFAMINC10 + HHFAMINC11 + HOMEOWN + homeden + workden + R_SEX + R_AGE + 
  ##                 R_RACE02 + R_RACE03 + R_RACE04 + R_RACE05 + R_RACE06 + R_RACE97 + R_HISP + 
  ##                 DRIVER + EDUC02 + EDUC03 + EDUC04 + EDUC05 + OCCAT02 + OCCAT03 + OCCAT04 + OCCAT97 + 
  ##                 Telecommute01 + Telecommute02 + Telecommute03 + Telecommute04 + medcon + DELIVER + 
  ##                 GTscore.std, family=binomial(link="probit"), control = list(maxit = 100), 
  ##               data=data12[data12$UACE10==nhtsualist2$UACE10[i], ])
  ## a <- fitted.values(glm.UA)
  ## caliper.UA <- 0.25*sd(a)
  ## https://stats.stackexchange.com/questions/118636/nearest-neighbor-matching-in-r-using-matchit
  set.seed(1000)
  matchit.UA <- matchit(psm, method="nearest", ratio=1, replace=TRUE, model="probit", 
                        data=data12[data12$UACE10==nhtsualist2$UACE10[i], ], caliper=0.25)
  ## pooled.matchit <-rbind(pooled.matchit, pooled.match.UA) ## matchit object 
  match.data.UA <- match.data(matchit.UA)
  ## https://r.iq.harvard.edu/docs/matchit/2.4-20/How_Exactly_are.html
  b <- min(match.data.UA[match.data.UA$RS==0, ]$weights)
  weights2 <- numeric()
  match.data.UA$weights2 <- ifelse(match.data.UA$RS==0, match.data.UA$weights/b, match.data.UA$weights)
  match.data.all <- rbind(match.data.all, match.data.UA)  ## match.data object 
}
## warnings() 

## examine weights from matchit-set.seed(1000) method=nearest, ratio=3, replace=TRUE, caliper=0.25 i=1
## temp1 <- as.data.frame(pooled.match.UA$match.matrix)
## temp1$id <- rownames(temp1)
## temp1 <- temp1[, c(4, 1:3)]
## colnames(temp1) <- c("treatid", "match1", "match2", "match3")
## temp1[1:20, ]

## nrow(temp1[is.na(temp1$match1)==FALSE, ]) # 93 control cases
## nrow(temp1[is.na(temp1$match2)==FALSE, ]) # 88 control cases (-5)
## nrow(temp1[is.na(temp1$match3)==FALSE, ]) # 81 control cases (-12)
# 200 unique control cases 

## a <- ddply(temp1, .(match1), summarize, sum(is.na(match1)==FALSE))
## b <- ddply(temp1, .(match2), summarize, sum(is.na(match1)==FALSE))
## c <- ddply(temp1, .(match3), summarize, sum(is.na(match1)==FALSE))
## colnames(a) <- c("matchid", "count")
## colnames(b) <- c("matchid", "count")
## colnames(c) <- c("matchid", "count")
## d <- rbind(a, b)
## d <- rbind(d, c)

## e <- ddply(d, .(matchid), summarize, sum(count))
## colnames(e) <- c("matchid", "cases")
## e <- e[order(-e$cases), ]
## table(e$cases)


## summary(pooled.match[pooled.match$RS==1, ]$distance)
## summary(pooled.match[pooled.match$RS==0, ]$distance)

sum(match.data.all[match.data.all$RS==1, ]$weights)
table(match.data.all[match.data.all$RS==1, ]$weights)
sum(match.data.all[match.data.all$RS==0, ]$weights)
table(match.data.all[match.data.all$RS==0, ]$weights)

sum(match.data.all[match.data.all$RS==1, ]$weights2)
sum(match.data.all[match.data.all$RS==0, ]$weights2)
summary(match.data.all[match.data.all$RS==1, ]$weights2)
summary(match.data.all[match.data.all$RS==0, ]$weights2)
table(match.data.all[match.data.all$RS==0, ]$weights2)



match.data.all$weights3 <- NA
match.data.all$weights3 <- ifelse(match.data.all$RS==1, 1, match.data.all$weights3)
match.data.all$weights3 <- ifelse(match.data.all$RS==0 & match.data.all$weights2<1.1,  match.data.all$distance/(1-match.data.all$distance), match.data.all$weights3)
match.data.all$weights3 <- ifelse(match.data.all$RS==0 & match.data.all$weights2>1.1,  match.data.all$distance/(1-match.data.all$distance)*match.data.all$weights2, match.data.all$weights3)
summary(match.data.all$weights3)


quantile(match.data.all$distance, c(0.05, 0.10, 0.90, 0.95))
hist(match.data.all[match.data.all$RS==1, ]$distance)
hist(match.data.all[match.data.all$RS==0, ]$distance)

## weighted histogram for ln(HHVEHCNT)
## install.packages("plotrix")
library(plotrix)
temp <- match.data.all[, c(1:8, 73:75)]
temp$LNVEH    <- log(temp$HHVEHCNT+1) 
## weighted.hist(temp[temp$RS==1, ]$LNVEH, temp[temp$RS==1, ]$weights3)
## weighted.hist(temp[temp$RS==0, ]$LNVEH, temp[temp$RS==0, ]$weights3)

hist(match.data.all$HHVEHCNT)
summary(match.data.all$weights)
summary(match.data.all$weights2)
summary(match.data.all$weights3)
## hist(pooled.match$weights3)
summary(match.data.all$distance)
## nrow(pooled.match[pooled.match$distance<0.01, ])
## ps <- 0.9999
## ps/(1-ps) 

nrow(match.data.all[match.data.all$RS==1, ])
sum(match.data.all[match.data.all$RS==1, ]$weights2)
nrow(match.data.all[match.data.all$RS==0, ])
sum(match.data.all[match.data.all$RS==0, ]$weights2)

## Created weighted data object: https://rpubs.com/kaz_yos/matching-weights

library(grid) 
library(Matrix)
library(survey)

temp1 <- match.data.all
temp1$HHVEHCNT2 <- as.numeric(as.character(temp1$HHVEHCNT2))
temp1$PTUSED2   <- as.numeric(as.character(temp1$PTUSED2))
temp1$NWBMODE2  <- as.numeric(as.character(temp1$NWBMODE2))
temp1$HOMEOWN   <- as.numeric(as.character(temp1$HOMEOWN))
temp1$R_SEX     <- as.numeric(as.character(temp1$R_SEX))
temp1$R_HISP    <- as.numeric(as.character(temp1$R_HISP))

match.data.all.wt <- svydesign(ids = ~ 1, data = temp1, weights = ~ weights2)

## Weighted table with tableone
summary.test <- svyCreateTableOne(vars = xvars, strata ="RS", data = match.data.all.wt)
print(summary.test, test=TRUE, smd = TRUE)
write.csv(print(summary.test, test=TRUE, smd = TRUE), file="M:/Uber_NHTS/31_Conference/AfterMatching03.csv")
## ExtractSmd(summary.test)

## plot(pooled.match.UA, type="jitter")  ## matchit object, the last UA only after the for loop
## plot(pooled.match.UA, type="hist")    ## matchit object, the last UA only after the for loop
## par(mfrow=c(1,1))



## Generate SMD plots before/after matching
## https://rpubs.com/kaz_yos/matching-weights
## matching.cov.list0 <- c("# RIDESHARE", "# HHVEHCNT", "O HHVENCHT", "O PT", "O Walk/Bike", "Yes RIDESHARE")

dataPlot <- data.frame(variable   = rownames(ExtractSmd(summary.unmatched)), 
                       Unadjusted = ExtractSmd(summary.unmatched)[, "1 vs 2"], 
                       Weighted   = ExtractSmd(summary.test)[, "1 vs 2"])
dataPlot <- dataPlot[c(7:nrow(dataPlot)), ]
## dataPlot <- dataPlot[c(35, 61, 26, 27, 28, 45:49, 50:54, 55:59, 15:25, 1:10), ]


matching.cov.list <- c("one adult, no children", "2+ adults, no children", "one adult, youngest child 0-5", "2+ adults, youngest child 0-5", 
          "one adult, youngest child 6-15", "2+ adults, youngest child 6-15", "one adult, youngest child 16-21", "2+ adults, youngest child 16-21", "one adult, retired, no children", 
          "2+adults, retired, no children", "# HH worker", "# HH driver", "# child", "Yes child 0-4", 
          "Less than $10,000", "$10,000-$14,999", "$15,000-$24,999", "$25,000-$34,999", "$35,000-$49,999", 
          "$50,000-$74,999", "$75,000-$99,999", "$100,000-$124,999", "$125,000-$149,999", "$150,000-$199,999", 
          "$200,000 or more", "Home owner", "Home density", "Work density", "% with collge", 
          "% 25-34", "% HH with no cars", "Tech job density", "Service job density", "Female", 
          "Age", "White", "Black", "Asian", "American Indian", 
          "Pacific Islander", "Multiple races", "Some other race", "Hispanic", "Driver", 
          "Less than HS", "Highschool", "Some college", "Bachelor's", "Graduate",
          "Sales or service", "Clerical/admin", "Manufacturing", "Professional", "Something else", 
          "Telecommute 0", "Telecommute 1-3", "Telecommute 4-7", "Telecommute 8-11", "Telecommute 12+", 
          "Medical condition", "# online delivery", "Google Trend")

## install.packages("reshape2")
library(reshape2)
dataPlotMelt <- melt(data          = dataPlot, 
                     id.vars       = "variable", 
                     variable.name = "method", 
                     value.name    = "SMD")
## dataPlotMelt$variable[1:62]<- matching.cov.list
## dataPlotMelt$variable[63:124]<- matching.cov.list
 
varsOrderedBySmd <- rownames(dataPlot)[order(dataPlot[, "Unadjusted"])]
dataPlotMelt$variable <- factor(as.character(dataPlotMelt$variable), 
                                 levels = varsOrderedBySmd)
dataPlotMelt$method   <- factor(dataPlotMelt$method, 
                                levels = c("Weighted", "Unadjusted"))

jpeg("M:/Uber_NHTS/31_Conference/SMDplot03.jpg", 
     width = 600, height = 600, units = "px", pointsize = 72,
     quality = 600)
ggplot(data = dataPlotMelt, mapping = aes(x=variable, y=SMD, group=method, linetype=method)) + 
  geom_line() +
  geom_point() + 
  geom_hline(yintercept = 0, size = 0.3) + 
  geom_hline(yintercept = 0.1, size = 0.1, color="red") + 
  coord_flip() + 
  theme_bw() + theme(legend.key = element_blank())
dev.off()

## Unweighted table with tableone 
## summary.test <- CreateTableOne(vars=xvars, strata="RS", data=pooled.match)
## print(summary.test, test=TRUE, smd=TRUE)

## The list of UA in the final sample 
temp1 <- as.data.frame(table(match.data.all$UACE10))
temp1$Var1 <- as.integer(as.character(temp1$Var1))
colnames(temp1)[1] <- "UACE10"
nhtsualist2 <- read.csv(file="M:/Uber_NHTS/11_Scratch/nhtsualist2.csv", header=TRUE, sep=",")
temp1 <- merge(nhtsualist2, temp1, by="UACE10")
temp1

nrow(match.data.all[match.data.all$RS==1, ])
sum(match.data.all[match.data.all$RS==1, ]$weights)
nrow(match.data.all[match.data.all$RS==0, ])
sum(match.data.all[match.data.all$RS==0, ]$weights)
sum(match.data.all[match.data.all$RS==0, ]$weights2)


par(mfrow=c(2,2))
hist(as.numeric(as.character(match.data.all[match.data.all$RS==0, ]$HHVEHCNT2)), #breaks=c(0, 1, 2, 3, 4), 
     main="Household vehicles of non-users", xlab="0, 1, 2, & 3+ cars")#, col="green")
hist(as.numeric(as.character(match.data.all[match.data.all$RS==1, ]$HHVEHCNT2)), #breaks=c(0, 1, 2, 3, 4), 
     main="Household vehicles of frequent users", xlab="0, 1, 2, & 3+ cars")


par(mfrow=c(2,2))
hist(as.numeric(as.character(match.data.all$RIDESHARE)), #breaks=c(0,10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
     main="Frequency of ridehailing last month", xlab="0 or 8+ trips")
hist(as.numeric(as.character(match.data.all$HHVEHCNT2)), #breaks=c(0, 1, 2, 3, 4), 
     main="Household vehicles", xlab="0, 1, 2, & 3+ cars")
hist(as.numeric(as.character(match.data.all$PTUSED2)), #breaks=c(0, 1, 2, 3, 4), 
     main="Days of using public transit last month", xlab="0, 1-3, 4-11, & 12+ days")
hist(as.numeric(as.character(match.data.all$NWBMODE2)), #breaks=c(0, 1, 2, 3, 4), 
     main="Trips by walking/biking last week", xlab="0, 1-4, 5-8, & 9+ trips")


par(mfrow=c(2,2))
hist(as.numeric(as.character(match.data.all[match.data.all$RS==1, ]$LNHHVEHCNT)))
hist(as.numeric(as.character(match.data.all[match.data.all$RS==0, ]$LNHHVEHCNT)))
## hist(as.numeric(as.character(match.data.all[match.data.all$RS==1, ]$LNHHVEHCNT)))
## hist(as.numeric(as.character(match.data.all[match.data.all$RS==0, ]$LNHHVEHCNT)))

par(mfrow=c(2,2))
hist(match.data.all[match.data.all$RS==1, ]$homeden, breaks = c(-3, -2, -1, 0, 1, 2, 3, 4), ylim=c(0,320))
hist(match.data.all[match.data.all$RS==0, ]$homeden, breaks = c(-3, -2, -1, 0, 1, 2, 3, 4), ylim=c(0,320))
hist(match.data.all[match.data.all$RS==1, ]$workden, breaks = c(-3, -2, -1, 0, 1, 2, 3, 4), ylim=c(0,400))
hist(match.data.all[match.data.all$RS==0, ]$workden, breaks = c(-3, -2, -1, 0, 1, 2, 3, 4), ylim=c(0,400))
par(mfrow=c(1,1))


match.data.all$LNHHVEHCNT <- log(match.data.all$HHVEHCNT+1)
match.data.all$uaorder <- NULL
colnames(match.data.all)
data13 <- match.data.all[, c(2:6, 128, 7:71, 1, 74:127)]
colnames(data13)



## Task 4-3. Prepare for mplut estimation ----

varname <- as.data.frame(colnames(data13))
nrow(varname)-54

mplusname <- c("id1", "id2", "y1",  "y2",  "y3",  "y4",  "y5",  "y6", "y7", 
               "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",  "x8",  "x9",  "x10", 
               "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", 
               "x21", "x22", "x23", "x24", "x25", "x26", "y8",  "x27", "x28", "x29", "x30", 
               "x31", "x32", "x33", "x34", "x35", "x36", "x37", "x38", "x39", "x40",
               "x41", "x42", "x43", "x44", "x45", "x46", "x47", "x48", "x49", "x50", 
               "x51", "x52", "x53", "x54", "x55", "x56", "x57", "x58", "x59", "x60", 
               "x61", "x62", 
               "UA01", "UA02", "UA03", "UA04", "UA05", "UA06", "UA07", "UA08", "UA09", "UA10", 
               "UA11", "UA12", "UA13", "UA14", "UA15", "UA16", "UA17", "UA18", "UA19", "UA20", 
               "UA21", "UA22", "UA23", "UA24", "UA25", "UA26", "UA27", "UA28", "UA29", "UA30", 
               "UA31", "UA32", "UA33", "UA34", "UA35", "UA36", "UA37", "UA38", "UA39", "UA40", 
               "UA41", "UA42", "UA43", "UA44", "UA45", "UA46", "UA47", "UA48", "UA49", "UA50",
               "ps", "WT1", "WT2", "WT3") 
length(mplusname)-54

a <- cbind(varname, mplusname) 
matching.cov.list <- c("HOUSEID", "PERSONID", "# RIDESHARE", "# HHVEHCNT", "O HHVENCHT", "LN HHVENCNT", 
                       "O PT", "O Walk/Bike", "Yes RIDESHARE",
                       "one adult, no children", "2+ adults, no children", "one adult, youngest child 0-5", "2+ adults, youngest child 0-5", 
                       "one adult, youngest child 6-15", "2+ adults, youngest child 6-15", "one adult, youngest child 16-21", "2+ adults, youngest child 16-21", "one adult, retired, no children", 
                       "2+adults, retired, no children", "# HH worker", "# HH driver", "# child", "Yes child 0-4", 
                       "Less than $10,000", "$10,000-$14,999", "$15,000-$24,999", "$25,000-$34,999", "$35,000-$49,999", 
                       "$50,000-$74,999", "$75,000-$99,999", "$100,000-$124,999", "$125,000-$149,999", "$150,000-$199,999", 
                       "$200,000 or more", "Home owner", "Home density", "Work density", "% with collge", 
                       "% 25-34", "% HH with no cars", "Tech job density", "Service job density", "Female", 
                       "Age", "White", "Black", "Asian", "American Indian", 
                       "Pacific Islander", "Multiple races", "Some other race", "Hispanic", "Driver", 
                       "Less than HS", "Highschool", "Some college", "Bachelor's", "Graduate",
                       "Sales or service", "Clerical/admin", "Manufacturing", "Professional", "Something else", 
                       "Telecommute 0", "Telecommute 1-3", "Telecommute 4-7", "Telecommute 8-11", "Telecommute 12+", 
                       "Medical condition", "# online delivery", "Google Trend", "UACE10",  
                       "UA01", "UA02", "UA03", "UA04", "UA05", "UA06", "UA07", "UA08", "UA09", "UA10", 
                       "UA11", "UA12", "UA13", "UA14", "UA15", "UA16", "UA17", "UA18", "UA19", "UA20", 
                       "UA21", "UA22", "UA23", "UA24", "UA25", "UA26", "UA27", "UA28", "UA29", "UA30", 
                       "UA31", "UA32", "UA33", "UA34", "UA35", "UA36", "UA37", "UA38", "UA39", "UA40", 
                       "UA41", "UA42", "UA43", "UA44", "UA45", "UA46", "UA47", "UA48", "UA49", "UA50",
                       "pscore", "WT1", "WT2", "WT3")
length(matching.cov.list)-54
b <- cbind(a, matching.cov.list)
b[3:9, ]
b[10:72, ]
b


## rownames(pooled.match) <- NULL 
## nrow(pooled.match)

## data13$R_AGESQ <- data13$R_AGE*data13$R_AGE
## data13 <- data13[, c(1:44, 127, 45:126)]

## deliver.boxplot <- data13[, c("RS", "RIDESHARE", "DELIVER")] 
## deliver.boxplot$DELIVER <- iselse(data13$DELIVER==0,0, data13$DELIVER)  
## deliver.boxplot$DELIVER <- iselse(data13$DELIVER> 0  & data13$DELIVER<5, 1, data13$DELIVER) 
## deliver.boxplot$DELIVER <- iselse(data13$DELIVER>=5  & data13$DELIVER<10,2, data13$DELIVER) 
## deliver.boxplot$DELIVER <- iselse(data13$DELIVER>=10 & data13$DELIVER<20,3, data13$DELIVER)  
## deliver.boxplot$DELIVER <- iselse(data13$DELIVER>=20 & data13$DELIVER<30,4, data13$DELIVER) 
## deliver.boxplot$DELIVER <- iselse(data13$DELIVER>=30,5, data13$DELIVER) 

## deliver.boxplot$DELIVER <- log(deliver.boxplot$DELIVER+1) ## b/c extreme outliers 
## boxplot(DELIVER ~ RIDESHARE, data=deliver.boxplot)
## boxplot(DELIVER ~ RIDESHARE, data=deliver.boxplot[deliver.boxplot$DELIVER<99, ])

data13$DELIVER <- log(data13$DELIVER+1) ## b/c extreme outliers 
colnames(data13)
sum <- sapply(data13[, sapply(data13, is.factor)==FALSE], sum)
sum[sum==0]


write.csv(data13, "M:/Uber_NHTS/15_Model/pooled_match_03.csv")

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(data13, col.vars="NWBMODE2", row.vars=c("EDUC03", "EDUC04", "EDUC05"), type=
           "f", addmargins=FALSE)

nhtsualist$uaorder <- rownames(nhtsualist)

ualist <- ddply(data13, .(UACE10), summarize, Count=sum(is.na(UACE10)==FALSE))
merge(ualist, nhtsualist, by=c("UACE10"))

a <- sapply(data13[, c(73:122)], sum)
a[a>0]
sum(a[a>0])
## boxplot(as.numeric(data13$NWBMODE2)~UA44, data=data13)
## summary(as.numeric(as.character(data13[data13$UA44==1 & data13$RS==1, ]$NWBMODE2)))
## summary(as.numeric(as.character(data13[data13$UA44==1 & data13$RS==0, ]$NWBMODE2)))


