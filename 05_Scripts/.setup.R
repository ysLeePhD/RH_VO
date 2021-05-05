
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

if (!require("MASS")) install.packages("MASS", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("questionr")) install.packages("questionr", repos = "http://cran.us.r-project.org", dependencies = TRUE)
# if (!require("spatstat")) install.packages("spatstat", repos = "http://cran.us.r-project.org", dependencies = TRUE)

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

library(MASS)
library(questionr)
# library(spatstat)

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


