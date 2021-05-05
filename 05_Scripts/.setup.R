# to increase speed, install packages from source instead of pre-compiled version (i.e., complie)
# some useful scripts ---- 
# remove.packages("")
# install.packages("", type="source")
# install.packages("devtools", dep = TRUE)
# devtools::update_packages("")
# library()

# update packages ---- 
pkgs <- 
  c("devtools", "plyr", "tidyverse", 
    "sf", "tigris", "tidycensus", "mapview", "tmap", 
    "psych", "tableone", "MatchIt", "optmatch", "MASS", 
    "stargazer", "questionr", "spatstat", 
    "grid", "gridExtra")

for (i in seq_along(pkgs)){
  devtools::update_packages(pkgs[[i]])#, dep = TRUE) # update one at a time 
}
devtools::install_github("jamgreen/lehdr")

pkgs2 <- c(pkgs, "foreign", "here") # included in Base R + "lehdr"
inst = lapply(pkgs2, library, character.only = TRUE) # load them

options(stringsAsFactors = FALSE)
# .Machine$integer.max #check the integer max value
filepath  <- str_remove(here(), "/RH_VO")
path_NHTS <- str_remove(here(), "/RH_VO")
path_plot <- file.path(path_NHTS, "17_Visualize/02_ICMC2019")

options(tigris_use_cache = TRUE)
tigris_cache_dir(paste0(path_NHTS, "/05_Census/tigris"))
census_api_key("3b1d8912e33aa2d4c01bf1abc84729cfeb7cd6cd", install = TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")

# tigris_cache_dir - trouble shoot ----
# If tigris_cache_dir(paste0(filepath, "/05_Census/tigris")) doesn't work, 
# Run below scripts line by line from the function 

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