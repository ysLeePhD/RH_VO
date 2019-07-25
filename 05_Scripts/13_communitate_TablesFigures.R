
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



# Step 5. Generate Tables/Figures ---- 

## Task 5-1. Histograms of endogenous variables ---- 

library(grid)
library(gridExtra)

# from line 311 12_prepdata_PSM.R
data13 <- read_rds(file.path(filepath, "11_Scratch/data13.rds"))

names(data13)
table(data13$RS)
table(data13$HHVEHCNT)


plotpath <- "C:/Users/ylee366/Dropbox (GaTech)/3a_ResearchCEE/09_Uber_NHTS/17_Visualize/02_ICMC2019"

# columns: RS four groups 
# rows : BE, VO, RS, PT, WB 

p1 <- ggplot(data = data13) +
  geom_histogram(aes(x = data13$home.den.pp, y = ..density..), bins = 40, color = "grey30", fill = "white") + 
  coord_cartesian(xlim = c(-4, 4)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title ="", x = "", y = "") + 
  theme(strip.text.x = element_blank()) + 
  facet_wrap(facets = data13$RS, nrow = 1, ncol = 4)

# ggplot(data = data13) +
#   geom_bar(aes(x = as.factor(HHVEHCNT), y = ..prop.., group = 1)) + 
#   coord_cartesian(xlim = c(0, 12)) + 
#   scale_y_continuous(labels = scales::percent) + 
#   labs(title ="", x = "", y = "Percent") + 
#   theme(strip.text.x = element_blank()) + 
#   facet_wrap(facets = data13$RS, nrow = 1, ncol = 4)

p2 <- ggplot(data = data13) +
  geom_bar(aes(x = as.factor(HHVEHCNT2), y = ..prop.., group = 1), color = "grey30", fill = "white") + 
  coord_cartesian(xlim = c(1, 4)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title ="", x = "", y = "") + 
  theme(strip.text.x = element_blank()) + 
  facet_wrap(facets = data13$RS, nrow = 1, ncol = 4)

p3 <- ggplot(data = temp) +
  geom_bar(aes(x = PTUSED2, y = ..prop.., group = 1), color = "grey30", fill = "white") + 
  coord_cartesian(xlim = c(1, 4)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title ="", x = "", y = "") + 
  theme(strip.text.x = element_blank()) + 
  facet_wrap(facets = data13$RS, nrow = 1, ncol = 4)

p4 <- ggplot(data = temp) +
  geom_bar(aes(x = NWBMODE2, y = ..prop.., group = 1), color = "grey30", fill = "white") + 
  coord_cartesian(xlim = c(1, 4)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title ="", x = "", y = "") + 
  theme(strip.text.x = element_blank()) + 
  facet_wrap(facets = data13$RS, nrow = 1, ncol = 4)

grid.arrange(p1, p2, p3, p4, ncol=1)

ggsave(filename = file.path(plotpath, "HOMEDENPP.jpg"), plot = p1, 
       width = 18, height = 3, units = "in", dpi = 300) 

ggsave(filename = file.path(plotpath, "HHVEHCNT2.jpg"), plot = p2, 
       width = 18, height = 3, units = "in", dpi = 300) 

ggsave(filename = file.path(plotpath, "PTUSED2.jpg"), plot = p3, 
       width = 18, height = 3, units = "in", dpi = 300) 

ggsave(filename = file.path(plotpath, "NWBMODE2.jpg"), plot = p4, 
       width = 18, height = 3, units = "in", dpi = 300) 




## Task 5-2. Summary statistics of four groups ---- 

temp <- data13 %>% 
  select(RS, HHVEHCNT2, WRKCOUNT, DRVRCNT, NUMCHILD, HOMEOWN2, home.den.pp, work.den.pp, R_AGE, 
         DRIVER, EDUC03, EDUC04, OCCAT04, deliver01, deliver02, deliver03, deliver04) %>%
  mutate(RS = as.factor(RS), 
         HOMEOWN2 = as.integer(as.character(HOMEOWN2)), 
         college = EDUC03 + EDUC04) %>%
  select(-EDUC03, -EDUC04)


temp2 <- list(
  temp %>% filter(RS==0) %>% select(-RS, -HHVEHCNT2), 
  temp %>% filter(RS==1) %>% select(-RS, -HHVEHCNT2), 
  temp %>% filter(RS==2) %>% select(-RS, -HHVEHCNT2), 
  temp %>% filter(RS==3) %>% select(-RS, -HHVEHCNT2)
) 

sumstat01 <- 
  rbind(map_dbl(temp2[[1]], ~mean(.) %>% round(digits =3)), 
      map_dbl(temp2[[2]], ~mean(.) %>% round(digits =3)), 
      map_dbl(temp2[[3]], ~mean(.) %>% round(digits =3)), 
      map_dbl(temp2[[4]], ~mean(.) %>% round(digits =3))
  ) %>% 
  t() %>% 
  as.data.frame()

sumstat01$varnames <- rownames(sumstat01)

temp3 <- list(
  temp %>% filter(RS==0) %>% select(HHVEHCNT2), 
  temp %>% filter(RS==1) %>% select(HHVEHCNT2), 
  temp %>% filter(RS==2) %>% select(HHVEHCNT2), 
  temp %>% filter(RS==3) %>% select(HHVEHCNT2)
) 

sumstat02 <- 
  rbind(map(temp3[[1]], table)[[1]]/nrow(temp3[[1]]), 
     map(temp3[[2]], table)[[1]]/nrow(temp3[[2]]), 
     map(temp3[[3]], table)[[1]]/nrow(temp3[[3]]), 
     map(temp3[[4]], table)[[1]]/nrow(temp3[[4]])
  ) %>% 
  t() %>% 
  round(digits = 3) %>% 
  as.data.frame()

sumstat02$varnames <- rownames(sumstat02)

rbind(sumstat02, sumstat01)[, c(5, 1:4)] %>% write_csv(file.path(filepath, "11_Scratch/sumstat.csv"))




## Task 5-3. Compute probabilities of owning zero, 1, 2, and 3+ vehicles ---- 


