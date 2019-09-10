
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

if (!require("questionr")) install.packages("questionr", repos = "http://cran.us.r-project.org", dependencies = TRUE)
# if (!require("spatstat")) install.packages("spatstat", repos = "http://cran.us.r-project.org", dependencies = TRUE)

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

### Task 5-3-1. For occasional users ----

across01a <- read_csv(file.path(filepath, "15_Model/round03/round03_01/across01.csv"))
across01a$HHVEHCNT2 %>% table() / nrow(across01a)

across01b <- 
  across01a %>% 
  select(
    LIF_CYC02, LIF_CYC03, LIF_CYC04, LIF_CYC05, LIF_CYC06, 
    HOMEOWN2, EDUC02, EDUC03, EDUC04, home.den.pp, RS, WRKCOUNT, 
    HHFAMINC02, HHFAMINC03, HHFAMINC04, HHFAMINC05, HHFAMINC06, 
    SPHONE01, SPHONE02, SPHONE03, SPHONE04, 
    UA01, UA02, UA03, UA04, UA05, UA06, UA07, UA08, UA09, UA10, 
    UA11,       UA13, UA14, UA15, UA16, UA17, UA18, UA19, UA20, 
    UA21, UA22, UA23, UA24, UA25, UA26, UA27, UA28, UA29, UA30, 
    UA31, UA32, UA33, UA34, UA35, UA36, UA37, UA38, UA39, UA40, 
    UA41, UA42, UA43, UA44, UA45, UA46, UA47, UA48, UA49, UA50 
  ) 

be_exp <- across01b %>%
  select(LIF_CYC02, LIF_CYC03, LIF_CYC04, LIF_CYC05, LIF_CYC06, 
         HOMEOWN2, EDUC02, EDUC03, EDUC04, 
         UA01, UA02, UA03, UA04, UA05, UA06, UA07, UA08, UA09, UA10, 
         UA11,       UA13, UA14, UA15, UA16, UA17, UA18, UA19, UA20, 
         UA21, UA22, UA23, UA24, UA25, UA26, UA27, UA28, UA29, UA30, 
         UA31, UA32, UA33, UA34, UA35, UA36, UA37, UA38, UA39, UA40, 
         UA41, UA42, UA43, UA44, UA45, UA46, UA47, UA48, UA49, UA50
  ) %>%
  as.matrix() 

rh_exp <- across01b %>%
  select(SPHONE01, SPHONE02, SPHONE03, SPHONE04, 
         UA01, UA02, UA03, UA04, UA05, UA06, UA07, UA08, UA09, UA10, 
         UA11,       UA13, UA14, UA15, UA16, UA17, UA18, UA19, UA20, 
         UA21, UA22, UA23, UA24, UA25, UA26, UA27, UA28, UA29, UA30, 
         UA31, UA32, UA33, UA34, UA35, UA36, UA37, UA38, UA39, UA40, 
         UA41, UA42, UA43, UA44, UA45, UA46, UA47, UA48, UA49, UA50
  ) %>%
  as.matrix() 

vo_exp1 <- across01b %>%
  select(WRKCOUNT, HHFAMINC02, HHFAMINC03, HHFAMINC04, HHFAMINC05, HHFAMINC06,     
         UA01, UA02, UA03, UA04, UA05, UA06, UA07, UA08, UA09, UA10, 
         UA11,       UA13, UA14, UA15, UA16, UA17, UA18, UA19, UA20, 
         UA21, UA22, UA23, UA24, UA25, UA26, UA27, UA28, UA29, UA30, 
         UA31, UA32, UA33, UA34, UA35, UA36, UA37, UA38, UA39, UA40, 
         UA41, UA42, UA43, UA44, UA45, UA46, UA47, UA48, UA49, UA50
  ) %>%
  as.matrix() 

coeff01 <- read_csv(file.path(filepath, "15_Model/round03/round03_01/round03_01_coeff.csv"))

be_coeff <- coeff01 %>%
  filter(end=="YBE") %>%
  select(coeff) %>%
  as.matrix() 

be_hat <- be_exp %*% be_coeff

rh_coeff <- coeff01 %>%
  filter(end=="YRH") %>%
  select(coeff) %>%
  as.matrix() 

rh_hat <- rh_exp %*% rh_coeff

vo_exp2 <- be_hat %>% 
  cbind(rh_hat, vo_exp1) 

vo_coeff <- coeff01 %>%
  filter(end=="YVO") %>%
  select(coeff) %>%
  as.matrix() 

vo_hat <- (vo_exp2 %*% vo_coeff) %>% as_tibble()

temp <- 
  cbind(be_hat, rh_hat, vo_hat, across01a$RS, across01a$HHVEHCNT2, across01a$weights2) 
names(temp) <- c("be_hat", "rh_hat", "vo_hat", "RH", "VO", "wt")
across01c <- temp %>% as_tibble()

# across01c %>%
#   mutate(
#     vo_prob0 = pnorm(-0.258 - vo_hat), 
#     vo_prob1 = pnorm( 0.260 - vo_hat) - pnorm(-0.258 - vo_hat), 
#     vo_prob2 = pnorm( 0.721 - vo_hat) - pnorm( 0.260 - vo_hat), 
#     vo_prob3 = 1- pnorm(0.721 - vo_hat),     
#     check = vo_prob0 + vo_prob1 + vo_prob2 + vo_prob3
#   ) %>% 
#   group_by(RH) %>%
#   summarize(
#     vo_prob0 = weighted.mean(vo_prob0, w = wt), 
#     vo_prob1 = weighted.mean(vo_prob1, w = wt), 
#     vo_prob2 = weighted.mean(vo_prob2, w = wt), 
#     vo_prob3 = weighted.mean(vo_prob3, w = wt) 
#   )


rh_hat_means0 <- 
  across01c %>% 
  filter(RH==0) %>% 
  .$rh_hat %>% 
  weighted.mean(w = across01c[across01c$RH==0, ]$wt) %>% #order-preserved
  rep(times = nrow(across01a))

# pnorm(0.130 - 0.1923003)

rh_hat_means1 <- 
  across01c %>% 
  filter(RH==1) %>% 
  .$rh_hat %>% 
  mean() %>%
  rep(times = nrow(across01a))

# 1- pnorm(0.130 - 0.2886799)

across01c$vo_hat0 <- cbind(be_hat, rh_hat_means0, vo_exp1) %*% vo_coeff
across01c$vo_hat1 <- cbind(be_hat, rh_hat_means1, vo_exp1) %*% vo_coeff
  
across01c <- 
  across01c %>%
  mutate(
    vo_prob0_rh0 = pnorm(-0.258 - vo_hat0), 
    vo_prob1_rh0 = pnorm( 0.260 - vo_hat0) - pnorm(-0.258 - vo_hat0), 
    vo_prob2_rh0 = pnorm( 0.721 - vo_hat0) - pnorm( 0.260 - vo_hat0), 
    vo_prob3_rh0 = 1- pnorm(0.721 - vo_hat0),     
    check_rh0 = vo_prob0_rh0 + vo_prob1_rh0 + vo_prob2_rh0 + vo_prob3_rh0, 
    vo_prob0_rh1 = pnorm(-0.258 - vo_hat1), 
    vo_prob1_rh1 = pnorm( 0.260 - vo_hat1) - pnorm(-0.258 - vo_hat1), 
    vo_prob2_rh1 = pnorm( 0.721 - vo_hat1) - pnorm( 0.260 - vo_hat1), 
    vo_prob3_rh1 = 1- pnorm(0.721 - vo_hat1), 
    check_rh1 = vo_prob0_rh1 + vo_prob1_rh1 + vo_prob2_rh1 + vo_prob3_rh1, 
  ) 

# temp <- 
#   across01c %>% 
#   filter(RH==0) %>% 
#   select(
#     vo_prob0_rh0, vo_prob1_rh0, vo_prob2_rh0, vo_prob3_rh0, wt 
#   ) 

# a <- map_dbl(temp[1:4], ~weighted.mean(x=., w = temp$wt))

b <- across01c %>% # predicted cars based on "current" user status 
  filter(RH==1) %>% 
  select(
    vo_prob0_rh1, vo_prob1_rh1, vo_prob2_rh1, vo_prob3_rh1 
  ) %>% 
  map_dbl(mean)

a <- across01c %>% # predicted cars based on "counterfactual" user status 
  filter(RH==1) %>% 
  select(
    vo_prob0_rh0, vo_prob1_rh0, vo_prob2_rh0, vo_prob3_rh0 
  ) %>% 
  map_dbl(mean)

(b-a) %*% c(0, 1, 2, 3) # reduction/ditching of one car per 1,000 occasional users 
                        # less than once a week (1-3 times in the last 30 days) 



### Task 5-3-2. For moderate users ----

across02a <- read_csv(file.path(filepath, "15_Model/round03/round03_02/across02.csv"))
across02a$HHVEHCNT2 %>% table() / nrow(across02a)

across02b <- 
  across02a %>% 
  select(
    LIF_CYC02, LIF_CYC03, LIF_CYC04, LIF_CYC05, LIF_CYC06, 
    HOMEOWN2, EDUC02, EDUC03, EDUC04, home.den.pp, RS, WRKCOUNT, 
    HHFAMINC02, HHFAMINC03, HHFAMINC04, HHFAMINC05, HHFAMINC06, 
    work.den.pp, work.oldnbhd, 
    UA01, UA02, UA03, UA04, UA05, UA06, UA07, UA08, UA09, UA10, 
    UA11,       UA13, UA14, UA15, UA16, UA17, UA18, UA19, UA20, 
    UA21, UA22, UA23, UA24, UA25, UA26, UA27, UA28, UA29, UA30, 
    UA31, UA32, UA33, UA34, UA35, UA36, UA37, UA38, UA39, UA40, 
    UA41, UA42, UA43, UA44, UA45, UA46, UA47, UA48, UA49, UA50 
  ) 

be_exp <- across02b %>%
  select(LIF_CYC02, LIF_CYC03, LIF_CYC04, LIF_CYC05, LIF_CYC06, 
         HOMEOWN2, EDUC02, EDUC03, EDUC04, 
         UA01, UA02, UA03, UA04, UA05, UA06, UA07, UA08, UA09, UA10, 
         UA11,       UA13, UA14, UA15, UA16, UA17, UA18, UA19, UA20, 
         UA21, UA22, UA23, UA24, UA25, UA26, UA27, UA28, UA29, UA30, 
         UA31, UA32, UA33, UA34, UA35, UA36, UA37, UA38, UA39, UA40, 
         UA41, UA42, UA43, UA44, UA45, UA46, UA47, UA48, UA49, UA50
  ) %>%
  as.matrix() 

rh_exp <- across02b %>%
  select(work.den.pp, work.oldnbhd, 
         UA01, UA02, UA03, UA04, UA05, UA06, UA07, UA08, UA09, UA10, 
         UA11,       UA13, UA14, UA15, UA16, UA17, UA18, UA19, UA20, 
         UA21, UA22, UA23, UA24, UA25, UA26, UA27, UA28, UA29, UA30, 
         UA31, UA32, UA33, UA34, UA35, UA36, UA37, UA38, UA39, UA40, 
         UA41, UA42, UA43, UA44, UA45, UA46, UA47, UA48, UA49, UA50
  ) %>%
  as.matrix() 

vo_exp1 <- across02b %>%
  select(WRKCOUNT, HHFAMINC02, HHFAMINC03, HHFAMINC04, HHFAMINC05, HHFAMINC06,     
         UA01, UA02, UA03, UA04, UA05, UA06, UA07, UA08, UA09, UA10, 
         UA11,       UA13, UA14, UA15, UA16, UA17, UA18, UA19, UA20, 
         UA21, UA22, UA23, UA24, UA25, UA26, UA27, UA28, UA29, UA30, 
         UA31, UA32, UA33, UA34, UA35, UA36, UA37, UA38, UA39, UA40, 
         UA41, UA42, UA43, UA44, UA45, UA46, UA47, UA48, UA49, UA50
  ) %>%
  as.matrix() 

coeff02 <- read_csv(file.path(filepath, "15_Model/round03/round03_02/round03_02_coeff.csv"))

be_coeff <- coeff01 %>%
  filter(end=="YBE") %>%
  select(coeff) %>%
  as.matrix() 

be_hat <- be_exp %*% be_coeff

rh_coeff <- coeff01 %>%
  filter(end=="YRH") %>%
  select(coeff) %>%
  as.matrix() 

rh_hat <- rh_exp %*% rh_coeff

vo_exp2 <- be_hat %>% 
  cbind(rh_hat, vo_exp1) 

vo_coeff <- coeff01 %>%
  filter(end=="YVO") %>%
  select(coeff) %>%
  as.matrix() 

vo_hat <- (vo_exp2 %*% vo_coeff) %>% as_tibble()

temp <- 
  cbind(be_hat, rh_hat, vo_hat, across01a$RS, across01a$HHVEHCNT2, across01a$weights2) 
names(temp) <- c("be_hat", "rh_hat", "vo_hat", "RH", "VO", "wt")
across01c <- temp %>% as_tibble()

# across01c %>%
#   mutate(
#     vo_prob0 = pnorm(-0.258 - vo_hat), 
#     vo_prob1 = pnorm( 0.260 - vo_hat) - pnorm(-0.258 - vo_hat), 
#     vo_prob2 = pnorm( 0.721 - vo_hat) - pnorm( 0.260 - vo_hat), 
#     vo_prob3 = 1- pnorm(0.721 - vo_hat),     
#     check = vo_prob0 + vo_prob1 + vo_prob2 + vo_prob3
#   ) %>% 
#   group_by(RH) %>%
#   summarize(
#     vo_prob0 = weighted.mean(vo_prob0, w = wt), 
#     vo_prob1 = weighted.mean(vo_prob1, w = wt), 
#     vo_prob2 = weighted.mean(vo_prob2, w = wt), 
#     vo_prob3 = weighted.mean(vo_prob3, w = wt) 
#   )


rh_hat_means0 <- 
  across01c %>% 
  filter(RH==0) %>% 
  .$rh_hat %>% 
  weighted.mean(w = across01c[across01c$RH==0, ]$wt) %>% #order-preserved
  rep(times = nrow(across01a))

# pnorm(0.130 - 0.1923003)

rh_hat_means1 <- 
  across01c %>% 
  filter(RH==1) %>% 
  .$rh_hat %>% 
  mean() %>%
  rep(times = nrow(across01a))

# 1- pnorm(0.130 - 0.2886799)

across01c$vo_hat0 <- cbind(be_hat, rh_hat_means0, vo_exp1) %*% vo_coeff
across01c$vo_hat1 <- cbind(be_hat, rh_hat_means1, vo_exp1) %*% vo_coeff

across01c <- 
  across01c %>%
  mutate(
    vo_prob0_rh0 = pnorm(-0.258 - vo_hat0), 
    vo_prob1_rh0 = pnorm( 0.260 - vo_hat0) - pnorm(-0.258 - vo_hat0), 
    vo_prob2_rh0 = pnorm( 0.721 - vo_hat0) - pnorm( 0.260 - vo_hat0), 
    vo_prob3_rh0 = 1- pnorm(0.721 - vo_hat0),     
    check_rh0 = vo_prob0_rh0 + vo_prob1_rh0 + vo_prob2_rh0 + vo_prob3_rh0, 
    vo_prob0_rh1 = pnorm(-0.258 - vo_hat1), 
    vo_prob1_rh1 = pnorm( 0.260 - vo_hat1) - pnorm(-0.258 - vo_hat1), 
    vo_prob2_rh1 = pnorm( 0.721 - vo_hat1) - pnorm( 0.260 - vo_hat1), 
    vo_prob3_rh1 = 1- pnorm(0.721 - vo_hat1), 
    check_rh1 = vo_prob0_rh1 + vo_prob1_rh1 + vo_prob2_rh1 + vo_prob3_rh1, 
  ) 

# temp <- 
#   across01c %>% 
#   filter(RH==0) %>% 
#   select(
#     vo_prob0_rh0, vo_prob1_rh0, vo_prob2_rh0, vo_prob3_rh0, wt 
#   ) 

# a <- map_dbl(temp[1:4], ~weighted.mean(x=., w = temp$wt))

b <- across01c %>% # predicted cars based on "current" user status 
  filter(RH==1) %>% 
  select(
    vo_prob0_rh1, vo_prob1_rh1, vo_prob2_rh1, vo_prob3_rh1 
  ) %>% 
  map_dbl(mean)

a <- across01c %>% # predicted cars based on "counterfactual" user status 
  filter(RH==1) %>% 
  select(
    vo_prob0_rh0, vo_prob1_rh0, vo_prob2_rh0, vo_prob3_rh0 
  ) %>% 
  map_dbl(mean)

(b-a) %*% c(0, 1, 2, 3) # reduction/ditching of one car per 1,000 occasional users 
# less than once a week (1-3 times in the last 30 days) 



