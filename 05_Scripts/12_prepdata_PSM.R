
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

library(MASS)

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



# Step 4. Estimate binary logit/probit for balancing the final sample -----

## Task 4-1. PSM data prep ----

# if I don't start from scratch 
data10 <- read_rds(file.path(filepath, "11_Scratch/data10.rds"))
nrow(data10)

#4.1. Compute a new categorical variable, RS 
# 0 - no use in the last 30 days 
# 1 - less than once a week in the last 30 days 
# 2 - less than twice a week in the last 30 days 
# 3 - at least twice a week in the last 30 days 

table(data10$RIDESHARE)
# hist(data10[data10$RIDESHARE>0, ]$RIDESHARE)
quantile(data10[data10$RIDESHARE>0, ]$RIDESHARE, c(.9, .95, .975, .99, .999))

data10$RIDESHARE %>% class()
data10$RIDESHARE <- data10$RIDESHARE %>% round(digits = 0) %>% as.integer()
data10$RIDESHARE %>% class()

data10$RS <- ifelse(
  data10$RIDESHARE<0, NA_integer_, ifelse(
    data10$RIDESHARE==0, 0L, ifelse( # non-user
      data10$RIDESHARE<4, 1L, ifelse( # less than once a week 
        data10$RIDESHARE<8, 2L, ifelse( # less than twice a week  
          data10$RIDESHARE<12, 3L, 4L # less than 3 times a week vs. 3+ times a week 
        )
      )
    )
  )
)

table(data10$RS)
summary(data10$RS) # 32 NA cases

# a <- data10$RS %>% table() %>% as.data.frame() 
# a$Freq %>% sum()
# b <- nrow(data10[data10$RS >=0, ])
# a$pct <- (a[,2]/b*100) %>% round(digits = 1)
# a 

# choose *two* user types 
data10$RS %>% table()
data10$RS %>% table() %>% sum()
data10$RS %>% summary() # missing 32 cases 
data10$RS %>% class()

# data11 <- data10 %>% filter(data10$RS==0L | data10$RS!=0L)
data11 <- data10 %>% filter(is.na(RS) == FALSE)
data11$RS %>% table() 
data11$RS %>% table() %>% sum()

# (1- nrow(data11)/nrow(data10[is.na(data10$RS)==FALSE, ]))*100 # what % being dropped? 
names(data11)
temp <- map_lgl(data11, is.factor)
map(data11[!temp], summary)

data11$LIF_CYC01 <- ifelse(data11$LIF_CYC2=="1", 1L, 0L)
data11$LIF_CYC02 <- ifelse(data11$LIF_CYC2=="2", 1L, 0L)
data11$LIF_CYC03 <- ifelse(data11$LIF_CYC2=="3", 1L, 0L)
data11$LIF_CYC04 <- ifelse(data11$LIF_CYC2=="4", 1L, 0L)
data11$LIF_CYC05 <- ifelse(data11$LIF_CYC2=="5", 1L, 0L)
data11$LIF_CYC06 <- ifelse(data11$LIF_CYC2=="6", 1L, 0L)

data11$HHFAMINC01 <- ifelse(data11$HHFAMINC2=="1", 1, 0)
data11$HHFAMINC02 <- ifelse(data11$HHFAMINC2=="2", 1, 0)
data11$HHFAMINC03 <- ifelse(data11$HHFAMINC2=="3", 1, 0)
data11$HHFAMINC04 <- ifelse(data11$HHFAMINC2=="4", 1, 0)
data11$HHFAMINC05 <- ifelse(data11$HHFAMINC2=="5", 1, 0)
data11$HHFAMINC06 <- ifelse(data11$HHFAMINC2=="6", 1, 0)

data11$R_RACE01 <- ifelse(data11$R_RACE=="01", 1, 0)
data11$R_RACE02 <- ifelse(data11$R_RACE=="02", 1, 0)
data11$R_RACE03 <- ifelse(data11$R_RACE=="03", 1, 0)
data11$R_RACE04 <- ifelse(data11$R_RACE=="04", 1, 0)
data11$R_RACE05 <- ifelse(data11$R_RACE=="05", 1, 0)
data11$R_RACE06 <- ifelse(data11$R_RACE=="06", 1, 0)
data11$R_RACE97 <- ifelse(data11$R_RACE=="97", 1, 0)

data11$EDUC01 <- ifelse(data11$EDUC=="1", 1, 0)
data11$EDUC02 <- ifelse(data11$EDUC=="2", 1, 0)
data11$EDUC03 <- ifelse(data11$EDUC=="3", 1, 0)
data11$EDUC04 <- ifelse(data11$EDUC=="4", 1, 0)

data11$OCCAT01 <- ifelse(data11$OCCAT2=="1", 1, 0)
data11$OCCAT02 <- ifelse(data11$OCCAT2=="2", 1, 0)
data11$OCCAT03 <- ifelse(data11$OCCAT2=="3", 1, 0)
data11$OCCAT04 <- ifelse(data11$OCCAT2=="4", 1, 0)
data11$OCCAT05 <- ifelse(data11$OCCAT2=="5", 1, 0)

data11$Telecommute00 <- ifelse(data11$Telecommute=="0", 1, 0)
data11$Telecommute01 <- ifelse(data11$Telecommute=="1", 1, 0)
data11$Telecommute02 <- ifelse(data11$Telecommute=="2", 1, 0)
data11$Telecommute03 <- ifelse(data11$Telecommute=="3", 1, 0)
data11$Telecommute04 <- ifelse(data11$Telecommute=="4", 1, 0)

data11$deliver00 <- ifelse(data11$deliver=="0", 1, 0) 
data11$deliver01 <- ifelse(data11$deliver=="1", 1, 0) 
data11$deliver02 <- ifelse(data11$deliver=="2", 1, 0) 
data11$deliver03 <- ifelse(data11$deliver=="3", 1, 0) 
data11$deliver04 <- ifelse(data11$deliver=="4", 1, 0) 

data11$PC00 <- ifelse(data11$PC2=="0", 1, 0) 
data11$PC01 <- ifelse(data11$PC2=="1", 1, 0) 
data11$PC02 <- ifelse(data11$PC2=="2", 1, 0) 
data11$PC03 <- ifelse(data11$PC2=="3", 1, 0) 
data11$PC04 <- ifelse(data11$PC2=="4", 1, 0) 

data11$SPHONE00 <- ifelse(data11$SPHONE2=="0", 1, 0) 
data11$SPHONE01 <- ifelse(data11$SPHONE2=="1", 1, 0) 
data11$SPHONE02 <- ifelse(data11$SPHONE2=="2", 1, 0) 
data11$SPHONE03 <- ifelse(data11$SPHONE2=="3", 1, 0) 
data11$SPHONE04 <- ifelse(data11$SPHONE2=="4", 1, 0) 

data11$TAB00 <- ifelse(data11$TAB2=="0", 1, 0) 
data11$TAB01 <- ifelse(data11$TAB2=="1", 1, 0) 
data11$TAB02 <- ifelse(data11$TAB2=="2", 1, 0) 
data11$TAB03 <- ifelse(data11$TAB2=="3", 1, 0) 
data11$TAB04 <- ifelse(data11$TAB2=="4", 1, 0) 

data11$WEB00 <- ifelse(data11$WEB2=="0", 1, 0) 
data11$WEB01 <- ifelse(data11$WEB2=="1", 1, 0) 
data11$WEB02 <- ifelse(data11$WEB2=="2", 1, 0) 
data11$WEB03 <- ifelse(data11$WEB2=="3", 1, 0) 
data11$WEB04 <- ifelse(data11$WEB2=="4", 1, 0) 

names(data11)

usedvars <- c(
  "HOUSEID", "PERSONID", "RIDESHARE", "LNRS", "HHVEHCNT", "LNHHVEH", "HHVEHCNT2", 
  "PTUSED2", "LNPTUSED", "LNWBMODE", "NWBMODE2", "RS", 
  "LIF_CYC01", "LIF_CYC02", "LIF_CYC03", "LIF_CYC04", "LIF_CYC05", "LIF_CYC06", 
  "WRKCOUNT", "DRVRCNT", "NUMCHILD", "YOUNGCHILD", 
  "HHFAMINC01", "HHFAMINC02", "HHFAMINC03", "HHFAMINC04", "HHFAMINC05", "HHFAMINC06", 
  "HOMEOWN2", 
  "home.den.st", "home.den.pp", "home.jobrich", "home.oldnbhd", "home.sfh",      
  "home.pctcoll", "home.pctyoung", "home.pctxveh", "work.den.tech", "work.den.serv", 
  "work.den.st", "work.den.pp", "work.jobrich", "work.oldnbhd", "work.sfh", 
  "R_SEX", "R_AGE", "R_RACE01", "R_RACE02", "R_RACE03", "R_RACE04", "R_RACE05", "R_RACE06", "R_RACE97", 
  "R_HISP", "DRIVER", "EDUC01", "EDUC02", "EDUC03", "EDUC04", 
  "lncommute", "WKFTPT2", "FLEXTIME2", "GT1JBLWK2", 
  "OCCAT01", "OCCAT02", "OCCAT03", "OCCAT04", "OCCAT05", 
  "Telecommute00", "Telecommute01", "Telecommute02", "Telecommute03", "Telecommute04", 
  "deliver00", "deliver01", "deliver02", "deliver03", "deliver04", 
  "PC00", "PC01", "PC02", "PC03", "PC04", 
  "SPHONE00", "SPHONE01", "SPHONE02", "SPHONE03", "SPHONE04", 
  "TAB00", "TAB01", "TAB02", "TAB03", "TAB04", 
  "WEB00", "WEB01", "WEB02", "WEB03", "WEB04", 
  "medcon", "UACE10"
) # "GTscore.std", 

data12 <- data11[, usedvars]
data12 <- data12[complete.cases(data12), ] # drop cases who worked outside of UAs 
names(data12)
nrow(data12)
# map_chr(data12, class)

summary(data12$HHVEHCNT2)
table(data12$RS)
summary(data12$PTUSED2)
summary(data12$NWBMODE2)

ua2016 <- urban_areas(cb=FALSE, year=2016)
ualist <- ua2016@data
ualist <- ualist[, c("UACE10", "NAME10")]
ualist$UACE10 <- as.integer(ualist$UACE10)
# rm("ua2016")

# data05 <- merge(data04, ualist, by="UACE10")
nhtsualist <- as.data.frame(table(data10$UACE10))
colnames(nhtsualist) <- c("UACE10", "cases")
nhtsualist$UACE10 <- as.integer(as.character(nhtsualist$UACE10))
nhtsualist <- left_join(nhtsualist, ualist, by="UACE10")
nhtsualist <- nhtsualist %>% arrange(-cases) 
nrow(nhtsualist)

nhtsualist2 <- 
  nhtsualist %>%
  mutate(
    UACE10 = ifelse(
      UACE10<10000, 
      paste0("0", as.character(UACE10)), 
      as.character(UACE10))
  ) %>% 
  dplyr::select(UACE10, NAME10) %>%
  arrange(UACE10)

temp <- left_join(data12, nhtsualist2, by = "UACE10")
 

for (i in 1:50){
  new <- ifelse(temp$UACE10==nhtsualist2$UACE10[i], 1, 0)
  ## https://stackoverflow.com/questions/45439918/create-names-for-new-columns-within-a-loop
  if (i<10) {
    temp[paste0("UA0", i)] <- new  
  } else {
    temp[paste0("UA", i)] <- new  
  }
}

data13 <- temp

nhts_cases_byua <- data13 %>% group_by(UACE10) %>% summarize(cases = n())

data13 <- data13 %>%
  left_join(nhts_cases_byua, by = "UACE10")

a <- ddply(data13, .(UACE10), summarize, 
           Raw=mean(cases), 
           User=sum(is.na(HOUSEID)==FALSE & RS!=0L), 
           NonUser=sum(is.na(HOUSEID)==FALSE & RS==0L))
a$pctUser <- round(a$User/a$Raw*100, 1)
a$pctNonUser <- round(a$NonUser/a$Raw*100, 1)
a <- merge(a, nhtsualist[, c("UACE10", "NAME10")], by="UACE10")
a[order(-a$User, -a$pctUser), ]

# write_rds(data13, file.path(filepath, "11_Scratch/data13.rds")) # before dropping 
# data13 <- read_rds(file.path(filepath, "11_Scratch/data13.rds"))

## Task 4-2. PSM run binary logit ----

## https://lists.gking.harvard.edu/pipermail/matchit/2017-June/000728.html
## https://www.kdnuggets.com/2018/01/propensity-score-matching-r.html 
## install.packages("stargazer")
## library(stargazer)

data13 <- read_rds(file.path(filepath, "11_Scratch/data13.rds"))
# data13 %>% names() 
data13 %>% .$RS %>% table()
data13 %>% .$RS %>% table() %>% sum()
data13 %>% .$RS %>% summary()

data13$RS <- data13$RS %>% 
  recode(
    "0"=0L, "4"=1L, #"1"=1L, #"2"=1L, "3"=1L, "4"=1L, 
    .default=NA_integer_, .missing = NA_integer_
  ) 
data13 %>% .$RS %>% table()
data13 %>% .$RS %>% table() %>% sum()
data13 %>% .$RS %>% summary()

data13 <- data13 %>%
  filter(is.na(RS) == FALSE) 

# data13 %>% names()
b <- ncol(data13) - 53
xvars <- data13 %>% names() %>% .[c(3:b)]
xvars2 <- c(xvars, "distance")

summary.unmatched <-CreateTableOne(vars=xvars, strata="RS", data=data13, test=TRUE)
# print(summary.unmatched, smd=TRUE)
# ExtractSmd(summary.unmatched)
summary.unmatched.df <- print(summary.unmatched, test=TRUE, smd = TRUE) %>% as.data.frame() 
summary.unmatched.df$varnames <- rownames(summary.unmatched.df)

# write_csv(x = summary.unmatched.df,
#           path = file.path(filepath, "15_Model/round03/round03_01/across01_before_matching.csv"))
# write_csv(x = summary.unmatched.df,
#           path = file.path(filepath, "15_Model/round03/round03_02/across02_before_matching.csv"))
# write_csv(x = summary.unmatched.df,
#           path = file.path(filepath, "15_Model/round03/round03_03/across03_before_matching.csv"))


psm <- glm(
  RS ~ 
    LIF_CYC02 + LIF_CYC03 + LIF_CYC04 + LIF_CYC05 + LIF_CYC06 + 
    WRKCOUNT + DRVRCNT + NUMCHILD + YOUNGCHILD + 
    HHFAMINC02 + HHFAMINC03 + HHFAMINC04 + HHFAMINC05 + HHFAMINC06 + HOMEOWN2 + 
    home.den.pp + home.den.st + home.jobrich + home.oldnbhd + home.sfh + 
    home.pctcoll + home.pctyoung + home.pctxveh + 
    work.den.pp + work.den.st + work.jobrich + work.oldnbhd + work.sfh + 
    work.den.tech + work.den.serv + 
    R_SEX + R_AGE + R_RACE02 + R_RACE03 + R_RACE04 + R_RACE06 + R_RACE97 + # R_RACE01 + 
    R_HISP + DRIVER + EDUC02 + EDUC03 + EDUC04 +  OCCAT01 + OCCAT02 + OCCAT03 + OCCAT05 + # OCCAT04 + 
    lncommute + WKFTPT2 + FLEXTIME2 + GT1JBLWK2 +  
    Telecommute01 + Telecommute02 + Telecommute03 + Telecommute04 + 
    deliver01 + deliver02 + deliver03 + deliver04 + 
    PC01 + PC02 + PC03 + PC04 + 
    SPHONE01 + SPHONE02 + SPHONE03 + SPHONE04 + 
    TAB01 + TAB02 + TAB03 + TAB04 + 
    WEB01 + WEB02 + WEB03 + WEB04 + 
    medcon + UACE10, #+
  #UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  #UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19 + UA20 + 
  #UA21 + UA22 + UA23 + UA24 + UA25 + UA26 + UA27 + UA28 + UA29 + UA30 + 
  #UA31 + UA32 + UA33 + UA34 + UA35 + UA36 + UA37 + UA38 + UA39 + UA40 + 
  #UA41 + UA42 + UA43 + UA44 + UA45 + UA46 + UA47 + UA48 + UA49 + UA50, 
  family=binomial(link="logit"), 
  control = list(maxit = 100), 
  data=data13
)
summary(psm)
summary(psm$fitted.values)
nrow(data13)
hist(psm$fitted.values[data13$RS == 0])
hist(psm$fitted.values[data13$RS == 1])
quarter.stdv <- sd(psm$fitted.values)/4
quarter.stdv
data13$prob <- psm$fitted.values
# data13$prob <- NULL
data13$ID <- paste0(data13$HOUSEID, data13$PERSONID)
rownames(data13) <- paste0(data13$HOUSEID, data13$PERSONID)



## test: a single equation - VO ordered probit, **before** macthing ---- 

# library(MASS)
# 
# test1 <- polr(HHVEHCNT2 ~ RS + WRKCOUNT + NUMCHILD + home.den.pp + UACE10, data = data13)
# summary(test1)
# 
# data13$RS2 <- data13$RS %>% as.factor()
# test2 <- glm(RS2 ~ 
#                HHVEHCNT2 + home.den.pp + deliver01 + deliver02 + deliver03 + deliver04 + UACE10, 
#              family = binomial(link = "probit"), 
#              data = data13)
# summary(test2)
# data13$RS2 <- NULL 
# 
# detach("package:MASS", unload = TRUE)

## stargazer(psm, type="text")
## How to deal with perfect separation in logistic regression?
## https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
## https://stats.stackexchange.com/questions/40876/what-is-the-difference-between-a-link-function-and-a-canonical-link-function
## https://cran.r-project.org/web/packages/logistf/logistf.pdf
## install.packages("logistf")
## library(logistf)
## https://cran.r-project.org/web/packages/MatchIt/vignettes/matchit.pdf



### Task 4-2-1. within-UA matching ----  

mymatching <- function(i){
  tryCatch(
    {
      matchit(
        psm, method="nearest", ratio=1, replace=TRUE, 
        distance="logit", reestimate = TRUE,  caliper=0.25, 
        data=data13[data13$UACE10 == nhtsualist2$UACE10[i], ]) 
    }, 
    error=function(cond){
      message(paste(as.character(i), cond, "/n", sep=" "))
      return(NA)
    }, 
    warning=function(cond){
      message(paste(as.character(i), cond, "/n", sep=" "))
    }
  )
}

myresults <- map(1:50, mymatching) 
ua_remove <- c(1:50)[map_chr(myresults, typeof) != "list"]
ua_subset <- c(1:50)[map_chr(myresults, typeof) == "list"]
ua_subset %>% length()

match.UA <- NULL
match.data.UA <- NULL 
match.data.all <- NULL
for (i in ua_subset) {
  ## https://stats.stackexchange.com/questions/118636/nearest-neighbor-matching-in-r-using-matchit
  set.seed(1000)
  match.UA <- mymatching(i)
  match.data.UA <- match.data(match.UA)
  ## https://r.iq.harvard.edu/docs/matchit/2.4-20/How_Exactly_are.html
  b <- min(match.data.UA[match.data.UA$RS==0, ]$weights)
  match.data.UA$weights2 <- round(ifelse(match.data.UA$RS==0, match.data.UA$weights/b, match.data.UA$weights), digits = 0)
  match.data.all <- rbind(match.data.all, match.data.UA)  ## match.data object 
}
## warnings() 

match.data.within <-  # 5,163 cases from 11 UAs, pretty good 
  match.data.all %>%
  as_tibble() %>%
  mutate(
    weights3 = ifelse(RS==1, 1, distance/(1-distance)*weights2)
  )
# for now, let's use weights2

match.data.within %>% 
  group_by(RS, UACE10) %>% 
  summarize(wtsum = sum(weights2)) %>% 
  spread(key = RS, value = wtsum) %>%
  left_join(nhtsualist2, by = "UACE10")


### Task 4-2-2. across-UA manually matching ----  


# https://cran.r-project.org/web/packages/MatchIt/MatchIt.pdf
# https://r.iq.harvard.edu/docs/matchit/2.4-20/matchit.pdf
# https://stackoverflow.com/questions/42965310/r-matchit-on-7-variables-with-different-seeds
# https://stats.stackexchange.com/questions/86285/random-number-set-seedn-in-r
# This seed number is really really critical! 


library(data.table)

pooled <- data13[c(154, 12, 153)]
treated <- pooled %>% filter(RS == 1)
control <- pooled %>% filter(RS == 0)

find.nearest <-function(x){
  prob.treated <- 
    treated %>%
    filter(ID == x) %>%
    .$prob
  
  ID.matched.control <- 
    control %>%
    mutate(
      diff = abs(prob - prob.treated)
    ) %>%
    filter(diff<quarter.stdv) %>%
    data.table() %>%
    .[ , .SD[which.min(diff)], by = RS] 
  
  return (c(x, ID.matched.control$ID, ID.matched.control$diff))
}

temp00 <- map(treated$ID, find.nearest)
temp01 <- map_dbl(temp00, length) > 1
sum(temp01)/length(temp01)

temp02 <- data.frame( 
  treated = map(temp00[temp01], ~.[1]) %>% unlist(), 
  control = map(temp00[temp01], ~.[2]) %>% unlist(), 
  diff    = map(temp00[temp01], ~.[3]) %>% unlist() %>% as.numeric()
)

# temp02$diff %>% summary()

temp03 <- temp02 %>%
  group_by(control) %>%
  count()
# temp03$n %>% summary() # Max == 10 

temp04 <- data13 %>%
  semi_join(temp02, by = c("ID" = "treated")) 
temp04$n <- 1

temp05 <- data13 %>% 
  inner_join(temp03, by = c("ID" = "control")) 

temp06 <- rbind(temp04, temp05)

temp06 %>% filter(RS == 1) %>% .$n %>% sum()
temp06 %>% filter(RS == 0) %>% .$n %>% sum()
temp06 %>% filter(RS == 0) %>% .$n %>% summary()

temp06 %>% names()

psm <- lm(
  LNRS ~ 
    LIF_CYC02 + LIF_CYC03 + LIF_CYC04 + LIF_CYC05 + LIF_CYC06 + 
    WRKCOUNT + DRVRCNT + NUMCHILD + YOUNGCHILD + 
    HHFAMINC02 + HHFAMINC03 + HHFAMINC04 + HHFAMINC05 + HHFAMINC06 + HOMEOWN2 + 
    home.den.pp + home.den.st + home.jobrich + home.oldnbhd + home.sfh + 
    home.pctcoll + home.pctyoung + home.pctxveh + 
    work.den.pp + work.den.st + work.jobrich + work.oldnbhd + work.sfh + 
    work.den.tech + work.den.serv + 
    R_SEX + R_AGE + R_RACE02 + R_RACE03 + R_RACE04 + R_RACE06 + R_RACE97 + # R_RACE01 + 
    R_HISP + DRIVER + EDUC02 + EDUC03 + EDUC04 +  OCCAT01 + OCCAT02 + OCCAT03 + OCCAT05 + # OCCAT04 + 
    lncommute + WKFTPT2 + FLEXTIME2 + GT1JBLWK2 +  
    Telecommute01 + Telecommute02 + Telecommute03 + Telecommute04 + 
    deliver01 + deliver02 + deliver03 + deliver04 + 
    PC01 + PC02 + PC03 + PC04 + 
    SPHONE01 + SPHONE02 + SPHONE03 + SPHONE04 + 
    TAB01 + TAB02 + TAB03 + TAB04 + 
    WEB01 + WEB02 + WEB03 + WEB04 + 
    medcon + UACE10,
  #family=binomial(link="logit"), 
  #control = list(maxit = 100), 
  data=temp06
)
summary(psm)


nhtsualist3 <- nhtsualist2
nhtsualist3$UAno <- rownames(nhtsualist3) %>% as.integer()

temp06 %>% 
  group_by(UACE10, RS) %>% 
  summarize(n = n()) %>% 
  spread(key = RS, value = n) %>% 
  mutate( n = `0` + `1`) %>% 
  left_join(nhtsualist3, by = "UACE10") %>%
  #filter(n>67.04) %>%
  arrange(n) %>% #by default ascending order 
  # View() # %>%
  # write_csv(file.path(filepath, "15_Model/round05/CountbyUA01.csv"))
  # write_csv(file.path(filepath, "15_Model/round05/CountbyUA02.csv"))
  # write_csv(file.path(filepath, "15_Model/round05/CountbyUA03.csv"))
  # write_csv(file.path(filepath, "15_Model/round05/CountbyUA04.csv"))
  # write_csv(file.path(filepath, "15_Model/round05/CountbyUA05.csv"))

temp06 %>% names()

temp06[, c(31, 7, 12, 8, 11, 13:30, 32:100, 102:151, 154:155)] %>%
# temp06[, c(31, 6, 4, 9, 10, 13:30, 32:100, 102:151, 154:155)] %>%
  # write.csv(file.path(filepath, "15_Model/round05/across01.csv"))
  # write.csv(file.path(filepath, "15_Model/round05/across02.csv"))
  # write.csv(file.path(filepath, "15_Model/round05/across03.csv"))
  # write.csv(file.path(filepath, "15_Model/round05/across04.csv"))
  # write.csv(file.path(filepath, "15_Model/round05/across05.csv"))

  
# write_rds(temp06, file.path(filepath, "15_Model/round05/r5acrs01.rds"))
# write_rds(temp06, file.path(filepath, "15_Model/round05/r5acrs02.rds"))
# write_rds(temp06, file.path(filepath, "15_Model/round05/r5acrs03.rds"))
# write_rds(temp06, file.path(filepath, "15_Model/round05/r5acrs04.rds"))


# r5acrs01 <- read_rds(file.path(filepath, "15_Model/round05/r5acrs01.rds"))
# r5acrs02 <- read_rds(file.path(filepath, "15_Model/round05/r5acrs02.rds"))
# r5acrs03 <- read_rds(file.path(filepath, "15_Model/round05/r5acrs03.rds"))
# r5acrs04 <- read_rds(file.path(filepath, "15_Model/round05/r5acrs04.rds"))


### Task 4-2-3. across-UA R package matching ----  

options("optmatch_max_problem_size" = Inf)
# ?optmatch::fullmatch

# a <- round(rnorm(1)*1000000000, digits = 0)
# print(a) 
# set.seed(a)  

input_ratio <- 1 

t1 <- Sys.time()
match.UA50 <- 
  matchit(
    psm, method="optimal", ratio=input_ratio, replace=TRUE, # in fact with optimal, replace = FALSE
    distance="logit", reestimate = TRUE,  caliper=0.25, # caliper in the stdv unit
    data=data13
  ) 
t2 <- Sys.time()
t2-t1

matched.pairs <- 
  match.UA50$match.matrix %>% 
  as.data.frame()
matched.pairs$treated = rownames(matched.pairs)
matched.pairs$control = matched.pairs[, 1]

pairs.outofcaliper <- 
  matched.pairs %>% 
  as_tibble() %>%
  dplyr::select(treated, control) %>% 
  left_join(data13[, c("ID", "prob")], by = c("treated" = "ID")) %>%
  mutate(prob.treated = prob) %>%
  dplyr::select(-prob) %>%
  left_join(data13[, c("ID", "prob")], by = c("control" = "ID")) %>%
  mutate(prob.control = prob) %>%
  dplyr::select(-prob) %>%
  mutate(diff = abs(prob.treated - prob.control)) %>%
  filter(diff>=quarter.stdv) %>%
  dplyr::select(treated, control, diff)
  
pairs.outofcaliper$diff %>% summary()
  
# sd(match.UA50$distance)/4 # caliper
# sum(psm$fitted.values != match.UA50$distance) # distance is in fact logit/probit probability 

# [digression] check optimal matching performance (begin) -------

test00 <- match.UA50$match.matrix %>% as.data.frame()
test00$treated <- rownames(test00)
head(test00)
names(test00) <- 
  c(#"matched.01", "matched.02", "matched.03", "matched.04", "matched.05", 
    "control", "treated"
    )
test01 <- as_tibble(test00[c(2, 1)])

test02 <- vector("list")
for (i in 1:nrow(test01)){
  test02[[i]] <- 
    data.frame(
    treated = rep(test01[[i, 1]], input_ratio), 
    control = test01[i, 2] %>% as.character()
    )
} 

pairs01 <-
  test02 %>%
  bind_rows() %>%
  as_tibble() %>%
  arrange(treated, control) %>%
  filter(is.na(control) == FALSE) %>%
  group_by(treated, control)

pairs02 <-
  test02 %>%
  bind_rows() %>%
  as_tibble() %>%
  arrange(treated, control) %>%
  filter(is.na(control) == FALSE) %>%
  group_by(treated, control)

n1  <- pairs01 %>% nrow()
n12 <- pairs01 %>% anti_join(pairs02) %>% nrow()
n1
n12 
n12/n1

n2  <- pairs02 %>% nrow()
n21 <- pairs02 %>% anti_join(pairs01) %>% nrow()
n2
n21
n21/n2

test10 <- 
  data13 %>%
  select(HOUSEID, PERSONID, prob) %>%
  mutate(
    id = paste0(HOUSEID, PERSONID)
  ) %>%
  select(id, prob)

pairs01 %>%
  left_join(test10, by = c("treated" = "id")) %>%
  ungroup() %>%
  mutate(treated_prob = prob) %>%
  select(-prob) %>%
  left_join(test10, by = c("control" = "id")) %>%
  mutate(
    control_prob = prob, 
    diff_prob = abs(treated_prob - control_prob), 
    within_caliper = diff_prob < quarter.stdv
  ) %>%
  select(-prob) %>%
  filter(within_caliper != TRUE)
# 187 treated cases out of 3,644 or 5.13% no equivalent within the caliper

# [digression] check optimal matching performance (end) -------

match.UA50.across <-  
  match.data(match.UA50) %>% 
  as_tibble() %>%
  anti_join(pairs.outofcaliper, by = c("ID" = "treated")) %>%
  anti_join(pairs.outofcaliper, by = c("ID" = "control"))

library(MASS)

test1 <- polr(HHVEHCNT2 ~ RS + WRKCOUNT + NUMCHILD + home.den.pp + UACE10, data = match.UA50.across)
summary(test1)

match.UA50.across$RS2 <- match.UA50.across$RS %>% as.factor()

test2 <- glm(RS2 ~ 
               LIF_CYC02 + LIF_CYC03 + LIF_CYC04 + LIF_CYC05 + LIF_CYC06 + 
               WRKCOUNT + NUMCHILD + DRVRCNT + YOUNGCHILD + 
               HHFAMINC02 + HHFAMINC03 + HHFAMINC04 + HHFAMINC05 + HHFAMINC06 + 
               HOMEOWN2 + 
               HHVEHCNT2 + home.den.pp + work.den.pp + 
               R_AGE + EDUC02 + EDUC03 + EDUC04 + OCCAT04 + 
               Telecommute01 + Telecommute02 + Telecommute03 + Telecommute04 + 
               deliver01 + deliver02 + deliver03 + deliver04 + 
               SPHONE01 + SPHONE02 + SPHONE03 + SPHONE04 + 
               UACE10, 
             family = binomial(link = "probit"), 
             data = match.UA50.across)
summary(test2)
# https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
# test2.null <- glm(RS2 ~1,
#              family = binomial(link = "probit"),
#              data = match.UA50.across)
# 1-logLik(test2)/logLik(test2.null)
match.UA50.across$RS2 <- NULL 

detach("package:MASS", unload = TRUE)


install.packages("olsrr", dep = TRUE)
library(olsrr)

test0 <- lm(home.den.pp ~ work.den.pp + DRVRCNT + DRIVER + NUMCHILD +   
              LIF_CYC02 + LIF_CYC03 + LIF_CYC04 + LIF_CYC05 + LIF_CYC06 + 
              HHFAMINC02 + HHFAMINC03 + HHFAMINC04 + HHFAMINC05 + HHFAMINC06 + 
              HOMEOWN2 + # EDUC02 + EDUC03 + EDUC04 + # OCCAT04 + 
              UACE10, 
            data=match.UA50.across)

summary(test0)
ols_vif_tol(test0) %>%
  filter(VIF>=2.5) %>%
  arrange(desc(VIF))
ols_plot_resid_fit_spread(test0)
ols_plot_obs_fit(test0)




match.UA50.across %>%
  names()

nhtsualist3 <- nhtsualist2
nhtsualist3$UAno <- rownames(nhtsualist3) %>% as.integer()

match.UA50.across %>% 
  group_by(UACE10, RS) %>% 
  summarize(n = n()) %>% 
  spread(key = RS, value = n) %>% 
  mutate( n = `0` + `1`) %>% 
  left_join(nhtsualist3, by = "UACE10") %>%
  #filter(n>67.04) %>%
  arrange(n) %>% #by default ascending order 
  View() #%>%
  # write_csv(file.path(filepath, "15_Model/round04/CountbyUA01.csv"))
  # write_csv(file.path(filepath, "15_Model/round04/CountbyUA02.csv"))
  # write_csv(file.path(filepath, "15_Model/round04/CountbyUA03.csv"))
  write_csv(file.path(filepath, "15_Model/round04/CountbyUA04.csv"))

# match.UA50.across %>% write_rds(file.path(filepath, "11_Scratch/match_UA50_across_all.rds"))
# match.UA50.across <- read_rds(file.path(filepath, "11_Scratch/match_UA50_across_all.rds"))


## Task 4-3. before/after matching comparison ----  


### Task 4-3-1. tables: before/after matching comparison ----  

# temp <- read_csv(file.path(filepath, "15_Model/round03/round03_01/across01.csv"))
# temp <- read_csv(file.path(filepath, "15_Model/round03/round03_02/across02.csv"))
# temp <- read_csv(file.path(filepath, "15_Model/round03/round03_03/across03.csv"))
# temp$X1 <- NULL
# 
# temp %>%
#   group_by(RS, HHVEHCNT2) %>%
#   summarize(
#     n = sum(weights2) 
#   )


## Created weighted data object: https://rpubs.com/kaz_yos/matching-weights
# if (!require("plotrix")) install.packages("plotrix", repos = "http://cran.us.r-project.org", dependencies = TRUE)
# if (!require("grid")) install.packages("grid", repos = "http://cran.us.r-project.org", dependencies = TRUE)
# if (!require("Matrix")) install.packages("Matrix", repos = "http://cran.us.r-project.org", dependencies = TRUE)
# if (!require("survey")) install.packages("survey", repos = "http://cran.us.r-project.org", dependencies = TRUE)
# 
# library(plotrix)
# library(grid) 
# library(Matrix)
# library(survey)

# match.data.all.wt <- svydesign(ids = ~ 1, data = temp, weights = ~ weights2)
# ## Weighted table with tableone
# summary.test <- svyCreateTableOne(vars = xvars, strata ="RS", data = match.data.all.wt)
# summary.test.df <- print(summary.test, test=TRUE, smd = TRUE) %>% as.data.frame() 
# summary.test.df$varnames <- rownames(x)

temp <- match.UA50.across
summary.matched <-CreateTableOne(vars=xvars, strata="RS", data=temp, test=TRUE)
# print(summary.unmatched, smd=TRUE)
# ExtractSmd(summary.unmatched)
summary.matched.df <- print(summary.matched, test=TRUE, smd = TRUE) %>% as.data.frame() 
summary.matched.df$varnames <- rownames(summary.matched.df)

# write_csv(x = summary.test.df, 
#           path = file.path(filepath, "15_Model/round03/round03_01/across01_after_matching.csv"))
# write_csv(x = summary.test.df, 
#           path = file.path(filepath, "15_Model/round03/round03_02/across02_after_matching.csv"))
# write_csv(x = summary.test.df, 
#           path = file.path(filepath, "15_Model/round03/round03_03/across03_after_matching.csv"))

## ExtractSmd(summary.test)
## plot(pooled.match.UA, type="jitter")  ## matchit object, the last UA only after the for loop
## plot(pooled.match.UA, type="hist")    ## matchit object, the last UA only after the for loop
## par(mfrow=c(1,1))


### Task 4-3-2. SMD plots: before/after matching comparison ----  

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

if (!require("reshape2")) 
  install.packages("reshape2", repos = "http://cran.us.r-project.org", dependencies = TRUE)
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


jpeg(file.path(filepath, "17_Visualize/02_ICMC2019/balance01.jpg"), 
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




## Task 4-4. Prepare for mplus estimation ----

### Task 4-4-1. Within UA matching ---- 

match.data.within %>% names()
ualist.UA11.within <- match.data.within %>% group_by(UACE10) %>% summarize(n = n())
nhtsualist2$order = rownames(nhtsualist2)
uslist.UA11.within %>%
  left_join(nhtsualist2, by = "UACE10") %>%
  .$order

match.data.within %>% filter(RS==1) %>% .$weights2 %>% table()
match.data.within %>% filter(RS==0) %>% .$weights2 %>% table() 

match.data.within2 <- 
  match.data.within %>% 
  select(-NAME10, -cases, -distance, -weights, -weights3) %>%
  select(-UA01, -UA02, -UA03, -UA04, -UA05, -UA06, -UA07, -UA08, -UA09, -UA10, 
         -UA11, -UA13, -UA14, -UA15, -UA17, -UA18, -UA19, -UA20, 
         -UA22, -UA23, -UA24, -UA26, -UA27, -UA28, -UA30, 
         -UA31, -UA32, -UA33, -UA34, -UA35, -UA36, -UA37, -UA38, -UA39, 
         -UA41, -UA42, -UA43, -UA47, -UA48, -UA49)
names(match.data.within2)

varname.long <- match.data.within2[, c(36, 5, 8, 6:7, 9:35, 37:89)] %>% names() 

varname.short <- c("ybe", "yvo",  "yrh",  "ypt",  "ywb",   
                   "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",  "x8",  "x9",  "x10", 
                   "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", 
                   "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30", 
                   "x31", "x32", "x33", "x34", "x35", "x36", "x37", "x38", "x39", "x40",
                   "x41", "x42", "x43", "x44", "x45", "x46", "x47", "x48", "x49", "x50", 
                   "x51", "x52", "x53", "x54", "x55", "x56", "x57", "x58", "x59", "x60", 
                   "x61", "x62", "x63", "x64", "x65", "x66", "x67", "x68", 
                   "UA",  "UA12", "UA16", "UA21", "UA25", "UA29", "UA40", "UA44", "UA45", 
                   "UA46", "UA50", "wt2") 

varname.dict <- cbind(varname.short, varname.long) 

match.data.within2[, c(36, 5, 8, 6:7, 9:35, 37:89)] %>%
  write.csv(file = "M:/across01_within.csv")



### Task 4-4-2. Across UA matching ---- 

match.UA50.across %>% names()

# match.UA50.across[, c(31, 7, 12, 8, 11, 13:30, 32:100, 102:151, 154)] %>%
#   write.csv(file.path(filepath, "15_Model/round04/across01.csv"))
# match.UA50.across[, c(31, 7, 12, 8, 11, 13:30, 32:100, 102:151, 154)] %>% #names()
#   write.csv(file.path(filepath, "15_Model/round04/across02.csv")) # UA=23 missing
# match.UA50.across[, c(31, 7, 12, 8, 11, 13:30, 32:100, 102:151, 154)] %>%
#   write.csv(file.path(filepath, "15_Model/round04/across03.csv"))
match.UA50.across[, c(31, 7, 12, 8, 11, 13:30, 32:100, 102:151, 154)] %>%
  write.csv(file.path(filepath, "15_Model/round04/across04.csv"))







# varname.long <- match.UA50.across[, c(27, 5, 8, 6:7, 9:26, 28:96, 98:147, 151)] %>% names() 
varname.long <- match.UA50.across[, c(31, 7, 12, 8, 11, 13:30, 32:99, 102:151)] %>% names 

varname.short1 <- c("ybe", "yvo",  "yrh",  "ypt",  "ywb",   
                   "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",  "x8",  "x9",  "x10", 
                   "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", 
                   "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30", 
                   "x31", "x32", "x33", "x34", "x35", "x36", "x37", "x38", "x39", "x40",
                   "x41", "x42", "x43", "x44", "x45", "x46", "x47", "x48", "x49", "x50", 
                   "x51", "x52", "x53", "x54", "x55", "x56", "x57", "x58", "x59", "x60", 
                   "x61", "x62", "x63", "x64", "x65", "x66", "x67", "x68", "x69", "x70", 
                   "x71", "x72", "x73", "x74", "x75", "x76", "x77", "x78", "x79", "x80", 
                   "x81", "x82", "x83", "x84", "x85", "x86", 
                   "UA") 
# varname.short2 <- match.UA50.across[, c(98:147, 151)] %>% names() 
varname.short2 <- match.UA50.across[, c(102:151, 155)] %>% names() 

varname.short3 <- c(varname.short1, varname.short2) 

# cbind(varname.short3, varname.long) %>% 
#   write.csv(file.path(filepath, "15_Model/round03_01/varname.csv"))
# cbind(varname.short3, varname.long) %>% 
#   write.csv(file.path(filepath, "15_Model/round03_02/varname.csv"))
# cbind(varname.short3, varname.long) %>% 
#   write.csv(file.path(filepath, "15_Model/round03_03/varname.csv"))
cbind(varname.short3, varname.long) %>%
  write.csv(file.path(filepath, "15_Model/round03_04/varname.csv"))








# matching.cov.list <- c("HOUSEID", "PERSONID", "# RIDESHARE", "# HHVEHCNT", "O HHVENCHT", "LN HHVENCNT", 
#                        "O PT", "O Walk/Bike", "Yes RIDESHARE",
#                        "one adult, no children", "2+ adults, no children", "one adult, youngest child 0-5", "2+ adults, youngest child 0-5", 
#                        "one adult, youngest child 6-15", "2+ adults, youngest child 6-15", "one adult, youngest child 16-21", "2+ adults, youngest child 16-21", "one adult, retired, no children", 
#                        "2+adults, retired, no children", "# HH worker", "# HH driver", "# child", "Yes child 0-4", 
#                        "Less than $10,000", "$10,000-$14,999", "$15,000-$24,999", "$25,000-$34,999", "$35,000-$49,999", 
#                        "$50,000-$74,999", "$75,000-$99,999", "$100,000-$124,999", "$125,000-$149,999", "$150,000-$199,999", 
#                        "$200,000 or more", "Home owner", "Home density", "Work density", "% with collge", 
#                        "% 25-34", "% HH with no cars", "Tech job density", "Service job density", "Female", 
#                        "Age", "White", "Black", "Asian", "American Indian", 
#                        "Pacific Islander", "Multiple races", "Some other race", "Hispanic", "Driver", 
#                        "Less than HS", "Highschool", "Some college", "Bachelor's", "Graduate",
#                        "Sales or service", "Clerical/admin", "Manufacturing", "Professional", "Something else", 
#                        "Telecommute 0", "Telecommute 1-3", "Telecommute 4-7", "Telecommute 8-11", "Telecommute 12+", 
#                        "Medical condition", "# online delivery", "Google Trend", "UACE10",  
#                        "UA01", "UA02", "UA03", "UA04", "UA05", "UA06", "UA07", "UA08", "UA09", "UA10", 
#                        "UA11", "UA12", "UA13", "UA14", "UA15", "UA16", "UA17", "UA18", "UA19", "UA20", 
#                        "UA21", "UA22", "UA23", "UA24", "UA25", "UA26", "UA27", "UA28", "UA29", "UA30", 
#                        "UA31", "UA32", "UA33", "UA34", "UA35", "UA36", "UA37", "UA38", "UA39", "UA40", 
#                        "UA41", "UA42", "UA43", "UA44", "UA45", "UA46", "UA47", "UA48", "UA49", "UA50",
#                        "pscore", "WT1", "WT2", "WT3")
# length(matching.cov.list)-54
# b <- cbind(a, matching.cov.list)
# b[3:9, ]
# b[10:72, ]
# b
# 
# table(data13$deliver) # categorical variable - should be divided for each level 
# 
# 
# source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
# crosstab(data13, col.vars="NWBMODE2", row.vars=c("EDUC03", "EDUC04", "EDUC05"), type=
#            "f", addmargins=FALSE)
# 
# nhtsualist$uaorder <- rownames(nhtsualist)
# 
# ualist <- ddply(data13, .(UACE10), summarize, Count=sum(is.na(UACE10)==FALSE))
# merge(ualist, nhtsualist, by=c("UACE10"))

## boxplot(as.numeric(data13$NWBMODE2)~UA44, data=data13)
## summary(as.numeric(as.character(data13[data13$UA44==1 & data13$RS==1, ]$NWBMODE2)))
## summary(as.numeric(as.character(data13[data13$UA44==1 & data13$RS==0, ]$NWBMODE2)))


