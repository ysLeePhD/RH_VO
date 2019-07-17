
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



# Step 4. Estimate binary logit/probit for balancing the final sample -----

## Task 4-1. PSM data prep ----

# if I don't start from scratch 
data10 <- read_rds(file.path(filepath, "11_Scratch/data10.rds"))

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
        data10$RIDESHARE<8, 2L, 3L # less than twice a week vs. at least twice a week 
      )
    )
  )
)

# table(data10$RS)
# summary(data10$RS) # 32 NA cases 

# a <- data10$RS %>% table() %>% as.data.frame() 
# a$Freq %>% sum()
# b <- nrow(data10[data10$RS >=0, ])
# a$pct <- (a[,2]/b*100) %>% round(digits = 1)
# a 

# choose *two* user types 
data11 <- data10 %>% filter(data10$RS==0L | data10$RS==1L)
# table(data11$RS) %>% sum()

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
  "HOUSEID", "PERSONID", "RIDESHARE", "HHVEHCNT", "HHVEHCNT2", "PTUSED2", "NWBMODE2", "RS", 
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
  select(UACE10, NAME10) %>%
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
           User=sum(is.na(HOUSEID)==FALSE & RS==1), 
           NonUser=sum(is.na(HOUSEID)==FALSE & RS==0))
a$pctUser <- round(a$User/a$Raw*100, 1)
a$pctNonUser <- round(a$NonUser/a$Raw*100, 1)
a <- merge(a, nhtsualist[, c("UACE10", "NAME10")], by="UACE10")
a[order(-a$User, -a$pctUser), ]



## Task 4-2. PSM run binary logit ----

## https://lists.gking.harvard.edu/pipermail/matchit/2017-June/000728.html
## https://www.kdnuggets.com/2018/01/propensity-score-matching-r.html 
## install.packages("stargazer")
## library(stargazer)

names(data13)
# map(data13, summary)

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
    medcon, #+
  #UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
  #UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19 + UA20 + 
  #UA21 + UA22 + UA23 + UA24 + UA25 + UA26 + UA27 + UA28 + UA29 + UA30 + 
  #UA31 + UA32 + UA33 + UA34 + UA35 + UA36 + UA37 + UA38 + UA39 + UA40 + 
  #UA41 + UA42 + UA43 + UA44 + UA45 + UA46 + UA47 + UA48 + UA49 + UA50, 
  family=binomial(link="probit"), 
  control = list(maxit = 100), 
  data=data13
)
summary(psm)
## stargazer(psm, type="text")

b <- ncol(data13) - 53
xvars <- data13 %>% names() %>% .[c(3:b)]
xvars2 <- c(xvars, "distance")

summary.unmatched <-CreateTableOne(vars=xvars, strata="RS", data=data13, test=TRUE)
print(summary.unmatched, smd=TRUE)
## ExtractSmd(summary.unmatched)

## How to deal with perfect separation in logistic regression?
## https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression
## https://stats.stackexchange.com/questions/40876/what-is-the-difference-between-a-link-function-and-a-canonical-link-function
## https://cran.r-project.org/web/packages/logistf/logistf.pdf
## install.packages("logistf")
## library(logistf)

## https://cran.r-project.org/web/packages/MatchIt/vignettes/matchit.pdf


# Task 4-2-1. within-UA matching ----  

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


# Task 4-2-2. across-UA matching ----  

match.UA50 <- 
  matchit(
    psm, method="nearest", ratio=1, replace=TRUE, 
    distance="logit", reestimate = TRUE,  caliper=0.25, 
    data=data13
  ) 

b <- match.data(match.UA50) %>%
  filter(RS==0) %>%
  .$weights %>% 
  min() 

match.UA50.across <- # 6,704 cases from 50 UAs
  match.data(match.UA50) %>% 
  as_tibble() %>% 
  mutate(
    weights2 = round(ifelse(RS==0, weights/b, weights), digits = 0), 
    weights3 = ifelse(RS==0, distance/(1-distance), 1) 
  )  

match.UA50.across %>% 
  group_by(UACE10, RS) %>% 
  summarize(n = n()) %>% 
  spread(key = RS, value = n) %>% 
  left_join(nhtsualist2, by = "UACE10") %>%
  View()




# Task 4-3. before/after matching comparison ----  

if (!require("plotrix")) install.packages("plotrix", repos = "http://cran.us.r-project.org", dependencies = TRUE)
library(plotrix)

temp <- match.UA50.across[, c(1:78, 131:134)]
temp$LNVEH    <- log(temp$HHVEHCNT+1) 
## weighted.hist(temp[temp$RS==1, ]$LNVEH, temp[temp$RS==1, ]$weights3)
## weighted.hist(temp[temp$RS==0, ]$LNVEH, temp[temp$RS==0, ]$weights3)

hist(temp$HHVEHCNT)
summary(temp$weights)
summary(temp$weights2)
summary(temp$weights3)
summary(temp$distance)

nrow(temp[temp$RS==1, ])
sum(temp[temp$RS==1, ]$weights2)
nrow(temp[temp$RS==0, ])
sum(temp[temp$RS==0, ]$weights2)

## Created weighted data object: https://rpubs.com/kaz_yos/matching-weights

if (!require("grid")) install.packages("grid", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("Matrix")) install.packages("Matrix", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("survey")) install.packages("survey", repos = "http://cran.us.r-project.org", dependencies = TRUE)
library(grid) 
library(Matrix)
library(survey)

names(temp)[map_chr(temp, class) == "numeric"]

temp$HHVEHCNT2 <- as.numeric(as.character(temp$HHVEHCNT2))
temp$PTUSED2   <- as.numeric(as.character(temp$PTUSED2))
temp$NWBMODE2  <- as.numeric(as.character(temp$NWBMODE2))
temp$HOMEOWN2  <- as.numeric(as.character(temp$HOMEOWN2))
temp$R_SEX     <- as.numeric(as.character(temp$R_SEX))
temp$R_HISP    <- as.numeric(as.character(temp$R_HISP))

match.data.all.wt <- svydesign(ids = ~ 1, data = temp, weights = ~ weights2)

## Weighted table with tableone
summary.test <- svyCreateTableOne(vars = xvars, strata ="RS", data = match.data.all.wt)
print(summary.test, test=TRUE, smd = TRUE)
# write.csv(print(summary.test, test=TRUE, smd = TRUE), file="M:/Uber_NHTS/31_Conference/AfterMatching03.csv")
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
match.UA50.across %>% select(-NAME10, -cases, -distance, -weights, -weights3) %>% names()

match.UA50.across[, c(5, 8, 6:7)] %>% map(table)
match.UA50.across[, c(36, 5, 8, 6:7, 9:35, 37:78, 131:134)] %>% names()
match.UA50.across[, c(36, 5, 8, 6:7, 9:35, 37:78, 131:134)] %>% .[, c(48:54)] %>% map(table)


%>% 
  write.csv(file.path(filepath, "15_Model/round02/across01.csv"))

varname.long <- match.UA50.across[, c(36, 5, 8, 6:7, 9:35, 37:78, 131:134)] %>% names() 

varname.short <- c("ybe", "yvo",  "yrh",  "ypt",  "ywb",   
                   "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",  "x8",  "x9",  "x10", 
                   "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", 
                   "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "x29", "x30", 
                   "x31", "x32", "x33", "x34", "x35", "x36", "x37", "x38", "x39", "x40",
                   "x41", "x42", "x43", "x44", "x45", "x46", "x47", "x48", "x49", "x50", 
                   "x51", "x52", "x53", "x54", "x55", "x56", "x57", "x58", "x59", "x60", 
                   "x61", "x62", "x63", "x64", "x65", "x66", "x67", "x68", 
                   "UA",  "ps", "WT1", "WT2", "WT3") 

varname.dict <- cbind(varname.short, varname.long) 








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


