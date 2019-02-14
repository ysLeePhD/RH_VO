

if (!require("foreign")) install.packages("foreign", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("plyr")) install.packages("plyr", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tidyverse")) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("sf")) install.packages("sf", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tigris")) install.packages("tigris", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tidycensus")) install.packages("tidycensus", repos = "http://cran.us.r-project.org", dependencies = TRUE)

if (!require("devtools")) install.packages("devtools")
devtools::install_github("jamgreen/lehdr")

if (!require("psych")) install.packages("psych", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("tableone")) install.packages("tableone", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if (!require("MatchIt")) install.packages("MatchIt", repos = "http://cran.us.r-project.org", dependencies = TRUE)

library(foreign)
library(plyr)
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)

library(lehdr)

library(psych)
library(tableone)
library(MatchIt)

setwd("C:/Users/ylee366/Dropbox (GaTech)/3a_ResearchCEE/09_Uber_NHTS/13_Process/2017NHTS-Uber-VO")
options(stringsAsFactors = FALSE)
# check the integer max value 
#.Machine$integer.max 


#1. Read input files 

hh <- read.csv('M:/Uber_NHTS/03_NHTS/Csv/hhpub.csv')
## boxplot(HHVEHCNT ~ LIF_CYC, data=hh)
per <- read.csv('M:/Uber_NHTS/03_NHTS/Csv/perpub.csv')
hh$NUMCHILD <- ifelse(hh$HHSIZE>=hh$NUMADLT, hh$HHSIZE-hh$NUMADLT, NA)

#per$Under18 <- ifelse(per$R_AGE<18, 1, 0)
#temp1 <- per[, c("HOUSEID", "Under18")]
#temp1$HOUSEID <- as.character(temp1$HOUSEID)
#temp2 <- ddply(temp1, .(HOUSEID), summarize, tot=sum(Under18))
#temp2$HOUSEID <- as.integer(temp2$HOUSEID)
#hh <- merge(hh, temp2, by="HOUSEID")
#colnames(hh)[58] <- "NCHILD"

home <- read.csv('M:/Uber_NHTS/03_NHTS/GEO/hhctbg.csv') # restricted-use home block group file 
work <- read.csv('M:/Uber_NHTS/03_NHTS/GEO/workct.csv') # restricted-use work/school tract file

bg <- read.dbf('M:/Uber_NHTS/06_Shapefile/Blkgrp/Blkgrp_50topUA.dbf') # from a preprocessed shapefile 
colnames(bg) <- c("HHSTFIPS","HHCNTYFP","HHCT","HHBG", "GEOID", 
                  "ALAND", "AWATER", "UACE10", "WGS84x","WGS84y")
bgd <- bg[, c("GEOID", "ALAND", "UACE10")]
bgd$GEOID <- as.character(bgd$GEOID)
bgd$HHSTFIPS <- as.integer(substr(bgd$GEOID, 1, 2))
bgd$HHCNTYFP <- as.integer(substr(bgd$GEOID, 3, 5))
bgd$HHCT <- as.integer(substr(bgd$GEOID, 6, 11))  
bgd$HHBG <- as.integer(substr(bgd$GEOID, 12, 12))
bgd <- bgd[, c("HHSTFIPS","HHCNTYFP","HHCT","HHBG", "GEOID", 
               "ALAND", "UACE10")]
bgd$tr <- substr(bgd$GEOID, 1, 11)
trd <- ddply(bgd, "tr", numcolwise(mean))[, c("tr", "UACE10")]
colnames(trd)[1] <- "GEOID"
write.csv(trd, "M:/Uber_NHTS/11_Scratch/trd.csv")

#2. Merge datasets  
hhs <- hh[, c("HOUSEID", "TDAYDATE", "HH_RACE",  "HH_HISP", "LIF_CYC", "HHSIZE", "NUMADLT", "NUMCHILD",
              "YOUNGCHILD", "WRKCOUNT", "DRVRCNT", "HHFAMINC","HOMEOWN","HHVEHCNT",  
              "PC", "SPHONE", "TAB", "WEBUSE17", "TAXI", "SCRESP", "WTHHFIN")] #SCRESP = PERSONID
pers <- per[, c("HOUSEID", "PERSONID", "R_RELAT", "R_SEX", "R_AGE", "R_HISP", "R_RACE",  
                "DRIVER", "EDUC", "DISTTOWK17", "NOCONG", "SCHTYP", "SCHTRN1", "WORKER", "WKFTPT", "WRKTRANS",
                "FLEXTIME", "GT1JBLWK", "OCCAT", "WKRMHM", "WKFMHMXX", "MEDCOND", 
                "NBIKETRP", "BIKE4EX", "NWALKTRP", "WALK4EX", "PTUSED", "RIDESHARE", "DELIVER", "YEARMILE", 
                "WTPERFIN")] 
hhs <- merge(hhs, home, by="HOUSEID")
pers <- merge(pers, work[, c("HOUSEID", "PERSONID", "WKSTFIPS", "WKCNFIPS", "WORKCT")], 
              by=c("HOUSEID", "PERSONID"))
hhp <- merge(hhs, pers, by=c("HOUSEID"))
hhpbg <- merge(hhp, bgd, by=c("HHSTFIPS","HHCNTYFP","HHCT","HHBG"))
rm("home", "work", "hh", "per", "hhs", "pers", "hhp", "bg")


#table(hhpbg[hhpbg$YEARMILE<0, ]$YEARMILE)   #-88 I don't know -77 I prefer not to answer -9 not ascertained -1 appropriate skip
#table(hhpbg[hhpbg$YEARMILE==-1, ]$DRIVER)   #-1 appropriate skip 2 non-driver
#table(hhpbg[hhpbg$DRIVER==-1, ]$R_AGE)      # all cases under 15 



#3. Prepare outcome/covariates for PSM models 


#3.1. Append land-use variables using tidycensus 


#https://cran.r-project.org/web/packages/tidycensus/tidycensus.pdf
census_api_key("3b1d8912e33aa2d4c01bf1abc84729cfeb7cd6cd", install = TRUE, overwrite=TRUE)
readRenviron("~/.Renviron") # First time, reload your environment so you can use the key without restarting R.
Sys.getenv("CENSUS_API_KEY") # You can check it with:

#hhsub <- hhpbg[, c("HOUSEID", "GEOID")]
#hhsub$GEOID <- substr(hhsub$GEOID, 1, 11)
#rm("hhsub")

var.acs <- load_variables(2017, "acs5", cache=TRUE)

ST <- c("AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "IL", 
        "IN", "KS", "KY", "LA", "MD", "MA", "MI", "MN", "MS", "MO",
        "NV", "NH", "NJ", "NY", "NC", "OH", "OK", "OR", "PA", "RI",
        "SC", "TN", "TX", "UT", "VA", "WA", "WI")
varlist <- c("B25024_001", "B25024_002", "B25024_003", "B25018_001", 
             "B01001_001", "B08201_001", "B08201_006", "B08006_001", "B08006_008") 
be_st <- trd 
beall <- NULL 
for(i in 1:37) {
  be_st <- trd
  for (j in 1:9) {
    temp <- NULL
    temp <- get_acs(geography = "tract", state = ST[i], variables = varlist[j], year=2016, 
                    cache_table = TRUE)
    temp <- temp[, c("GEOID", "estimate")]
    colnames(temp) <- c("GEOID", paste(varlist[j], "est", sep=""))  
    be_st <- merge(be_st, temp, by="GEOID")
  }
  beall <- rbind(beall, be_st)
}
## View(beall)


beall$pctsfh <- ifelse(beall$B25024_001est>0, (beall$B25024_002est + beall$B25024_003est)/beall$B25024_001est *100, 0)
beall$mednrm <- beall$B25018_001est  
beall$pcth4v <- ifelse(beall$B08201_001est>0, beall$B08201_006est/beall$B08201_001est *100, 0)
beall$pctctr <- ifelse(beall$B08006_001est>0, beall$B08006_008est/beall$B08006_001est *100, 0)
beall2 <- beall[, c("GEOID", "UACE10", "pctsfh", "mednrm", "B01001_001est", "pcth4v", "pctctr")]  

tigris_cache_dir("M:/Uber_NHTS/05_Census/tigris")
readRenviron('~/.Renviron')
options(tigris_use_cache=TRUE)

ALAND <- NULL 
for(i in 1:37){
  temp <- tracts(ST[i], year=2016)
  temp2 <- temp@data[, c("GEOID", "ALAND")]
  ALAND <- rbind(ALAND, temp2)
}
temp3 <- merge(beall2, ALAND, by="GEOID")
temp3$popden <- ifelse(temp3$ALAND != "0", temp3$B01001_001est*1000/as.numeric(temp3$ALAND), 0)

beall3 <- temp3[, c("GEOID", "UACE10", "pctsfh", "mednrm", "popden", "pcth4v", "pctctr")]
beall3a <- ddply(beall3,  c("UACE10"), transform, pctsfh.std=scale(pctsfh))
beall3b <- ddply(beall3a, c("UACE10"), transform, mednrm.std=scale(mednrm))
beall3c <- ddply(beall3b, c("UACE10"), transform, popden.std=scale(popden))
beall3d <- ddply(beall3c, c("UACE10"), transform, pcth4v.std=scale(pcth4v))
beall3e <- ddply(beall3d, c("UACE10"), transform, pctctr.std=scale(pctctr))
beall4 <- beall3e[, c("GEOID", "UACE10", "pctsfh.std", "mednrm.std", "popden.std", "pcth4v.std", "pctctr.std")]

rm("temp", "temp2", "temp3")
rm("beall", "be_st", "beall2", "beall3", "beall3a", "beall3b", "beall3c", "beall3d", "beall3e")


#3.2. Run exploratory factor analysis on five LU measures, standardized by UA 

#https://cran.r-project.org/web/packages/psych/psych.pdf
#http://personality-project.org/r/psych/HowTo/factor.pdf

beall5 <- beall4[!(is.na(beall4$pctsfh.std)==TRUE | is.na(beall4$mednrm.std)==TRUE | is.na(beall4$popden.std)==TRUE | 
                   is.na(beall4$pcth4v.std)==TRUE | is.na(beall4$pctctr.std)==TRUE ), 
                 c("GEOID", "pctsfh.std", "mednrm.std", "popden.std", "pcth4v.std", "pctctr.std")]

fit1 <- fa(beall5[, 2:6], nfactors=1, rotate = "oblimin", scores="Bartlett", fm="pa", SMC=TRUE, covar=FALSE)
fit2 <- fa(beall5[, 2:6], nfactors=1, rotate = "oblimin", scores="Bartlett", fm="ml", SMC=TRUE, covar=FALSE)
fit2
beall5$den <- as.numeric(fit2$scores)*(-1)
beall6 <- beall5[, c("GEOID", "den")]
beall7 <- merge(beall4, beall6, by="GEOID")

rm("beall4", "beall5", "beall6")
rm("fit1", "fit2")
 
write.csv(beall7, "M:/Uber_NHTS/11_Scratch/beall7.csv")


beall7 <- read.csv("M:/Uber_NHTS/11_Scratch/beall7.csv")
beall7[, 1] <- NULL 
beall7$GEOID <- as.character(beall7$GEOID)
beall7$GEOID <- ifelse(nchar(beall7$GEOID)==10, paste("0", beall7$GEOID, sep=""), beall7$GEOID)


#3.3. Append instrument variables for the use of Uber, which captures *exogenous* variation in Uber supply  


# tract-level variables from 2008-2012 ACS 5-year estimates (mid-year 2010) 
# % college graduates, % young adults (25-34), % individuals without any vehicle
# LEHD 2010 
# count of jobs in food services, entertainment/arts, etc 
# standardize by UA


varlist2 <- c("B06009_001", "B06009_005", "B06009_006", "B06001_001", "B06001_005", 
              "B08014_001", "B08014_002") 
iv_st <- trd 
ivall <- NULL 
for (i in 1:37) {
  iv_st <- trd 
  for (j in 1:7) {
    temp <- NULL
    temp <- get_acs(geography = "tract", state = ST[i], variables = varlist2[j], year=2016, 
                    cache_table = TRUE)
    #Sys.sleep(0.75)
    temp <- temp[, c("GEOID", "estimate")]
    colnames(temp) <- c("GEOID", paste(varlist2[j], "est", sep=""))  
    iv_st <- merge(iv_st, temp, by="GEOID")
  }
  ivall <- rbind(ivall, iv_st)
}

ivall$pctcoll <- ifelse(ivall$B06009_001est>0, (ivall$B06009_005est + ivall$B06009_006est)/ivall$B06009_001est *100, 0)
ivall$pctyoung <- ifelse(ivall$B06001_001est>0, ivall$B06001_005est/ivall$B06001_001est*100, 0) 
ivall$pctxveh <- ifelse(ivall$B08014_001est>0, ivall$B08014_002est/ivall$B08014_001est *100, 0)
ivall <- ivall[, c("GEOID", "UACE10", "pctcoll", "pctyoung", "pctxveh")]  
head(ivall)

ivjob <- NULL 
for(i in 1:37) {
  temp <- grab_lodes(state=ST[i], year=2011, lodes_type="wac", agg_geo="tract")
  temp <- temp[, c("w_tract", "CNS09", "CNS12", "CNS17", "CNS18")]  
  temp <- as.data.frame(temp)
  ivjob <- rbind(ivjob, temp)
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


ivall4 <- read.csv("M:/Uber_NHTS/11_Scratch/ivall4.csv")
ivall4[, 1] <- NULL
ivall4$GEOID <- as.character(ivall4$GEOID)
ivall4$GEOID <- ifelse(nchar(ivall4$GEOID)==10, paste("0", ivall4$GEOID, sep=""), ivall4$GEOID)


#3.4. Scrape walkscore.com: using a different file of scripts 


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


#3.4. Clean socioeconomic/demographic variables 


#3.4.1. Outcome variables: data01
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



#3.4.2. Household level covariates
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




#3.4.3. Individual level covariates: "non-workers removed" 
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



#3.4.4. Merge three dataframes 

colnames(data01)
colnames(data06)
colnames(data09)

temp1 <- merge(data01, data06, by=c("HOUSEID", "PERSONID"))
data10 <- merge(temp1, data09, by=c("HOUSEID", "PERSONID"))
rm("temp1")
nrow(data10)



#4. PSM estimation 
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

#4.2. PSM estimation 

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
