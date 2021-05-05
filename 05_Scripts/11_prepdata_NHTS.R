
source("05_Scripts/.setup.R")

## Task 1-2. Read in and select variables of NHTS restricted-use data files ----

hh <- read_csv(paste0(filepath, "/03_NHTS/Csv/hhpub.csv"))
hh$NUMCHILD <- ifelse(hh$HHSIZE>=hh$NUMADLT, hh$HHSIZE-hh$NUMADLT, 0)
#nrow(hh)

per <- read_csv(paste0(filepath, "/03_NHTS/Csv/perpub.csv"))
#nrow(per)

home <- read_csv('F:/3 Study/0 medium term backup/2017_NHTS_US/GEO/hhctbg.csv') # restricted-use home block group file 
work <- read_csv('F:/3 Study/0 medium term backup/2017_NHTS_US/GEO/workct.csv') # restricted-use work/school tract file

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

hhp <- left_join(hh3, per3, by=c("HOUSEID"))

bgd <- read_rds(paste0(filepath, "/11_Scratch/bgd.rds"))
hhpbg <- left_join(hhp, bgd, by=c("HHSTFIPS","HHCNTYFP","HHCT","HHBG")) %>%
  filter(is.na(UACE10) == FALSE) %>% # residents within the top 50 UA
  filter(nchar(WORKCT) == 6) # workers 

hhpbg %>% group_by(HHSTFIPS, HHCNTYFP, HHCT, HHBG) %>% summarize() %>% nrow() # 16,916 unique block groups
hhpbg %>% select(tr) %>% n_distinct() # 11,424 unique tracts

# check annual miles driven & driver status 
# table(hhpbg[hhpbg$YEARMILE<0, ]$YEARMILE)   #-88 I don't know -77 I prefer not to answer -9 not ascertained -1 appropriate skip
# table(hhpbg[hhpbg$YEARMILE==-1, ]$DRIVER)   #-1 appropriate skip 2 non-driver
# table(hhpbg[hhpbg$DRIVER==-1, ]$R_AGE)      # all cases under 15

# a <- hhpbg %>% nrow()
# b <- hhpbg %>% filter(HHVEHCNT > 0 & DRIVER == "01" & YEARMILE < -9) %>% nrow()
# round(b/a * 100, digits = 1)

# Step 3. Prepare HH/PERSON/individual-level IV variables -------------------------------------

## Task 3-1. Outcome variables: data01 ----

# n of vehicles, transit trips per week, non-motorized trips per week 
# treatment indicator 

names(hhpbg)
quantile(hhpbg[hhpbg$PTUSED>0, ]$PTUSED, c(0.25, 0.5, 0.75, 0.9, 0.95, 0.99)) 
# in the past 30 days, how many days you used public transportation? 
# 0-3, 4-7, 8-11, 12+ (4 levels)
nrow(hhpbg[hhpbg$PTUSED==0, ])/nrow(hhpbg[hhpbg$PTUSED>=0, ])*100
nrow(hhpbg[hhpbg$PTUSED>=1 & hhpbg$PTUSED<=3, ])/nrow(hhpbg[hhpbg$PTUSED>=0, ])*100  # Less than once a week 
nrow(hhpbg[hhpbg$PTUSED>=4 & hhpbg$PTUSED<=11, ])/nrow(hhpbg[hhpbg$PTUSED>=0, ])*100 # 1-2 times a week 
nrow(hhpbg[hhpbg$PTUSED>=12, ])/nrow(hhpbg[hhpbg$PTUSED>=0, ])*100                   # 3 or more times a week 

hhpbg$PTUSED2 <- ifelse(hhpbg$PTUSED>=0,  0, NA) # no use
hhpbg$PTUSED2 <- ifelse(hhpbg$PTUSED>=1,  1, hhpbg$PTUSED2) # 1-3 times (<1 times/week)
hhpbg$PTUSED2 <- ifelse(hhpbg$PTUSED>=4,  2, hhpbg$PTUSED2) # 4-11 times (1-2 times/week)
hhpbg$PTUSED2 <- ifelse(hhpbg$PTUSED>=12, 3, hhpbg$PTUSED2) # 12 or more times (3 or more times/week)
table(hhpbg$PTUSED2)
summary(hhpbg$PTUSED2) # NA 49 cases out of 37,118 no reporting 
hhpbg$PTUSED2 <- as.factor(hhpbg$PTUSED2)

hhpbg$LNPTUSED <- ifelse(hhpbg$PTUSED>=0, log(hhpbg$PTUSED+1), NA)
hhpbg$LNPTUSED %>% is.na() %>% sum() # NA 49 cases out of 37,118 no reporting 
hhpbg$LNPTUSED %>% hist()

summary(hhpbg$NBIKETRP)
table(hhpbg$NBIKETRP) 
table(hhpbg$BIKE4EX) 

hhpbg$NBIKEMODE <- NA
hhpbg$NBIKEMODE <- ifelse(hhpbg$NBIKETRP>=0 & hhpbg$BIKE4EX>=0 & hhpbg$NBIKETRP>=hhpbg$BIKE4EX, hhpbg$NBIKETRP-hhpbg$BIKE4EX, NA)
hhpbg$NBIKEMODE <- ifelse(hhpbg$NBIKETRP>=0 & hhpbg$BIKE4EX==-1, hhpbg$NBIKETRP, hhpbg$NBIKEMODE)
summary(hhpbg$NBIKEMODE) # NA 51 cases out of 37,118 no reporting 
table(hhpbg$NBIKEMODE)
# in the past 7 days, how many times did you ride a bicycle? 
# 0-3, 4-7, 8-11, 12+ (4 levels)

hhpbg$NWALKMODE <- NA
hhpbg$NWALKMODE <- ifelse(hhpbg$NWALKTRP>=0 & hhpbg$WALK4EX>=0 & hhpbg$NWALKTRP>=hhpbg$WALK4EX, hhpbg$NWALKTRP-hhpbg$WALK4EX, NA)
hhpbg$NWALKMODE <- ifelse(hhpbg$NWALKTRP>=0 & hhpbg$WALK4EX==-1, hhpbg$NWALKTRP, hhpbg$NWALKMODE)
summary(hhpbg$NWALKMODE) # NA 255 cases out of 37,118 no reporting 
table(hhpbg$NWALKMODE)
# In the past 7 days, how many times did [$YOU1] take a walk outside including walks 
# to exercise, go somewhere, or to walk the dog (e.g., walk to a friend’s house, 
# walk around the neighborhood, walk to the store, etc.)?

hhpbg$LNBIKEMODE <- ifelse(hhpbg$NBIKEMODE>=0, log(hhpbg$NBIKEMODE+1), NA)
hhpbg$LNWALKMODE <- ifelse(hhpbg$NWALKMODE>=0, log(hhpbg$NWALKMODE+1), NA)

hhpbg$NWBMODE <- hhpbg$NBIKEMODE + hhpbg$NWALKMODE
summary(hhpbg$NWBMODE) # NA 279 cases out of 37,118 
table(hhpbg$NWBMODE)
hhpbg$LNWBMODE <- ifelse(hhpbg$NWBMODE>=0, log(hhpbg$NWBMODE+1), NA)
hist(hhpbg$LNWBMODE)

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
summary(hhpbg$NWBMODE2) # NA 279 cases out of 37,118 
hhpbg$NWBMODE2 <- as.factor(hhpbg$NWBMODE2)

hhpbg$HHVEHCNT2 <- ifelse(round(hhpbg$HHVEHCNT, 0)>2, 3, hhpbg$HHVEHCNT)
hhpbg$HHVEHCNT2 <- as.factor(hhpbg$HHVEHCNT2)

hhpbg$LNRS <- ifelse(hhpbg$RIDESHARE>=0, log(hhpbg$RIDESHARE + 1), NA_real_)
hhpbg$RIDESHARE %>% hist()
hhpbg$LNRS %>% hist()

hhpbg$HHVEHCNT %>% hist()

hhpbg$HHVEHCNT %>% hist()
hhpbg$LNHHVEH <- ifelse(hhpbg$HHVEHCNT>=0, log(hhpbg$HHVEHCNT+1), NA_real_)
hhpbg$LNHHVEH %>% hist()

data01 <- hhpbg[, c("HOUSEID", "PERSONID", 
                    "RIDESHARE", "LNRS", 
                    "HHVEHCNT", "LNHHVEH", "HHVEHCNT2", 
                    "LNPTUSED", "PTUSED2", 
                    "LNWBMODE", "NWBMODE2")] #"YEARMILE", # too many missing
colnames(data01)
map_chr(data01, class)

write_rds(data01, file.path(filepath, "11_Scratch//data01.rds"))
# data01 <- read_rds(file.path(filepath, "11_Scratch//data01.rds"))


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

class(data02$LIF_CYC)
table(data02$LIF_CYC)
data02$LIF_CYC %>% is.na() %>% sum() # no missing 
temp <- data02$LIF_CYC %>% table() /37118 *100 
temp %>% round(digits = 1)
# unweighted                                    recode
# 01 = one adult, no children           16.2 %  1
# 02 = 2+ adults, no children           38.4 %  2 
# 03 = one adult, youngest child 0-5     0.5 %  3 
# 04 = 2+ adults, youngest child 0-5    11.6 %  3
# 05 = one adult, youngest child 6-15    1.6 %  4
# 06 = 2+ adults, youngest child 6-15   14.0 %  4 
# 07 = one adult, youngest child 16-21   1.2 %  5
# 08 = 2+ adults, youngest child 16-21   7.6 %  5
# 09 = one adult, retired, no children   0.4 %  6 
# 10 = 2+ adults, retired, no children   8.6 %  6
data02$LIF_CYC2 <- recode(data02$LIF_CYC, 
                          "01"=1L, "02"=2L, "03"=3L, "04"=3L, 
                          "05"=4L, "06"=4L, "07"=5L, "08"=5L, 
                          "09"=6L, "10"=6L) %>% as.factor()

table(data02$HHFAMINC)
data02$HHFAMINC %>% is.na() %>% sum() # no missing 
temp <- table(data02$HHFAMINC) / 37118 * 100
temp %>% round(digits = 1)
# unweighted                        recode 
# -9 Not ascertained
# -8 I don't know
# -7 I prefer not to answer 
# 01=Less than $10,000       1.6 %  1 
# 02=$10,000 to $14,999      1.6 %  1
# 03=$15,000 to $24,999      3.9 %  1
# 04=$25,000 to $34,999      5.2 %  1
# 05=$35,000 to $49,999      8.3 %  2
# 06=$50,000 to $74,999     15.1 %  3
# 07=$75,000 to $99,999     15.1 %  4
# 08=$100,000 to $124,999   14.2 %  5 
# 09=$125,000 to $149,999    9.5 %  5
# 10=$150,000 to $199,999   10.5 %  6
# 11=$200,000 or more       12.8 %  6
data02$HHFAMINC2 <- 
  recode(
    data02$HHFAMINC, 
    "-7"=NA_integer_, "-8"=NA_integer_, "-9"=NA_integer_, 
    "01"=1L, "02"=1L, "03"=1L, "04"=1L, 
    "05"=2L, "06"=3L, "07"=4L, "08"=5L, 
    "09"=5L, "10"=6L, "11"=6L) %>% 
  as.factor()
table(data02$HHFAMINC2)
summary(data02$HHFAMINC2)

table(data02$HOMEOWN)
data02$HOMEOWN2 <- ifelse(data02$HOMEOWN=="01", 1, 0)
data02$HOMEOWN2 <- ifelse(data02$HOMEOWN=="-7", NA, data02$HOMEOWN2)
data02$HOMEOWN2 <- as.factor(data02$HOMEOWN2) # 1-own, 0-rent or other arrangements 
summary(data02$HOMEOWN2)

data02$GEOID <- paste0(data02$HHSTFIPS, data02$HHCNTYFP, data02$HHCT)

tract_be7 <- read_rds(paste0(filepath, "/11_Scratch/tract_be7.rds"))
data03 <- data02 %>%
  left_join(tract_be7, by=c("GEOID", "UACE10")) %>%
  as_tibble() 

b <- ncol(data03)
a <- b-4 
colnames(data03)[a:b] <- 
  c("home.den.st", "home.den.pp", "home.jobrich", "home.oldnbhd", "home.sfh")

iv_all01 <- read_rds(paste0(filepath, "/11_Scratch/iv_all01.rds")) 
data04 <- data03 %>% 
  left_join(iv_all01[, c("GEOID", "UACE10", "z.pctcoll", "z.pctyoung", "z.pctxveh")], # use of z-scores
            by=c("GEOID", "UACE10"))

b <- ncol(data04)
a <- b-2 
colnames(data04)[a:b] <- 
  c("home.pctcoll", "home.pctyoung", "home.pctxveh")

data04$GEOID <- paste0(data04$WKSTFIPS, data04$WKCNFIPS, data04$WORKCT)

iv_all01_2 <- 
  iv_all01 %>% 
  select(GEOID, UACE10, z.techden, z.servden) %>% 
  group_by(GEOID) %>%
  summarize(
    work.den.tech = mean(z.techden), # use of z-scores
    work.den.serv = mean(z.servden)  # use of z-scores
  ) %>%
  ungroup()

tract_be7_2 <- 
  tract_be7 %>%
  group_by(GEOID) %>%
  summarize(
    work.den.st = mean(ML3), 
    work.den.pp = mean(ML1), 
    work.jobrich = mean(ML2), 
    work.oldnbhd = mean(ML5), 
    work.sfh = mean(ML4)
  ) %>% 
  ungroup()

data05 <- left_join(data04, iv_all01_2, by="GEOID")
data06 <- left_join(data05, tract_be7_2, by="GEOID")
data06$GEOID <- NULL
data06$LIF_CYC <- NULL
data06$HHFAMINC <- NULL
data06$HOMEOWN <- NULL

names(data06)
map(data06, summary) # still includes those who work outside of UAs. 

write_rds(data06, file.path(filepath, "11_Scratch/data06.rds")) 
# data06 <- read_rds(file.path(filepath, "11_Scratch/data06.rds")) 


## Task 3-4. Person variables, "non-workers removed" ----

# Age, commute distance, disability, educational attainment, gender, driver's license, occupation (1 among 3)
# n of commute days per week 

data08 <- 
  hhpbg[, c("HOUSEID", "PERSONID", "R_SEX", "R_AGE", "R_RACE", "R_HISP", "DRIVER", "EDUC", 
            "WORKER", "WKFTPT", "WRKTRANS", "FLEXTIME", "GT1JBLWK", "DISTTOWK17", "OCCAT", 
            "WKRMHM", "WKFMHMXX", "MEDCOND", "PC", "SPHONE", "TAB", "WEBUSE17", "DELIVER")] %>%
  filter(WORKER=="01") 

table(data08$R_SEX) 
# -8=I don't know
# -7=I prefer not to answer
# 01=Male
# 02=Female
# After processing: 1-male, 0-female
# recode - https://dplyr.tidyverse.org/reference/recode.html
data08$R_SEX <- 
  recode(data08$R_SEX, "01" = 1L, "02" = 0L, .default = NA_integer_) %>%
  as.factor()

data08$R_AGE <- ifelse(data08$R_AGE<0, NA, data08$R_AGE)

data08$R_RACE <- ifelse(data08$R_RACE=="-7", NA, data08$R_RACE)
data08$R_RACE <- ifelse(data08$R_RACE=="-8", NA, data08$R_RACE)
data08$R_RACE <- as.factor(data08$R_RACE)

data08$R_HISP <- #1=Yes, 2=No
  recode(data08$R_HISP, "01" = 1L, "02" = 0L, .default = NA_integer_) %>% 
  as.factor()

data08$DRIVER <- 
  recode(data08$DRIVER, "01" = 1L, "02" = 0L, .default = NA_integer_) 

table(data08$EDUC)
data08$EDUC <- 
  recode(data08$EDUC,
         "01" = 1L, "02" = 1L, "03" = 2L, "04" = 3L, "05" = 4L, 
         .default = NA_integer_) %>%
  as.factor()
table(data08$EDUC)

quantile(data08[data08$DISTTOWK17>=0,]$DISTTOWK17, c(.95, .975, .99))
100 - nrow(data08[data08$DISTTOWK17>=0,])/nrow(data08)*100
# network commute distance missing 0.09% of the worker subsample 
data08$lncommute = ifelse(data08$DISTTOWK17>=0, log(data08$DISTTOWK17+1), NA_real_)
hist(data08$lncommute) # NA 34 cases 

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
data08$OCCAT2 <- 
  recode(
    data08$OCCAT, 
    "01"=1L, "02"=2L, "03"=3L, "04"=4L, "97"=5L, 
    .default = NA_integer_, .missing = NA_integer_
  ) %>% 
  as.factor()
table(data08$OCCAT2)
summary(data08$OCCAT2)

table(data08$WKFTPT)   # Full-Time or Part-Time Worker
data08$WKFTPT2 <- 
  recode(
    data08$WKFTPT, 
    "01"=1L, "02"=0L, .default = NA_integer_, .missing = NA_integer_
  ) %>% 
  as.factor()
table(data08$WKFTPT2) # 1=full time, 0=part time 

table(data08$FLEXTIME) # flex time 
data08$FLEXTIME2 <- 
  recode(
    data08$FLEXTIME, 
    "01"=1L, "02"=0L, .default = NA_integer_, .missing = NA_integer_
  ) %>% 
  as.factor()
table(data08$FLEXTIME2) # 1=yes, 0=no 

table(data08$GT1JBLWK) # More than One Job
data08$GT1JBLWK2 <- 
  recode(
    data08$GT1JBLWK, 
    "01"=1L, "02"=0L, .default = NA_integer_, .missing = NA_integer_
  ) %>% 
  as.factor()
table(data08$GT1JBLWK2) # 1=yes, 0=no 

table(data08$WKRMHM)   # Option of Working from Home
#-9=Not ascertained
#-8=I don't know
#-7=I prefer not to answer
#-1=Appropriate skip
#01=Yes
#02=No

table(data08[data08$WKRMHM!="01", ]$WKFMHMXX)
table(data08[data08$WKRMHM=="01", ]$WKFMHMXX)
# Count of Days Worked From Home in Last Month
# -8=I don't know - 5 cases
# -7=I prefer not to answer - 5 cases 
# -1=Appropriate skip

quantile(data08[data08$WKRMHM=="01", ]$WKFMHMXX, c(.25, .50, .75, .90, .95, .99))
quantile(data08[data08$WKRMHM=="01" & data08$WKFMHMXX>0, ]$WKFMHMXX, c(.25, .50, .75, .90, .95, .99))

data08$Telecommute <- NA
data08$Telecommute <- ifelse(data08$WKRMHM=="02", 0, data08$Telecommute)
data08$Telecommute <- ifelse(data08$WKRMHM=="01" & data08$WKFMHMXX== 0, 0, data08$Telecommute)
data08$Telecommute <- ifelse(data08$WKRMHM=="01" & data08$WKFMHMXX>= 1, 1, data08$Telecommute)
data08$Telecommute <- ifelse(data08$WKRMHM=="01" & data08$WKFMHMXX>= 4, 2, data08$Telecommute)
data08$Telecommute <- ifelse(data08$WKRMHM=="01" & data08$WKFMHMXX>= 8, 3, data08$Telecommute)
data08$Telecommute <- ifelse(data08$WKRMHM=="01" & data08$WKFMHMXX>=12, 4, data08$Telecommute)
data08$Telecommute <- as.factor(data08$Telecommute)
table(data08$Telecommute)
summary(data08$Telecommute)

table(data08$MEDCOND) 
# Medical Condition (person file)
# -8=I don't know
# -7=I prefer not to answer
data08$medcon <- recode(data08$MEDCOND, "01" = 1L, "02" = 0L, .default = NA_integer_, .missing = NA_integer_)  
table(data08$medcon)

table(data08$DELIVER)
# Count of Times Purchased Online for Delivery in Last 30 Days (person file)
# -9=Not ascertained
# -8=I don't know
# -7=I prefer not to answer
# Responses=0-99 (count) 

data08$deliver <- NA
data08$deliver <- ifelse(data08$DELIVER ==  0, 0, data08$deliver) # no online-delivery last month 
data08$deliver <- ifelse(data08$DELIVER >=  1, 1, data08$deliver) # less than once a week 
data08$deliver <- ifelse(data08$DELIVER >=  4, 2, data08$deliver) # once a week  
data08$deliver <- ifelse(data08$DELIVER >=  8, 3, data08$deliver) # twice a week 
data08$deliver <- ifelse(data08$DELIVER >= 12, 4, data08$deliver) # 3 or more times a week 
data08$deliver <- data08$deliver %>% round(digits = 0) %>% as.factor()
# 01 = Daily
# 02 = A few times a week
# 03 = A few times a month
# 04 = A few times a year
# 05 = Never

# Frequency of ICT device use - only one (initial recruit) in each household was asked 
names(data08)
map(data08[, c("PC", "SPHONE", "TAB", "WEBUSE17")], table)

# PC: Frequency of Desktop or Laptop Computer Use to Access the Internet
# SPHONE: Frequency of Smartphone Use to Access the Internet
# TAB: Frequency of Tablet Use to Access the Internet
# WEBUSE17: Frequency of internet use
# -9=Not ascertained
# -8=I don't know
# -7=I prefer not to answer
# 01=Daily
# 02=A few times a week
# 03=A few times a month
# 04=A few times a year
# 05=Never

myrecode <- function(x){
  recode(
    x, "01"=4L, "02"=3L, "03"=2L, "04"=1L, "05"=0L, 
    .default=NA_integer_, .missing=NA_integer_) %>% 
  as.factor()
}

data08$PC2 <- myrecode(data08$PC) 
data08$SPHONE2 <- myrecode(data08$SPHONE) 
data08$TAB2 <- myrecode(data08$TAB) 
data08$WEB2 <- myrecode(data08$WEBUSE17) 

data09 <- data08[, c("HOUSEID", "PERSONID", "R_SEX", "R_AGE", "R_RACE", "R_HISP", 
                     "DRIVER", "EDUC", "WKFTPT2", "FLEXTIME2", "GT1JBLWK2", "OCCAT2", "lncommute", 
                     "Telecommute", "deliver", "PC2", "SPHONE2", "TAB2", "WEB2", "medcon")] 
names(data09)

map(data09[, 3:20], ~is.na(.) %>% sum()) # missing cases for each variable 
unique(data09$HOUSEID) %>% length() # 25837 unique households 

write_rds(data09, file.path(filepath, "11_Scratch/data09.rds"))
# data09 <- read_rds(file.path(filepath, "11_Scratch/data09.rds")) 

## Task 3-5. Merge three dataframes ----

names(data01)
names(data06)
names(data09)

temp01 <- left_join(data01, data06, by=c("HOUSEID", "PERSONID"))
data10 <- left_join(temp01, data09, by=c("HOUSEID", "PERSONID"))

# rm("temp1")
nrow(data10)
names(data10)

write_rds(data10, file.path(filepath, "11_Scratch/data10.rds"))
# data10 <- read_rds(file.path(filepath, "11_Scratch/data10.rds"))