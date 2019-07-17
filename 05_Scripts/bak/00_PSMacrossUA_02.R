

## Run the main script first 

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
             
UAvars <- c("UA01", "UA02", "UA03", "UA04", "UA05", "UA06", "UA07", "UA08", "UA09", "UA10", 
           "UA11", "UA12", "UA13", "UA14", "UA15", "UA16", "UA17", "UA18", "UA19", "UA20", 
           "UA21", "UA22", "UA23", "UA24", "UA25", "UA26", "UA27", "UA28", "UA29", "UA30", 
           "UA31", "UA32", "UA33", "UA34", "UA35", "UA36", "UA37", "UA38", "UA39", "UA40", 
           "UA41", "UA42", "UA43", "UA44", "UA45", "UA46", "UA47", "UA48", "UA49", "UA50")  

summary.unmatched <-CreateTableOne(vars=xvars, strata="RS", data=data12, test=TRUE)
print(summary.unmatched, smd=TRUE)


## PSM for all UA together 
glm.50UA <- glm(RS ~ LIF_CYC02 + LIF_CYC03 + LIF_CYC04 + LIF_CYC05 + LIF_CYC06 + LIF_CYC07 + LIF_CYC08 +
                  LIF_CYC09 + LIF_CYC10 + WRKCOUNT + DRVRCNT  + NUMCHILD + YOUNGCHILD + HHFAMINC02 + 
                  HHFAMINC03 + HHFAMINC04 + HHFAMINC05 + HHFAMINC06 + HHFAMINC07 + HHFAMINC08 + 
                  HHFAMINC09 + HHFAMINC10 + HHFAMINC11 + HOMEOWN + homeden + workden + R_SEX + R_AGE + 
                  R_RACE02 + R_RACE03 + R_RACE04 + R_RACE05 + R_RACE06 + R_RACE97 + R_HISP + 
                  DRIVER + EDUC02 + EDUC03 + EDUC04 + EDUC05 + OCCAT02 + OCCAT03 + OCCAT04 + OCCAT97 + 
                  Telecommute01 + Telecommute02 + Telecommute03 + Telecommute04 + medcon + DELIVER + 
                  GTscore.std + UA02 + UA03 + UA04 + UA05 + UA06 + UA07 + UA08 + UA09 + UA10 + 
                  UA11 + UA12 + UA13 + UA14 + UA15 + UA16 + UA17 + UA18 + UA19 + UA20 + 
                  UA21 + UA22 + UA23 + UA24 + UA25 + UA26 + UA27 + UA28 + UA29 + UA30 + 
                  UA31 + UA32 + UA33 + UA34 + UA35 + UA36 + UA37 + UA38 + UA39 + UA40 + 
                  UA41 + UA42 + UA43 + UA44 + UA45 + UA46 + UA47 + UA48 + UA49 + UA50 , 
                family=binomial(link="probit"), control = list(maxit = 100), data=data12)
a <- fitted.values(glm.50UA)
caliper.50UA <- 0.25*sd(a)
caliper.50UA
matchit.50UA <- matchit(glm.50UA, method="nearest", ratio=1, replace=TRUE, model="probit", 
                        data=data12, caliper=caliper.50UA)
match.data.50UA <- match.data(matchit.50UA)
## https://r.iq.harvard.edu/docs/matchit/2.4-20/How_Exactly_are.html
b <- nrow(match.data.50UA[match.data.50UA$RS==1, ])/nrow(match.data.50UA[match.data.50UA$RS==0, ])
match.data.50UA$weights2 <- ifelse(match.data.50UA$RS==0, match.data.50UA$weights*b, match.data.50UA$weights)
match.data.50UA$weights2 <- as.integer(round(match.data.50UA$weights2, 0))


match.data.50UA$weights3 <- NA
match.data.50UA$weights3 <- ifelse(match.data.50UA$RS==1, 1, match.data.50UA$weights3)
match.data.50UA$weights3 <- ifelse(match.data.50UA$RS==0 & match.data.50UA$weights2==1,  
                                   match.data.50UA$distance/(1-match.data.50UA$distance), 
                                   match.data.50UA$weights3)
match.data.50UA$weights3 <- ifelse(match.data.50UA$RS==0 & match.data.50UA$weights2>1,   
                                   match.data.50UA$distance/(1-match.data.50UA$distance)*match.data.50UA$weights2, 
                                   match.data.50UA$weights3)
hist(match.data.50UA$weights3)


## check the matching performance 
sapply(match.data.50UA, class)
hist(match.data.50UA$HHVEHCNT)
hist(match.data.50UA[match.data.50UA$RS==1, ]$HHVEHCNT)
hist(match.data.50UA[match.data.50UA$RS==0, ]$HHVEHCNT)

match.data.50UA$HHVEHCNT3 <- log(match.data.50UA$HHVEHCNT+1)
hist(match.data.50UA$HHVEHCNT3, xlim=c(0,3))
hist(match.data.50UA[match.data.50UA$RS==1, ]$HHVEHCNT3, xlim=c(0,3))
hist(match.data.50UA[match.data.50UA$RS==0, ]$HHVEHCNT3, xlim=c(0,3))



a <- ddply(match.data.50UA[match.data.50UA$RS==1, ], .(UACE10), summarize, OccUsers=sum(is.na(HOUSEID)==FALSE))
b <- ddply(match.data.50UA[match.data.50UA$RS==0, ], .(UACE10), summarize, NonUsers=sum(is.na(HOUSEID)==FALSE))
c <- ddply(data12[data12$RS==1, ], .(UACE10), summarize, OccUsersBefore=sum(is.na(HOUSEID)==FALSE))
d <- ddply(data12[data12$RS==0, ], .(UACE10), summarize, NonUsersBefore=sum(is.na(HOUSEID)==FALSE))

e <- merge(a, b, by=c("UACE10"))
e <- merge(e, c, by=c("UACE10"))
e <- merge(e, d, by=c("UACE10"))
e <- merge(nhtsualist[, c("UACE10", "NAME10")], e, by=c("UACE10"))
e$pctOccUsers=round(e$OccUsers/e$OccUsersBefore*100, 1)
e$pctNonUsers=round(e$NonUsers/e$NonUsersBefore*100, 1)
e[order(-e$NonUsers), ]


plot(matchit.50UA, type="jitter") 
plot(matchit.50UA, type="hist") 
par(mfrow=c(1,1))


## Created weighted data object: https://rpubs.com/kaz_yos/matching-weights
library(grid) 
library(Matrix)
library(survey)

temp1 <- match.data.50UA
temp1$HHVEHCNT2 <- as.numeric(as.character(temp1$HHVEHCNT2))
temp1$PTUSED2   <- as.numeric(as.character(temp1$PTUSED2))
temp1$NWBMODE2  <- as.numeric(as.character(temp1$NWBMODE2))
temp1$HOMEOWN   <- as.numeric(as.character(temp1$HOMEOWN))
temp1$R_SEX     <- as.numeric(as.character(temp1$R_SEX))
temp1$R_HISP    <- as.numeric(as.character(temp1$R_HISP))

## Weighted table with tableone - all covariates except UA indicators
a1 <- svydesign(ids = ~ 1, data = temp1, weights = ~ weights2)
summary.matched1 <- svyCreateTableOne(vars = xvars, strata ="RS", data = a1)
print(summary.matched1, test=TRUE, smd = TRUE)
## ExtractSmd(summary.test)

## Weighted table with tableone - 50 UA indicators
a2 <- svydesign(ids = ~ 1, data = temp1, weights = ~ weights2)
summary.matched2 <- svyCreateTableOne(vars = UAvars, strata ="RS", data = a2)
print(summary.matched2, test=TRUE, smd = TRUE)
## ExtractSmd(summary.test)



## generate a list of variable names 

pair03 <- match.data.50UA[, c(2:6, 128, 7:9, 10:72, 1, 73:127)] 
colnames(pair03)

varname <- as.data.frame(colnames(pair03))
nrow(varname)
mplusname <- c("id1", "id2", "y1",  "y2",  "y3",  "y4",  "y5",  "y6",  "y7",
               "x1",  "x2",  "x3",  "x4",  "x5",  "x6",  "x7",  "x8",  "x9",  "x10", 
               "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", 
               "x21", "x22", "x23", "x24", "x25", "x26", "y8",  "x27", "x28", "x29", "x30", 
               "x31", "x32", "x33", "x34", "x35", "x36", "x37", "x38", "x39", "x40",
               "x41", "x42", "x43", "x44", "x45", "x46", "x47", "x48", "x49", "x50", 
               "x51", "x52", "x53", "x54", "x55", "x56", "x57", "x58", "x59", "x60", 
               "x61", "x62", "x63", "x64", 
               "UA01", "UA02", "UA03", "UA04", "UA05", "UA06", "UA07", "UA08", "UA09", "UA10", 
               "UA11", "UA12", "UA13", "UA14", "UA15", "UA16", "UA17", "UA18", "UA19", "UA20", 
               "UA21", "UA22", "UA23", "UA24", "UA25", "UA26", "UA27", "UA28", "UA29", "UA30", 
               "UA31", "UA32", "UA33", "UA34", "UA35", "UA36", "UA37", "UA38", "UA39", "UA40", 
               "UA41", "UA42", "UA43", "UA44", "UA45", "UA46", "UA47", "UA48", "UA49", "UA50", 
               "ps", "wt1", "wt2", "wt3") 
length(mplusname)
a <- cbind(varname, mplusname) 
a

varfullname <- c(
  "HOUSEID", "PERSONID", "# RIDESHARE", "# HHVEHCNT", "O HHVENCHT", "LN HHVENCHT", "O PT", "O Walk/Bike", "Yes RIDESHARE",
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
  "Medical condition", "# online delivery", "Google Trend", "cases", "UACE10", "NAME10", 
  "UA01", "UA02", "UA03", "UA04", "UA05", "UA06", "UA07", "UA08", "UA09", "UA10", 
  "UA11", "UA12", "UA13", "UA14", "UA15", "UA16", "UA17", "UA18", "UA19", "UA20", 
  "UA21", "UA22", "UA23", "UA24", "UA25", "UA26", "UA27", "UA28", "UA29", "UA30", 
  "UA31", "UA32", "UA33", "UA34", "UA35", "UA36", "UA37", "UA38", "UA39", "UA40", 
  "UA41", "UA42", "UA43", "UA44", "UA45", "UA46", "UA47", "UA48", "UA49", "UA50",
  "pscore", "WT1", "WT2", "WT3")
b <- cbind(a, varfullname)
b


## rownames(pooled.match) <- NULL 
## nrow(pooled.match)

write.csv(pair03[, c(1:73, 75:128)], "M:/Uber_NHTS/15_Model/pooled_match_02.csv")

ddply(pair03, .(UACE10), summarize, Count=sum(is.na(UACE10)==FALSE))
