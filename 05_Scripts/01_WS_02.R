


setwd('M:/Uber_NHTS/13_Process')

#3.3. walkscore.com scrapping 

#install.packages("foreign")
#https://stats.stackexchange.com/questions/8225/how-to-summarize-data-by-group-in-r
#install.packages("plyr")

library(foreign)
library(plyr)

bg <- read.dbf('M:/Uber_NHTS/06_Shapefile/Blkgrp/Blkgrp_50topUA.dbf')
colnames(bg) <- c("HHSTFIPS","HHCNTYFP","HHCT","HHBG", "GEOID", 
                  "ALAND", "AWATER", "UACE10", "WGS84x","WGS84y")
bgd <- bg[, c("GEOID", "ALAND", "UACE10")]
bgd$GEOID2 <- as.character(bgd$GEOID)
bgd$HHSTFIPS <- as.integer(substr(bgd$GEOID2, 1, 2))
bgd$HHCNTYFP <- as.integer(substr(bgd$GEOID2, 3, 5))
bgd$HHCT <- as.integer(substr(bgd$GEOID2, 6, 11))
bgd$HHBG <- as.integer(substr(bgd$GEOID2, 12, 12))
bgd <- bgd[, c("HHSTFIPS","HHCNTYFP","HHCT","HHBG", "GEOID", 
               "ALAND", "UACE10")]
bgd$tr <- substr(bgd$GEOID, 1, 11)
trd <- ddply(bgd, "tr", numcolwise(mean))[, c("tr", "UACE10")]
colnames(trd)[1] <- "GEOID"
rm("bg", "bgd")

#https://walkerke.github.io/tidycensus/articles/spatial-data.html
#install.packages("tidycensus")
#install.packages("tidyverse")
library(tidycensus)
library(tidyverse)

#https://cran.r-project.org/web/packages/tigris/tigris.pdf
#install.packages("tigris")
library(tigris)

tigris_cache_dir("M:/Uber_NHTS/05_Census/tigris")
readRenviron('~/.Renviron')
options(tigris_use_cache=TRUE)

ST <- c("AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "IL", 
        "IN", "KS", "KY", "LA", "MD", "MA", "MI", "MN", "MS", "MO",
        "NV", "NH", "NJ", "NY", "NC", "OH", "OK", "OR", "PA", "RI",
        "SC", "TN", "TX", "UT", "VA", "WA", "WI")

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
nrow(trxy)

#https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
#https://cran.r-project.org/web/packages/rvest/rvest.pdf
#install.packages("rvest")

#https://stackoverflow.com/questions/45733802/r-scraping-skip-html-error-500-in-loop
#install.packages("httr")

#Loading the rvest package
library(xml2)

#https://stackoverflow.com/questions/45733802/r-scraping-skip-html-error-500-in-loop
library(rvest)

#https://stackoverflow.com/questions/33295686/rvest-error-in-open-connectionx-rb-timeout-was-reached
library(httr)

#https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
options(stringsAsFactors = FALSE)
walkscorescrape <- function(start, end) {
  walkscore <- NULL
  for(i in start:end){
    x   <- trxy[i, "x"] 
    y   <- trxy[i, "y"] 
    url <- paste('https://www.walkscore.com/score/lat=4http://www.walkscore.com/score/lat=', y,'/lng=', x, sep="")
    if((class(try(read_html(url), silent = TRUE))[1] == "xml_document") == TRUE){
      webpage <- read_html(url)
      walkscore_html <- html_nodes(webpage, '.score-div:nth-child(1) img')
      temp1 <- as.character(walkscore_html)
      temp2 <- as.numeric(substr(temp1, 41, as.numeric(gregexpr(pattern = 'alt=', temp1)[[1]][1])-7))
      temp3 <- c(trxy[i, "GEOID"], temp2)
      message(".", appendLF=FALSE)
      #Sys.sleep(time=1)
    } else {
      message(trxy[i, "GEOID"], appendLF=FALSE)
      Sys.sleep(time=5)   
    }
    walkscore <- rbind(walkscore, temp3)
  }
  walkscore <- as.data.frame(walkscore)
  colnames(walkscore) <- c("GEOID", "walkscore")
  rownames(walkscore) <- NULL 
  walkscore$walkscore <- as.numeric(walkscore$walkscore)
  return(walkscore)
}


#walk01 <- walkscorescrape(    1,10000)
#write.csv(walk01, file="M:/Uber_NHTS/07_BE/walkscore/walk01.csv")
walk02 <- walkscorescrape(10001,20000)
write.csv(walk02, file="M:/Uber_NHTS/07_BE/walkscore/walk02.csv")
walk03 <- walkscorescrape(20001,30000)
write.csv(walk03, file="M:/Uber_NHTS/07_BE/walkscore/walk03.csv")
walk04 <- walkscorescrape(30001,32949)
write.csv(walk04, file="M:/Uber_NHTS/07_BE/walkscore/walk04.csv")
