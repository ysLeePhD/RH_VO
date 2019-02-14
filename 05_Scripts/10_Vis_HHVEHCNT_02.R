
library(foreign)
library(plyr)
library(tidyverse)
library(ggplot2)

hh <- read.csv('M:/Uber_NHTS/03_NHTS/Csv/hhpub.csv')

hh$HHVEHCNT2 <- ifelse(hh$HHVEHCNT>=3, 3, hh$HHVEHCNT)
temp0 <- ddply(hh, .(HHVEHCNT2), summarize, freq=length(WTHHFIN), tot=sum(WTHHFIN))
a0 <- sum(temp0$tot)
temp0$wtpct <- temp0$tot/a0*100
temp0 <- temp0[, c("HHVEHCNT2", "wtpct")]
temp0$class <- "Households"

per <- read.csv('M:/Uber_NHTS/03_NHTS/Csv/perpub.csv')

per$HHVEHCNT2 <- ifelse(per$HHVEHCNT>=3, 3, per$HHVEHCNT)
temp1 <- ddply(per[per$R_AGE>=18, ], .(HHVEHCNT2), summarize, freq=length(WTPERFIN), tot=sum(WTPERFIN))
a1 <- sum(temp1$tot)
temp1$wtpct <- temp1$tot/a1*100
temp1 <- temp1[, c("HHVEHCNT2", "wtpct")]
temp1$class <- "Persons(18+)"

temp2 <- ddply(per[per$R_AGE>=18 & per$RIDESHARE==0, ], .(HHVEHCNT2), summarize, freq=length(WTPERFIN), tot=sum(WTPERFIN))
a2 <- sum(temp2$tot)
temp2$wtpct <- temp2$tot/a2*100
temp2 <- temp2[, c("HHVEHCNT2", "wtpct")]
temp2$class <- "Non users (89.9%)"

temp3 <- ddply(per[per$R_AGE>=18 & per$RIDESHARE>0, ], .(HHVEHCNT2), summarize, freq=length(WTPERFIN), tot=sum(WTPERFIN))
a3 <- sum(temp3$tot)
temp3$wtpct <- temp3$tot/a3*100
temp3 <- temp3[, c("HHVEHCNT2", "wtpct")]
temp3$class <- "Users (10.0%)"

temp4 <- ddply(per[per$R_AGE>=18 & per$RIDESHARE>0 & per$RIDESHARE<4, ], .(HHVEHCNT2), summarize, freq=length(WTPERFIN), tot=sum(WTPERFIN))
a4 <- sum(temp4$tot)
temp4$wtpct <- temp4$tot/a4*100
temp4 <- temp4[, c("HHVEHCNT2", "wtpct")]
temp4$class <- "Occ users (5.9%)"

temp5 <- ddply(per[per$R_AGE>=18 & per$RIDESHARE>=4 & per$RIDESHARE<8, ], .(HHVEHCNT2), summarize, freq=length(WTPERFIN), tot=sum(WTPERFIN))
a5 <- sum(temp5$tot)
temp5$wtpct <- temp5$tot/a5*100
temp5 <- temp5[, c("HHVEHCNT2", "wtpct")]
temp5$class <- "Reg users (2.5%)"

temp6 <- ddply(per[per$R_AGE>=18 & per$RIDESHARE>=8, ], .(HHVEHCNT2), summarize, freq=length(WTPERFIN), tot=sum(WTPERFIN))
a6 <- sum(temp6$tot)
temp6$wtpct <- temp6$tot/a6*100
temp6 <- temp6[, c("HHVEHCNT2", "wtpct")]
temp6$class <- "Freq users (1.6%)"

c(a2/a1*100, a3/a1*100, a4/a1*100, a5/a1*100, a6/a1*100)

temp10 <- rbind(temp0,  temp1)  
temp10 <- rbind(temp10, temp2)
temp10 <- rbind(temp10, temp3) 
temp10 <- rbind(temp10, temp4)
temp10 <- rbind(temp10, temp5)
temp10 <- rbind(temp10, temp6)
colnames(temp10) <- c("HHVEHCNT", "SHARE", "GROUP")

temp10$GROUP <- as.factor(temp10$GROUP) 
print(levels(temp10$GROUP))
temp10$GROUP <- factor(temp10$GROUP, levels(temp10$GROUP)[c(2, 5, 3, 7, 4, 6, 1)])

temp10$HHVEHCNT <- as.factor(temp10$HHVEHCNT)
print(levels(temp10$HHVEHCNT))
temp10$HHVEHCNT <- factor(temp10$HHVEHCNT, levels(temp10$HHVEHCNT)[c(4, 3, 2, 1)])

ggplot() + 
  geom_bar(aes(y=SHARE, x=GROUP, fill=HHVEHCNT), data=temp10, stat="identity") + 
  geom_text(data=temp10, position=position_stack(vjust=0.5), aes(x = GROUP, y = SHARE,
                                                                 label = paste0(round(SHARE, digits=1),"%")), size=3) 