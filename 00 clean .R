setwd("D:/OneDrive/Yong_Doc/MMU_Degree/_paper")

library(sqldf)
library(dplyr)
library(data.table)

#read ===========
y2008.ori = read.csv(file="2008.csv", header = TRUE, nrows = 100)
airports = read.csv(file = "airports.csv", header = TRUE)
regions = read.csv(file = "Regions.csv", header = TRUE)

#Summary ====
summary(y2008.ori)
#unique(y2008.ori)

#Y ====
y2008.ori$Y = ifelse(y2008.ori$ArrDelay >= 15, 1, 0)

#Y is na ==== 
Y.isna = y2008.ori[is.na(y2008.ori$Y), ]

#cancelled flights ====
cancel = Y.isna[(Y.isna$Cancelled == "1" | Y.isna$CancellationCode == c("A", "B", "C", "D")), ]
#write.csv(cancel, file = "cancel3.csv", row.names = FALSE)
#cancel2 = read.csv(file = "cancel.csv", header = TRUE)
#cancel3 = read.csv(file = "cancel3.csv", header = TRUE)
#isTRUE(all.equal(cancel3, cancel2))

#incomplete ====
incomplete = anti_join(Y.isna, cancel)
write.csv(incomplete, file = "incomplete.csv", row.names = FALSE)

#y2008 ====

#omit Y = NA/cancelled flights
y2008 = y2008.ori[!(is.na(y2008.ori$Y)), ]
y2008 = y2008[!(y2008$Cancelled == "1" | y2008$CancellationCode == c("A", "B", "C", "D")), ]
#colSums(is.na(y2008))
#unique(y2008$Diverted)
#Diverted is dropped because of all values are IDENTICAL --> SD don't change also
y2008 = within(y2008, rm(Year, TailNum, Cancelled, CancellationCode, Diverted))
write.csv(y2008, file = "y2008.csv", row.names = FALSE)

#non-delay
ndelay = y2008[y2008$Y == 0, ]
colSums(is.na(ndelay))
ndelay[ , 20:24][is.na(ndelay[ , 20:24])] = 0
write.csv(ndelay, file = "ndelay.csv", row.names = FALSE)

#delay
delay = y2008[y2008$Y == 1, ]
colSums(is.na(delay))
write.csv(delay, file = "delay.csv", row.names = FALSE)

#y2008geo ====
y2008 = rbind(ndelay, delay)
#colSums(is.na(y2008))

x = data.frame(x = unique(y2008.ori$Origin))
#y2008geo Origin
sql_origin = "select y2008.*, airports.* from y2008, airports where y2008.Origin = airports.iata"
y2008geo = sqldf(sql_origin)
unique(y2008geo$country)
y2008geo = within(y2008geo,rm(iata,airport,city, country, lat,long))
origin.isna = y2008geo[is.na(y2008geo$state), ]
sql_region = "select y2008geo.*, regions.region from y2008geo, regions where y2008geo.state = regions.state"
y2008geo = sqldf(sql_region)
setnames(y2008geo, old = c("state", "region"), new = c("ori_state", "ori_region"))

#y2008geo Dest
sql_dest = "select y2008geo.*, airports.* from y2008geo, airports where y2008geo.Dest = airports.iata"
y2008geo = sqldf(sql_dest)
y2008geo = within(y2008geo,rm(iata,airport,city, country, lat,long))
dest.isna = y2008geo[is.na(y2008geo$state), ]
unique(dest.isna$Dest)
sql_region = "select y2008geo.*, regions.region from y2008geo, regions where y2008geo.state = regions.state"
y2008geo = sqldf(sql_region)
setnames(y2008geo, old = c("state", "region"), new = c("dest_state", "dest_region"))
#colSums(is.na(y2008geo))

write.csv(y2008geo, file = "y2008.geo.csv", row.names = FALSE)
ndelay.geo = y2008geo[y2008geo$Y == 0, ]
delay.geo = y2008geo[y2008geo$Y == 1, ]
write.csv(ndelay.geo, file = "ndelay.geo.csv", row.names = FALSE)
write.csv(delay.geo, file = "delay.geo.csv", row.names = FALSE)

save.image("D:/OneDrive/Rea_Doc/MMU_Degree/Degree_Sems_007_NOV/FYP 2/0_Yong/00 clean.RData")

#sapply(y2008drop,function(x) sum(is.na(x)))
#sapply(y2008drop,function(x) length(unique(x)))
#sapply(y2008, class)



