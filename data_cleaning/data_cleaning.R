library(reshape)
library(data.table)
library(stringr)
library(plyr)

setwd("/Users/giuliabrutti/Documents/GitHub/MY499_code")

# population an dviolence data

df <- read.csv("data/pop_incidents_GOV.csv")

df$popchange_9095 <- df$POP1995 - df$POP1990
df$popchange_9500 <- df$POP2000 - df$POP1995
df$popchange_0005 <- df$POP2005 - df$POP2000
df$popchange_0510 <- df$POP2010 - df$POP2005
df$popchange_1015 <- df$POP2015 - df$POP2010

pop_9091 <- data.frame(aggregate(df$POP1990, by = list(df$state), FUN = sum))
pop_9596 <- data.frame(aggregate(df$POP1995, by = list(df$state), FUN = sum))
pop_0001 <- data.frame(aggregate(df$POP2000, by = list(df$state), FUN = sum))
pop_0506 <- data.frame(aggregate(df$POP2005, by = list(df$state), FUN = sum))
pop_1011 <- data.frame(aggregate(df$POP2010, by = list(df$state), FUN = sum))
pop_1516 <- data.frame(aggregate(df$POP2015, by = list(df$state), FUN = sum))

df_incidents <- df[,c(1, 11:15)]
df_pop_change <- df[, c(1, 16:20)]
df_pop <- df[, c(1, 3:7)]

df_1 <- melt(setDT(df_incidents), id = 1, variable.name = "year")
df_2 <- melt(setDT(df_pop_change), id = 1, variable.name = "year")
df_3 <- melt(setDT(df_pop), id = 1, variable.name = "year")

df_2$year <- as.character(df_2$year)
df_2[df_2 == "popchange_9095"] <- "1"
df_2[df_2 == "popchange_9500"] <- "2"
df_2[df_2 == "popchange_0005"] <- "3"
df_2[df_2 == "popchange_0510"] <- "4"
df_2[df_2 == "popchange_1015"] <- "5"

df_1$year <- as.character(df_1$year)
df_1[df_1 == "Incidents_9600"] <- "1"
df_1[df_1 == "Incidents_0105"] <- "2"
df_1[df_1 == "Incidents_0610"] <- "3"
df_1[df_1 == "Incidents_1115"] <- "4"
df_1[df_1 == "Incidents_1620"] <- "5"

df_3$year <- as.character(df_3$year)
df_3[df_3 == "POP1990"] <- "1"
df_3[df_3 == "POP1995"] <- "2"
df_3[df_3 == "POP2000"] <- "3"
df_3[df_3 == "POP2005"] <- "4"
df_3[df_3 == "POP2010"] <- "5"

data <- merge(df_1, df_2, by = c("name", "year"))
data <- merge(data, df_3, by = c("name", "year"))
names(data)[3] <- "incidents"
names(data)[4] <- "pop_change"
names(data)[5] <- "pop"

for (i in 1:nrow(data)){
  data$area[i] <- df$Shape_Area[df$name == data$name[i]]
}

for (i in 1:nrow(data)){
  data$state[i] <- df$state[df$name == data$name[i]]
}

# LULC data
LULC_9297 <- read.csv("data/pop_LULC9297.csv")
LULC_9702 <- read.csv("data/pop_LULC9702.csv")
LULC_0207 <- read.csv("data/pop_LULC0207.csv")
LULC_0712 <- read.csv("data/pop_LULC0712.csv")
LULC_1217 <- read.csv("data/pop_LULC1217.csv")

LULC_9297 <- reshape(LULC_9297, idvar = c("name"), timevar = "Class_name", direction = "wide", v.names = c ("Shape_Area"))
LULC_9297[is.na(LULC_9297)] <- 0

LULC_9702 <- reshape(LULC_9702, idvar = c("name"), timevar = "Class_name", direction = "wide", v.names = c ("Shape_Area"))
LULC_9702[is.na(LULC_9702)] <- 0

LULC_0207<- reshape(LULC_0207, idvar = c("name"), timevar = "Class_name", direction = "wide", v.names = c ("Shape_Area"))
LULC_0207[is.na(LULC_0207)] <- 0

LULC_0712 <- reshape(LULC_0712, idvar = c("name"), timevar = "Class_name", direction = "wide", v.names = c ("Shape_Area"))
LULC_0712[is.na(LULC_0712)] <- 0

LULC_1217 <- reshape(LULC_1217, idvar = c("name"), timevar = "Class_name", direction = "wide", v.names = c ("Shape_Area"))
LULC_1217[is.na(LULC_1217)] <- 0

area <- df[, c(1,10, 15:18)]

names(area)[2] <- "area"
names(area)[1] <- "name"
area$name <- str_to_title(area$name)

LULC_9297 <- merge(LULC_9297, area, by = "name", all = T)
LULC_9702 <- merge(LULC_9702, area, by = "name", all = T)
LULC_0207 <- merge(LULC_0207, area, by = "name", all = T)
LULC_0712 <- merge(LULC_0712, area, by = "name", all = T)
LULC_1217 <- merge(LULC_1217, area, by = "name", all = T)

LULC_9297[,-c(1:2,22:ncol(LULC_9297))] <- (LULC_9297[,-c(1:2,22:ncol(LULC_9297))]/LULC_9297$area)*100
LULC_9702[,-c(1:2,33:ncol(LULC_9702))] <- (LULC_9702[,-c(1:2,33:ncol(LULC_9702))]/LULC_9702$area)*100
LULC_0207[,-c(1:2,33:ncol(LULC_0207))] <- (LULC_0207[,-c(1:2,33:ncol(LULC_0207))]/LULC_0207$area)*100
LULC_0712[,-c(1:2,33:ncol(LULC_0712))] <- (LULC_0712[,-c(1:2,33:ncol(LULC_0712))]/LULC_0712$area)*100
LULC_1217[,-c(1:2,33:ncol(LULC_1217))] <- (LULC_1217[,-c(1:2,33:ncol(LULC_1217))]/LULC_1217$area)*100

LULC_9297$year <- "1"
LULC_9702$year <- "2"
LULC_0207$year <- "3"
LULC_0712$year <- "4"
LULC_1217$year <- "5"

LULC <- rbind.fill(LULC_9297, LULC_9702, LULC_0207, LULC_0712, LULC_1217)
LULC <- LULC[, -c(22:26)]
LULC[is.na(LULC)] <- 0

data_LULC <- data.frame(merge(data, LULC, by = c("name", "state", "year")))

data_LULC_1 <- data_LULC[data_LULC$year == "1",]
data_LULC_2 <- data_LULC[data_LULC$year == "2",]
data_LULC_3 <- data_LULC[data_LULC$year == "3",]
data_LULC_4 <- data_LULC[data_LULC$year == "4",]
data_LULC_5 <- data_LULC[data_LULC$year == "5",]

# prior violence data
d1 <- read.csv("data/incidents_9195.csv")
d1 <- d1[,-2]

d2 <- data_LULC_1[,c(1,4)]
d3 <- data_LULC_2[,c(1,4)]
d4 <- data_LULC_3[,c(1,4)]
d5 <- data_LULC_4[,c(1,4)]

d1$year <- "1"
d2$year <- "2"
d3$year <- "3"
d4$year <- "4"
d5$year <- "5"

names(d1)[2] <- "incidents_prior"
names(d2)[2] <- "incidents_prior"
names(d3)[2] <- "incidents_prior"
names(d4)[2] <- "incidents_prior"
names(d5)[2] <- "incidents_prior"

d_merge <- rbind.fill(d1, d2, d3, d4, d5)
data_LULC <- merge(data_LULC, d_merge, by =c("name","year"), all = T)

# population growth variable
data_LULC$pop_growth <- log((data_LULC$pop_change+data_LULC$pop)/data_LULC$pop)

# subset data set
data_LULC_1 <- data_LULC[data_LULC$year == "1",]
data_LULC_2 <- data_LULC[data_LULC$year == "2",]
data_LULC_3 <- data_LULC[data_LULC$year == "3",]
data_LULC_4 <- data_LULC[data_LULC$year == "4",]
data_LULC_5 <- data_LULC[data_LULC$year == "5",]


