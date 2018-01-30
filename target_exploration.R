library(data.table)
library(dplyr)
library(zoo)
library(randomForest)
library(tidyr)
library(ggplot2)
library(DescTools)
library(pROC)

source("datathon_functions.R")

PATH_FILES_OBS <- "../data/data_target/"
FILE_HEADERS_OBS <- "../data/data_participants/headers_obs.csv"
FILE_STATIONS <- "../data/data_participants/stations.csv"

dateStart <- '2015-01-01'
dateEnd <- '2015-12-31'

THRESHOLD <- 150

dataReal <- ReadRealData(path=PATH_FILES_OBS, headersFile=FILE_HEADERS_OBS) %>% 
            CleanRealData() %>% 
            arrange(station, date, hour) %>% 
            data.table()

stationsInfo <- ReadStationsInfo(file=FILE_STATIONS)


# CREATING HOURLY VARIABLES -----------------------------------------------

# Real

dataRealHour <- copy(dataReal)


# CREATING DAILY VARIABLES ------------------------------------------------

# Real

dataRealDay <- CreateTableWithDateSequence(dateStart, dateEnd) %>%
    MergeStations(stationsInfo)

concentrationMean <- CalculateDailyMean(dataRealHour, "concentration")
dataRealDay <- merge(dataRealDay, concentrationMean, by=c("date", "station"))

concentrationMax <- CalculateDailyMax(dataRealHour, "concentration")
dataRealDay <- merge(dataRealDay, concentrationMax, by=c("date", "station"))

concentrationMedian <- CalculateDailyMedian(dataRealHour, "concentration")
dataRealDay <- merge(dataRealDay, concentrationMedian, by=c("date", "station"))

dataRealDay[, alarm := concentrationMax > THRESHOLD]

dataRealDay <- CalculateRunningMean(dataRealDay, "concentrationMean", "realYesterday", lag=1, length=1)

dataRealDay <- CalculateRunningMean(dataRealDay, "concentrationMean", "realPrev5", lag=1, length=5)

dataRealDay <- CalculateRunningMean(dataRealDay, "concentrationMean", "realPrev10", lag=1, length=10)

dataRealDay <- CalculateRunningMean(dataRealDay, "concentrationMedian", "realMedianYesterday", lag=1, length=1)

dataRealDay <- CalculateRunningMean(dataRealDay, "concentrationMax", "realMaxYesterday", lag=1, length=1)


# CREATING TABLE WITH VARIABLES FOR THE MODEL -----------------------------

tableModel <- CreateTableWithDateSequence(dateStart, dateEnd) %>%
    MergeStations(stationsInfo)

# Alarm (target)
tableModel <- merge(tableModel, dataRealDay[,.(date, station, alarm)], by=c("date", "station"))

# Maximum concentration
tableModel <- merge(tableModel, dataRealDay[,.(date, station, concentrationMax)], by=c("date", "station"))

write.csv(tableModel, file="alarm_target_days.csv", row.names=F)


# THRESHOLD STUDY ---------------------------------------------------------

thresholds <- data.table(threshold=seq <- 1:200)
stations <- stationsInfo$code
for(s in stations){
    tableModelStation <- tableModel[station==s]
    thresholds[, count := lapply(threshold, function(x) sum(tableModelStation$concentrationMax >= x)/length(tableModelStation$concentrationMax) * 100)]
    setnames(thresholds, "count", paste0("count_", s))
}
thresholds[, countTotal := lapply(threshold, function(x) sum(tableModel$concentrationMax >= x)/length(tableModel$concentrationMax) * 100)]
th <- cbind(thresholds$threshold,
            unlist(thresholds$count_ES1480A),
            unlist(thresholds$count_ES1856A),
            unlist(thresholds$count_ES1679A),
            unlist(thresholds$count_ES1438A),
            unlist(thresholds$count_ES1396A),
            unlist(thresholds$count_ES1992A),
            unlist(thresholds$count_ES0691A),
            unlist(thresholds$countTotal))
names(th) <- names(thresholds)
write.csv(th, file="threshold_counts_targetdays.csv", row.names=F)


# PLOTS -------------------------------------------------------------------

hist(tableModel$concentrationMax)
plot(ecdf(tableModel$concentrationMax))

thresholds[, count := lapply(threshold, function(x) sum(tableModel$concentrationMax >= x)/length(tableModel$concentrationMax) * 100)]

ggplot(thresholds, aes(x=threshold, y=count)) +
    geom_point()
