# Working on data for the BCNAnalytics datathon on air quality

# SETTINGS ----------------------------------------------------------------

library(data.table)
library(dplyr)
library(zoo)
library(randomForest)
library(tidyr)
library(ggplot2)
library(DescTools)
library(pROC)
library(MLmetrics)

source("datathon_functions.R")

FILE_STATIONS <- "../data/data_participants/stations.csv"
PATH_FILES_MOD <- "../data/data_participants/mod"
FILE_HEADERS_MOD <- "../data/data_participants/headers_mod.csv"
PATH_FILES_OBS <- "../data/data_participants/obs"
FILE_HEADERS_OBS <- "../data/data_participants/headers_obs.csv"
FILE_TARGET <- "../data/data_participants/targets.csv"

THRESHOLD <- 100

dateStart <- '2013-01-01'
dateEnd <- '2015-12-31'
lastTrainingDate <- '2014-12-31'


# READING DATA FILES ------------------------------------------------------

stationsInfo <- ReadStationsInfo(file=FILE_STATIONS)
dataModel <- ReadModelData(path=PATH_FILES_MOD, headersFile=FILE_HEADERS_MOD) %>% 
             CleanModelData(stationsInfo)
dataReal <- ReadRealData(path=PATH_FILES_OBS, headersFile=FILE_HEADERS_OBS) %>% 
            CleanRealData()


# CREATING HOURLY VARIABLES -----------------------------------------------

# Real

dataRealHour <- copy(dataReal)


# Model

dataModelHour <- copy(dataModel) %>% 
                 SeparatePredictionByPredictionDate()


# CREATING DAILY VARIABLES ------------------------------------------------

# Real

dataRealDay <- CreateTableWithDateSequence(dateStart, dateEnd) %>%
               MergeStations(stationsInfo)

concentrationMean <- CalculateDailyMean(dataRealHour, "concentration")
dataRealDay <- merge(dataRealDay, concentrationMean,
                     by=c("date", "station"), all.x=T)

concentrationMax <- CalculateDailyMax(dataRealHour, "concentration")
dataRealDay <- merge(dataRealDay, concentrationMax,
                     by=c("date", "station"), all.x=T)

concentrationMedian <- CalculateDailyMedian(dataRealHour, "concentration")
dataRealDay <- merge(dataRealDay, concentrationMedian,
                     by=c("date", "station"), all.x=T)

dataRealDay[, alarm := concentrationMax > THRESHOLD]

dataRealDay <- CalculateRunningMean(dataRealDay, "concentrationMean", "realYesterday", lag=1, length=1)

dataRealDay <- CalculateRunningMean(dataRealDay, "concentrationMean", "realPrev5", lag=1, length=5)

dataRealDay <- CalculateRunningMean(dataRealDay, "concentrationMean", "realPrev10", lag=1, length=10)

dataRealDay <- CalculateRunningMean(dataRealDay, "concentrationMedian", "realMedianYesterday", lag=1, length=1)

dataRealDay <- CalculateRunningMean(dataRealDay, "concentrationMax", "realMaxYesterday", lag=1, length=1)

# Model

dataModelDay <- CreateTableWithDateSequence(dateStart, dateEnd) %>%
    MergeStations(stationsInfo)

predictionTodayMean <- CalculateDailyMean(dataModelHour, "predictionToday")
dataModelDay <- merge(dataModelDay, predictionTodayMean, by=c("date", "station"))

predictionTodayMax <- CalculateDailyMax(dataModelHour, "predictionToday")
dataModelDay <- merge(dataModelDay, predictionTodayMax, by=c("date", "station"))

predictionYesterdayMean <- CalculateDailyMean(dataModelHour, "predictionYesterday")
dataModelDay <- merge(dataModelDay, predictionYesterdayMean, by=c("date", "station"))

predictionYesterdayMax <- CalculateDailyMax(dataModelHour, "predictionYesterday")
dataModelDay <- merge(dataModelDay, predictionYesterdayMax, by=c("date", "station"))


# CREATING TABLE WITH VARIABLES FOR THE MODEL -----------------------------

tableModel <- CreateTableWithDateSequence(dateStart, dateEnd) %>%
              MergeStations(stationsInfo)

# Alarm (target)
tableModel <- merge(tableModel, dataRealDay[,.(date, station, alarm)],
                    by=c("date", "station"), all.x=T)

# Maximum houry conecntration predicted in the prediction performed today at 00:00
tableModel <- merge(tableModel, dataModelDay[,.(date, station, predictionTodayMax)],
                    by=c("date", "station"), all.x=T)

# Mean daily concentration predicted in the prediction performed today at 00:00
tableModel <- merge(tableModel, dataModelDay[,.(date, station, predictionTodayMean)],
                    by=c("date", "station"), all.x=T)

# Maximum houry concentration predicted in the prediction performed yesterday at 00:00
tableModel <- merge(tableModel, dataModelDay[,.(date, station, predictionYesterdayMax)],
                    by=c("date", "station"), all.x=T)

# Mean daily concentration predicted in the prediction performed yesterday at 00:00
tableModel <- merge(tableModel, dataModelDay[,.(date, station, predictionYesterdayMean)],
                    by=c("date", "station"), all.x=T)

# Real concentration observed yesterday
tableModel <- merge(tableModel, dataRealDay[,.(date, station, realYesterday)],
                    by=c("date", "station"), all.x=T)

# Mean concentration of the previous 5 days
tableModel <- merge(tableModel, dataRealDay[,.(date, station, realPrev5)],
                    by=c("date", "station"), all.x=T)

# Mean concentration of the previous 10 days
tableModel <- merge(tableModel, dataRealDay[,.(date, station, realPrev10)],
                    by=c("date", "station"), all.x=T)

# Median concentration observed yesterday
tableModel <- merge(tableModel, dataRealDay[,.(date, station, realMedianYesterday)],
                    by=c("date", "station"), all.x=T)

# Maximum concentration observed yesterday
tableModel <- merge(tableModel, dataRealDay[,.(date, station, realMaxYesterday)],
                    by=c("date", "station"), all.x=T)

# Weekday
tableModel$weekday <- as.factor(weekdays(tableModel$date))

# Month
tableModel$month <- as.factor(month(tableModel$date))

tableModel[, station := as.factor(station)]


# MODELS ------------------------------------------------------------------

tableTraining <- tableModel[date<=lastTrainingDate]

tableTest <- tableModel[date>lastTrainingDate]

tableDaysToPredict <- tableTest[is.na(alarm)]


# K FOLD SIMPLE MODEL

k <-  3 #Folds

variablesKFSM <- c("realMaxYesterday",
                   "realMedianYesterday")

formulaKFSM <- as.formula(paste0("factor(alarm) ~ ", paste(variablesKFSM, collapse=" + ")))

trainingKFSM <- tableTraining
trainingKFSM <- select(trainingKFSM, "alarm", variablesKFSM) %>% 
                na.omit()
trainingKFSM[, alarm := as.numeric(alarm)]
trainingKFSM$id <- sample(1:k, nrow(trainingKFSM), replace = TRUE)

foldList = 1:k
for (i in 1:k){
    
    trainingSet <- subset(trainingKFSM, id %in% foldList[-i])
    testSet <- subset(trainingKFSM, id %in% c(i))
    
    # Train
    modelKFSM <- glm(formulaKFSM, family=binomial(link='logit'), data=trainingSet)
    
    # Test
    pred <- predict(modelKFSM, testSet)
    real <- testSet$alarm
    
    cat(auc(real,pred),"\n")
    
}

# Making predictions

dtpKFSM <- tableDaysToPredict
dtpKFSM[, alarm := as.numeric(alarm)]

dtpKFSM[, prediction := predict.glm(modelKFSM, dtpKFSM, type="response")]

PrepareOutputFile(FILE_TARGET, dtpKFSM, targetName='prediction', outputFile='results.csv')

metric <- EvaluateResult(submissionFile='results2.csv',
                         pathObs=paste0('../data/data_complete/obs/2015/'),
                         headersObs=FILE_HEADERS_OBS,
                         fileTarget=FILE_TARGET,
                         threshold=THRESHOLD)
message(paste0("KFSM ", metric))


# RANDOM FOREST

variablesRF <- c("station",
                 "predictionTodayMax",
                 "predictionTodayMean",
                 "predictionYesterdayMax",
                 "predictionYesterdayMean",
                 "realYesterday",
                 "realPrev5",
                 "realPrev10",
                 "weekday", 
                 "month")

formulaRF <- as.formula(paste0("factor(alarm) ~ ", paste(variablesRF, collapse=" + ")))

trainingRF <- select(tableTraining, "alarm", variablesRF) %>% 
              na.omit()
trainingRF[, station := as.factor(station)]
trainingRF[, month := as.factor(month)]

modelRF <- randomForest(formulaRF, data=trainingRF)

# Making predictions

dtpRF <- tableDaysToPredict

dtpRF[, prediction := predict(modelRF, dtpRF, type="prob")]

dtpRF[is.na(prediction), prediction := 0.5]

dtpRF(is.na(prediction))

PrepareOutputFile(FILE_TARGET, dtpRF, targetName='prediction', outputFile='results_rf.csv')

metric <- EvaluateResult(submissionFile='results_rf.csv',
                         pathObs=paste0('../data/data_complete/obs/2015/'),
                         headersObs=FILE_HEADERS_OBS,
                         fileTarget=FILE_TARGET,
                         threshold=THRESHOLD)
message(paste0("RF ", metric))


# RANDOM FOREST 2

modelName <- "random_forest_2"

variables <- c("station",
               "realMaxYesterday",
               "realMedianYesterday")

formula <- as.formula(paste0("factor(alarm) ~ ", paste(variables, collapse=" + ")))

training <- select(tableTraining, "alarm", variables) %>% 
            na.omit()

model <- randomForest(formula, data=training)

# Making predictions

dtp <- tableDaysToPredict

dtp[, prediction := predict(model, dtp, type="prob")]

dtp[is.na(prediction), prediction := 0]

PrepareOutputFile(FILE_TARGET, dtp, targetName='prediction',
                  outputFile=paste0('results_', modelName, '.csv'))

metric <- EvaluateResult(submissionFile=paste0('results_', modelName, '.csv'),
                         pathObs=paste0('../data/data_complete/obs/2015/'),
                         headersObs=FILE_HEADERS_OBS,
                         fileTarget=FILE_TARGET,
                         threshold=THRESHOLD)
message(paste0(modelName, " ", metric))

mingometric <- EvaluateResultMingot(submissionFile=paste0('results_', modelName, '.csv'),
                         pathObs=paste0('../data/data_complete/obs/2015/'),
                         headersObs=FILE_HEADERS_OBS,
                         fileTarget=FILE_TARGET,
                         threshold=THRESHOLD)
message(paste0(modelName, " ", mingometric))

# GLM

modelName <- "glm"

variables <- c("realMaxYesterday",
               "realMedianYesterday")

formula <- as.formula(paste0("factor(alarm) ~ ", paste(variables, collapse=" + ")))

training <- select(tableTraining, "alarm", variables) %>% 
    na.omit()

model <- glm(formula, data=training, family="binomial")

# Making predictions

dtp <- tableDaysToPredict

dtp[, prediction := predict.glm(model, dtp, type='response')]

dtp[is.na(prediction), prediction := 0]

PrepareOutputFile(FILE_TARGET, dtp, targetName='prediction',
                  outputFile=paste0('results_', modelName, '.csv'))

metric <- EvaluateResult(submissionFile=paste0('results_', modelName, '.csv'),
                         pathObs=paste0('../data/data_complete/obs/2015/'),
                         headersObs=FILE_HEADERS_OBS,
                         fileTarget=FILE_TARGET,
                         threshold=THRESHOLD)
message(paste0(modelName, " ", metric))


# GLM

modelName <- "glm2"

variables <- c("station",
               "realMaxYesterday",
               "realMedianYesterday")

formula <- as.formula(paste0("factor(alarm) ~ ", paste(variables, collapse=" + ")))

training <- select(tableTraining, "alarm", variables) %>% 
    na.omit()

model <- glm(formula, data=training, family="binomial")

# Making predictions

dtp <- tableDaysToPredict

dtp[, prediction := predict.glm(model, dtp, type='response')]

dtp[is.na(prediction), prediction := 0]

PrepareOutputFile(FILE_TARGET, dtp, targetName='prediction',
                  outputFile=paste0('results_', modelName, '.csv'))

metric <- EvaluateResult(submissionFile=paste0('results_', modelName, '.csv'),
                         pathObs=paste0('../data/data_complete/obs/2015/'),
                         headersObs=FILE_HEADERS_OBS,
                         fileTarget=FILE_TARGET,
                         threshold=THRESHOLD)
message(paste0(modelName, " ", metric))

mingometric <- EvaluateResultMingot(submissionFile=paste0('results_', modelName, '.csv'),
                         pathObs=paste0('../data/data_complete/obs/2015/'),
                         headersObs=FILE_HEADERS_OBS,
                         fileTarget=FILE_TARGET,
                         threshold=THRESHOLD)
message(paste0(modelName, " ", mingometric))


# RANDOM FOREST 3

modelName <- "random_forest_3"

variables <- c("station",
               "realMaxYesterday",
               "realMedianYesterday")

formula <- as.formula(paste0("factor(alarm) ~ ", paste(variables, collapse=" + ")))

training <- select(tableTraining, "alarm", variables) %>% 
    na.omit()

model <- randomForest(formula, data=training)

# Making predictions

dtp <- tableDaysToPredict

dtp[, prediction := predict(model, dtp, type="prob")]

dtp[is.na(prediction), prediction := 0]

PrepareOutputFile(FILE_TARGET, dtp, targetName='prediction',
                  outputFile=paste0('results_', modelName, '.csv'))

metric <- EvaluateResult(submissionFile=paste0('results_', modelName, '.csv'),
                         pathObs=paste0('../data/data_complete/obs/2015/'),
                         headersObs=FILE_HEADERS_OBS,
                         fileTarget=FILE_TARGET,
                         threshold=THRESHOLD)
message(paste0(modelName, " ", metric))

mingometric <- EvaluateResultMingot(submissionFile=paste0('results_', modelName, '.csv'),
                                    pathObs=paste0('../data/data_complete/obs/2015/'),
                                    headersObs=FILE_HEADERS_OBS,
                                    fileTarget=FILE_TARGET,
                                    threshold=THRESHOLD)
message(paste0(modelName, " ", mingometric))

