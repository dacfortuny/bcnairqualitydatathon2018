# Evaluating submissions

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

PATH_FILES_OBS <- "../data/data_complete/obs/2015/"
FILE_HEADERS_OBS <- "../data/data_participants/headers_obs.csv"
FILE_TARGET <- "../data/data_participants/targets.csv"

THRESHOLD <- 100

file <- "../NewSubmissions/submission4.csv"

tempmetric <- EvaluateResultProvisionalScore(submissionFile=file,
                                             pathObs=paste0(PATH_FILES_OBS),
                                             headersObs=FILE_HEADERS_OBS,
                                             fileTarget=FILE_TARGET,
                                             threshold=THRESHOLD)
message(paste0(file, " PROVISIONAL SCORE ", tempmetric))

finalmetric <- EvaluateResult(submissionFile=file,
                         pathObs=paste0(PATH_FILES_OBS),
                         headersObs=FILE_HEADERS_OBS,
                         fileTarget=FILE_TARGET,
                         threshold=THRESHOLD)
message(paste0(file, " FINAL SCORE ", finalmetric))


