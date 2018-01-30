# Functions for datathon.R

ReadStationsInfo <- function(file){
    info <- fread(file,verbose=F, encoding="UTF-8")
    return(info)
}

ReadModelData <- function(path, headersFile){
    headers <- fread(headersFile, verbose=F, encoding="UTF-8")
    filedt <- data.table(fn=list.files(path=path,
                                       full.names=T,
                                       recursive=T))
    filedt[, contents := .(lapply(fn, fread, sep=" "))]
    dt <- filedt[, rbindlist(setNames(contents, fn), idcol="file_id", fill=T)]
    setnames(dt, c('file', names(headers)))
    return(dt)
}

ReadRealData <- function(path, headersFile){
    headers <- fread(headersFile, verbose=F, encoding="UTF-8")
    filedt <- data.table(fn=list.files(path=path,
                                       full.names=T,
                                       recursive=T))
    filedt[, contents := .(lapply(fn, fread, sep="\t", encoding="UTF-8"))]
    dt <- rbindlist(filedt$contents)
    setnames(dt, names(headers))
    dt[, Countrycode := gsub(".*:","",dt$Countrycode)]
    return(dt)
}

CleanModelData <- function(data, stationsInfo){
    data[, file := as.Date(substr(file, nchar(file)-23, nchar(file)-16), format="%Y%m%d")]
    data[, day := as.Date(gsub("-", "", day), format="%Y%m%d")]
    data <- merge(data, stationsInfo, by=c('lon', 'lat'))
    data[, hour := as.integer(substr(hour,1,2))]
    data <- data[, .(predictionDate=file,
                     station=code,
                     date=day,
                     hour,
                     concentration=Concentration)]
    return(data)
}

CleanRealData <- function(data){
    data <- data[, .(station=AirQualityStationEoICode,
                     date=as.Date(DatetimeBegin),
                     hour=as.integer(substr(data$DatetimeBegin,12,13)),
                     concentration=Concentration)]
    return(data)
}

CreateTableWithDateSequence <- function(dateStart, dateEnd=Sys.Date()){
    # Retrieve temperature data from the weather table.
    #
    # Args:
    # dateStart: First date of the sequance.
    # dateEnd: Last date of the sequence.
    #
    # Returns:
    # Data table with one column containing a sequence of dates.
    dateSequence <- seq(from=as.Date(dateStart), to=as.Date(dateEnd), by=1)
    return(data.table(date=dateSequence))
}

MergeStations <- function(table, stationsInfo){
    table <- copy(table)
    table[, aux := 1]
    table <- merge(table, data.table(station=stationsInfo$code, aux=1),
                   by="aux", allow.cartesia=T)
    table[, aux := NULL]
    return(table)
}

CalculateDailyMean <- function(table, variable){
    table <- copy(table)
    setnames(table, variable, "variable")
    table <- table[,.(variableDaily=mean(variable, na.rm=T)),
                   by=.(date, station)]
    setnames(table, "variableDaily", paste0(variable, "Mean"))
    return(table)
}

CalculateDailyMax <- function(table, variable, na.rm=F){
    table <- copy(table)
    setnames(table, variable, "variable")
    table <- table[,.(variableDaily=max(variable, na.rm=na.rm)),
                   by=.(date, station)]
    setnames(table, "variableDaily", paste0(variable, "Max"))
    return(table)
}

CalculateDailyMedian <- function(table, variable, na.rm=F){
    table <- copy(table)
    setnames(table, variable, "variable")
    table <- table[,.(variableDaily=median(variable, na.rm=na.rm)),
                   by=.(date, station)]
    setnames(table, "variableDaily", paste0(variable, "Median"))
    return(table)
}

SeparatePredictionByPredictionDate <- function(table){
    table <- copy(table)
    
    tablePredictionToday <- table[predictionDate == date,]
    tablePredictionToday[, predictionDate := NULL]
    setnames(tablePredictionToday, "concentration", "predictionToday")
    
    tablePredictionYesterday <- table[predictionDate == (date-1),]
    tablePredictionYesterday[, predictionDate := NULL]
    setnames(tablePredictionYesterday, "concentration", "predictionYesterday")
    
    table <- merge(tablePredictionToday, tablePredictionYesterday, by=c("date", "hour", "station"))
    return(table)
}

CalculateRunningMean <- function(table, variable, newName, lag, length){
    table <- copy(table)
    setnames(table, variable, "variable")
    table[, newVariable := rollapply(lag(variable, n=lag), length, FUN=mean, partial=T, align="right", fill=T),
          by="station"]
    setnames(table, c("variable", "newVariable"), c(variable, newName))
    return(table)
}

CalculateOddaRatio <- function(observed, predicted){
    a <- sum(observed & predicted)
    b <- sum(!observed & predicted)
    c <- sum(observed & !predicted)
    d <- sum(!observed & !predicted)
    or <- (a*d) / (b*c)
    return(or)
}

CalculateOddaRatioSkillScore <- function(or){
    orss <- (or - 1) / (or + 1)
    return(orss)
}

PrepareOutputFile <- function(targetFile, table,
                              targetName='prediction', outputFile='results.csv'){
    template <- read.csv(targetFile, sep=',', stringsAsFactors=F) %>% 
                data.table()
    template[, target := NULL]
    template[, date := as.Date(date)]
    setnames(table, targetName, "prediction")
    results <- merge(template, table[, .(date, station, prediction)],
                     by=c('date', 'station'), all.x=T)
    results[, ':=' (date=NULL, station=NULL)]
    write.csv(results, outputFile, row.names=F)
    return(paste0("The ", outputFile, " file has been created."))
}

EvaluateResult <- function(submissionFile,
                           pathObs,
                           headersObs,
                           fileTarget,
                           threshold){

    template <- read.csv(fileTarget, sep=',', stringsAsFactors=F) %>% 
                data.table()
    template[, target := NULL]
    template[, date := as.Date(date)]

    submission <- read.csv(submissionFile) %>% 
                  data.table()
    names(submission) <- 'prediction'
    
    results <- cbind(template, submission)

    reality <- ReadRealData(path=pathObs, headersFile=headersObs) %>% 
               CleanRealData()
    
    reality <- CalculateDailyMax(reality, 'concentration')
    
    results <- merge(results, reality, by=c('date', 'station'), all.x=T)
    
    results[, reality := as.numeric(concentrationMax > threshold)]
    
    results[, metric := -(reality*log(prediction) + (1 - reality)*log(1 - prediction))]
    
    metric <- LogLoss(results$prediction, results$reality)
    
    
    return(metric)
}

EvaluateResultProvisionalScore <- function(submissionFile,
                                           pathObs,
                                           headersObs,
                                           fileTarget,
                                           threshold){
    
    template <- read.csv(fileTarget, sep=',', stringsAsFactors=F) %>% 
        data.table()
    template[, target := NULL]
    template[, date := as.Date(date)]
    
    submission <- read.csv(submissionFile) %>% 
        data.table()
    names(submission) <- 'prediction'
    
    results <- cbind(template, submission)
    
    reality <- ReadRealData(path=pathObs, headersFile=headersObs) %>% 
        CleanRealData()
    
    reality <- CalculateDailyMax(reality, 'concentration')
    
    results <- merge(results, reality, by=c('date', 'station'), all.x=T)
    
    results[, reality := as.numeric(concentrationMax > threshold)]
    
    results[, metric := -(reality*log(prediction) + (1 - reality)*log(1 - prediction))]
    
    results[, test := seq(1:nrow(results))]
    results <- results[(test %% 3)==0]
    
    metric <- LogLoss(results$prediction, results$reality)
    
    return(metric)
}