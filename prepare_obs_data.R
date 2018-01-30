# Preparing data for the test period

library(dplyr)
library(data.table)

PATH_FILES_OBS <- "../data/data_complete/obs/2015/"

datesTarget <- read.csv2('2015_test.csv', sep=',') %>% 
               data.table()
datesTarget[, target := NULL]


datesTarget0 <- as.character(seq(from=as.Date('2015-01-03'), to=as.Date('2015-12-31'), by=3))

fileList <- list.files(path=PATH_FILES_OBS, full.names=T, recursive=T)


# 2015 DATA GIVEN TO PARTICIPANTS -----------------------------------------

for(file in fileList){

    stationFile <- substr(file, nchar(file)-14, nchar(file)-8)
    
    datesTargetStation <- datesTarget[station==stationFile, date]
    
    content <- readLines(file)

    dates <- substr(fread(file, sep="\t", encoding="UTF-8")$V14,1,10)
    datesKeep <- !(dates %in% datesTargetStation)

    contentKeep <- content[datesKeep]
    
    if(length(content) != length(dates)){
        stop()
    }
    
    fileOutput <- paste0(stationFile, '_NO2_cut.csv')

    fileConn<-file(fileOutput)
    writeLines(contentKeep, fileConn)
    close(fileConn)
    
}


# 2015 DATA OF TARGET DAYS ------------------------------------------------

for(file in fileList){
    
    content <- readLines(file)
    
    dates <- substr(fread(file, sep="\t", encoding="UTF-8")$V14,1,10)
    datesKeep <- dates %in% datesTarget
    
    contentKeep <- content[datesKeep]
    
    if(length(content) != length(dates)){
        stop()
    }
    
    fileOutput <- paste0("..", strsplit(file, ".", fixed=T)[[1]][3],'_cut.csv')
    
    print
    fileConn<-file(fileOutput)
    writeLines(contentKeep, fileConn)
    close(fileConn)
    
}
