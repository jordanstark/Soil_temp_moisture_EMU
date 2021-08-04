# Script to remove data when sensors were not deployed

# Jordan Stark, Spring 2021

# run 'ReadBSensors.R' first to generate 'clean_sensordata.csv'

#### Setup ####
  # packages
    library(lubridate)

  # paths
    #Metadata_path <- "C:/Users/Jordan/Desktop/Smokies Data/"
    Metadata_path <- sensormetadata_path
      # this should contain SensorLocationHistory.csv
    
    #Out_path <- "C:/Users/Jordan/Desktop/Smokies Data/CleanSoilData/"
    Out_path <- intermediate_path
      # this should contain 'sensordata.csv'
      # output file 'field_sensordata.csv' will also be saved here
    
  # data import
    SensorData <- read.csv(paste0(Out_path,"sensordata.csv"))
    SensorData$timestamp <- ymd_hms(SensorData$timestamp)
    SensorData[,c("vmc_Deep_raw","vmc_Surf_raw",
                  "stErr","voltErr","vmcsErr","vmcdErr")] <- list(NULL)  

  # metadata import
    SensorLocs <- read.csv(paste(Metadata_path,"SensorLocationHistory.csv",sep=""),stringsAsFactors=F)
    SensorLocs$Deploy_Date <- mdy(SensorLocs$Deploy_Date)
    SensorLocs$Remove_Date <- mdy(SensorLocs$Remove_Date)

    
#### combine data and remove dates before and after deployment ####
  # combine data and metadata
    AllDat <- merge(SensorData,SensorLocs) 
      # this merges by sensorID - so redeployed sensors will have double listings
    
  # remove non-field dates and incorrect duplicates from above
    max_date <- as_date(max(AllDat$timestamp,na.rm=T)) # for sensors not removed
    AllDat$End_Date <- as_date(ifelse(is.na(AllDat$Remove_Date),
                                      max_date,AllDat$Remove_Date))
      # if sensor not removed set end date to last day any sensor recorded
    AllDat <- AllDat[AllDat$timestamp > ymd("2019-01-01"),] # removes NA values
    
    FieldDat <- AllDat[(AllDat$timestamp>AllDat$Deploy_Date +1) & (AllDat$timestamp < AllDat$End_Date -1),]
      # this removes non-deployed times and duplicates from re-deployed sensors
    FieldDat <- FieldDat[!is.na(FieldDat$timestamp),] 
    
#### save data ####
  write.csv(FieldDat,paste0(Out_path,"field_sensordata.csv"),row.names=F)
    