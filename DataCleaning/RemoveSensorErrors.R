# Script to examine sensor data and check for malfunctions
# run after 'SaveFieldData.R'

# Jordan Stark, Spring 2021

#### Setup ####
  # packages
    library(lubridate)

  
  # paths
    #Metadata_path <- "C:/Users/Jordan/Desktop/Smokies Data/"
    Metadata_path <- sensormetadata_path
      # this should contain 'Bsensor_BadDataList.csv'
  
    #Out_path <- "C:/Users/Jordan/Desktop/Smokies Data/CleanSoilData/"
    Out_path <- intermediate_path
      # this should contain 'field_sensordata.csv'
      # output file 'cleaned_sensordata.csv' will also be saved here
  
  # data import
    SensorData <- read.csv(paste0(Out_path,"field_sensordata.csv"))
    SensorData$timestamp <- ymd_hms(SensorData$timestamp)
    SensorData$Deploy_Date <- ymd(SensorData$Deploy_Date) 
    SensorData$Remove_Date <- ymd(SensorData$Remove_Date)
    SensorData$Check_Date_1 <- mdy(SensorData$Check_Date_1)
    SensorData$Check_Date_2 <- mdy(SensorData$Check_Date_2)

  # import already identified errors
    DataErrors <- read.csv(paste0(Metadata_path,"Bsensor_BadDataList.csv"))
    DataErrors$remove_start <- as_datetime(mdy(DataErrors$remove_start))
    DataErrors$remove_end <- as_datetime(mdy(DataErrors$remove_end))
    
    
  # identify max and min dates for 'na' start/end of errors
    MaxDay <- max(SensorData$Remove_Date,na.rm=T)
    MinDay <- min(SensorData$Deploy_Date,na.rm=T)
    
    
    DataErrors$remove_start[is.na(DataErrors$remove_start)] <- MinDay
    DataErrors$remove_end[is.na(DataErrors$remove_end)] <- MaxDay
    
    
#### remove identified errors #### 

  # function to remove identified errors from data
  
  CleanDataset <- function(DataErrors,SensorData){
                      for(i in 1:length(DataErrors[,1])){
                        setToNA <- which(SensorData$SensorID==DataErrors$SensorID[i] &
                                           SensorData$SiteID==DataErrors$SiteID[i] &
                                           SensorData$timestamp>=DataErrors$remove_start[i] &
                                           SensorData$timestamp<=DataErrors$remove_end[i]+days(1))
                        
                        if(DataErrors$datatype[i]=="st"){
                          SensorData$soiltemp[setToNA] <- NA
                          # also set the vmc to NA since soil temp is used in calculation
                          SensorData$vmc_Deep[setToNA] <- NA
                          SensorData$vmc_Surf[setToNA] <- NA
                        }
                        if(DataErrors$datatype[i]=="vmcs"){
                          SensorData$vmc_Surf[setToNA] <- NA
                        }
                        if(DataErrors$datatype[i]=="vmcd"){
                          SensorData$vmc_Deep[setToNA] <- NA
                        }
                        
                      }  
                      
                      return(SensorData)
                    }
                    
 
    Clean_data <- CleanDataset(DataErrors,SensorData)
    
    
  # add warnings for other potential errors
    Clean_data$warning <- NA
    
    Clean_data$warning[Clean_data$SiteID=="PK1"] <- "not under canopy" 
    Clean_data$warning[Clean_data$SiteID=="GM8"] <- "very high vmcs"
    Clean_data$warning[Clean_data$SiteID=="R4"] <- "low variation in vmcs"
    Clean_data$warning[Clean_data$SiteID=="ATC3"] <- "very high vmcd"
    Clean_data$warning[Clean_data$SiteID=="ATC4.5"] <- "low vmcs"
    Clean_data$warning[Clean_data$SiteID=="ATE02"] <- "odd dips in vmc"
    Clean_data$warning[Clean_data$SiteID=="PK04a"] <- "st was 10 cm deep"
    Clean_data$warning[Clean_data$SiteID=="PK04b"] <- "st and vmcs were 10 cm deep"
    Clean_data$warning[Clean_data$SiteID=="SD6"] <- "vmcd very high, possibly need to remove starting June 2020"
    
  # remove sensors where warnings are a problem
    if(length(remove_list) > 0) Clean_data <- Clean_data[-which(Clean_data$SiteID %in% remove_list),]
    
  # save data
    write.csv(Clean_data,paste0(Out_path,"cleaned_sensordata.csv"),row.names=F)
