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
    
    write.csv(Clean_data,paste0(Out_path,"cleaned_sensordata.csv"),row.names=F)
