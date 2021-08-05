# Data cleaning process for 'B' soil moisture and temp sensors
# Spring 2021


# directories -------------------------------------------------------------


  data_path <-  "C:/Users/Jordan/Desktop/Smokies Data/FieldData/B_sensors/"
  # this should contain all raw sensor data (and nothing else)
  
  Metadata_path <- "C:/Users/Jordan/Desktop/Smokies Data/"
  # this should contain calib_coefs.csv, SensorLocationHistory.csv and Bsensor_BadDataList.csv
  
  Out_path <- "C:/Users/Jordan/Desktop/Smokies Data/CleanSoilData/"
  # this is where 'sensordata.csv', 'field_sensordata.csv' and 'cleaned_sensordata.csv' 
  # will be saved
  # sensordata.csv -- all data
  # field_sensordata.csv -- data while sensors deployed
  # cleaned_sensordata.csv -- errors checked and removed (use this for models)
  
  Script_path <- "C:/Users/Jordan/Desktop/Github/Soil_temp_moisture_EMU/DataCleaning/"
  # where all of the scripts are saved

# packages ----------------------------------------------------------------

  # lubridate -- for dates
  # stringr -- for text
  # ggplot2 and patchwork -- for figures
  
  library(rstudioapi) # to call files

# step 1: import raw data, apply calibration ------------------------------

  #jobRunScript(paste0(Script_path,"ReadBSensors.R"),importEnv=T)
    # note that this currently assumes that surface and 10cm temps are the same


  # wait for this to finish before proceeding
  
# step 2: remove dates when sensors not deployed --------------------------
  
  #jobRunScript(paste0(Script_path,"SaveFieldData.R"),importEnv=T)

  # wait for this to finish before proceeding

# step 3: check data for anomalies ----------------------------------------

  source(paste0(Script_path,"CheckSensorData.R"))    
    # this loads data and four functions:
      # ID_Errors() plots data and diagnostics from a sensor
        # argument 1 is a site name as text (eg "BFR5")
        # argument 2 is the full dataset
        # plot shows five charts (top to bottom)
          # soil temp degrees C
          # daily range of soil temp
          # surface VMC
          # deep VMC
          # surface - deep VMC
        # in each panel, black line is data from the site
          # if a second sensor was present, it will show as a blue line
          # grey lines are data from all other sites
          # if there is an identified error for this sensor, it will show as a red box
            # and error description will print in console
          # if there are any notes when checked, a line with "Ch1" or "Ch2" will show on checked date
            # and the comments will print in console
      # CheckSet() is a wrapper for ID_Errors that allows application to all sites in a dataset
      # ZoomIn() is a wrapper for ID_Errors that allows specification of a start and end date
        # to make it easier to ID start/end of problems
        # argument 1 is the site
        # argument 2 is the full dataset
        # argument 3 is the start date (use the ymd() function to wrap text)
        # argument 4 is the end date (same format as arg 3)
      # CleanDataset() sets identified errors to NA 
          # (start with this if you only want to ID new errors)
          # if you run ID_Errors() on the cleaned data the red box will still appear where data were removed
          # argument 1 is the data frame of errors to be removed
          # argument 2 is the full dataset
          # note that for soil temp errors this also removes soil moisture data
            # because temp was used in the calibration
  
  ID_Errors(site="AT2",fulldat=SensorData)
  
  SensorData_clean <- CleanDataset(DataErrors,SensorData)
  
  ID_Errors(site="AT2",fulldat=SensorData_clean)
  
  ZoomIn("LM7",SensorData,ymd("2020-04-15"),ymd("2020-06-01"))

  #CheckSet(SensorData_clean)

# step 4: save cleaned data -----------------------------------------------


  write.csv(SensorData_clean,paste0(Out_path,"cleaned_sensordata.csv"),row.names=F)
  
# step 5: add any new anomalies to BadSensorData.csv, and repeat steps 3 and 4 as needed ----------


# step 6: check distribution of data to ID water infiltration ----------

  SensorData <- read.csv(paste0(Out_path,"cleaned_sensordata.csv"))
  q05_surf <- aggregate(vmc_Surf ~ SensorID + SiteID,SensorData,quantile,probs=0.05,na.rm=T)
  q05_deep <- aggregate(vmc_Deep ~ SensorID + SiteID,SensorData,quantile,probs=0.05,na.rm=T)

  full_comp <- merge(q05_surf,q05_deep,all=T)  

  
  library(ggplot2)
  library(patchwork)  
  library(ggrepel)  


  
  surf <- ggplot(full_comp,aes(x=vmc_Surf,y=0)) +
            geom_point(alpha=0.4,size=6) +
            geom_label_repel(aes(label=paste0(SiteID,SensorID)),
                             max.overlaps=Inf,box.padding=1) +
            theme_classic() +
            theme(axis.text.y=element_blank()) + ylab("")
  deep <- ggplot(full_comp,aes(x=vmc_Deep,y=0)) +
            geom_point(alpha=0.4,size=6) +
            geom_label_repel(aes(label=paste0(SiteID,SensorID)),
                             max.overlaps=Inf,box.padding=1) +
            theme_classic()+
            theme(axis.text.y=element_blank()) + ylab("")
  ratio <- ggplot(full_comp,aes(x=vmc_Surf/vmc_Deep,y=0)) +
            geom_point(alpha=0.4,size=6) +
            geom_label_repel(aes(label=paste0(SiteID,SensorID)),
                             max.overlaps=Inf,box.padding=1) +
            theme_classic()+
            theme(axis.text.y=element_blank()) + ylab("")

  
  surf / deep / ratio
  
  
  outlier_surf <- c("B14","B04","B02")
  outlier_deep <- c("")  

  
  no_outliers <- SensorData

  no_outliers$vmc_Surf[no_outliers$SensorID %in% outlier_surf] <- NA  
  no_outliers$vmc_Deep[no_outliers$SensorID %in% outlier_deep] <- NA  

  write.csv(no_outliers,paste0(Out_path,"no_outliers_data.csv"))  
  