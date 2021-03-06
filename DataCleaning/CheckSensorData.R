# Script to examine sensor data and check for malfunctions
# run after 'SaveFieldData.R'

# Jordan Stark, Spring 2021

#### Setup ####
  # packages
    library(lubridate)
    library(ggplot2)
    library(patchwork)
  
  # paths
    #Metadata_path <- "C:/Users/Jordan/Desktop/Smokies Data/"
      # this should contain 'Bsensor_BadDataList.csv'
  
    #Out_path <- "C:/Users/Jordan/Desktop/Smokies Data/CleanSoilData/"
      # this should contain 'field_sensordata.csv'
      # output file 'cleaned_sensordata.csv' will also be saved here
  
  # data import
    SensorData <- read.csv(paste0(Out_path,"field_sensordata.csv"))
    SensorData$timestamp <- ymd_hms(SensorData$timestamp)
    SensorData$Deploy_Date <- ymd(SensorData$Deploy_Date) 
    SensorData$Remove_Date <- ymd(SensorData$Remove_Date)
    SensorData$Check_Date_1 <- mdy(SensorData$Check_Date_1)
    SensorData$Check_Date_2 <- mdy(SensorData$Check_Date_2)
  
  # identify max and min dates for 'na' start/end of errors
    MaxDay <- max(SensorData$Remove_Date,na.rm=T)
    MinDay <- min(SensorData$Deploy_Date,na.rm=T)

  # import already identified errors
    DataErrors <- read.csv(paste0(Metadata_path,"Bsensor_BadDataList.csv"))
    DataErrors$remove_start <- as_datetime(mdy(DataErrors$remove_start))
    DataErrors$remove_end <- as_datetime(mdy(DataErrors$remove_end))
    
    DataErrors$remove_start[is.na(DataErrors$remove_start)] <- MinDay
    DataErrors$remove_end[is.na(DataErrors$remove_end)] <- MaxDay
    
#### function to plot sensor data and ID problems ####
  
  # colors if there are multiple sensors per site  
  cols <- c("black","blue","green")
    

  
  ID_Errors <- function(site,fulldat) {
    fulldat$vmc_diff <- fulldat$vmc_Surf - fulldat$vmc_Deep
    
    data <- fulldat[fulldat$SiteID==site,]
    errors <- DataErrors[DataErrors$SiteID==site,]
    checks <- unique(data[,grep("Check",names(data),value=T)])
    
    ref_data <- fulldat[fulldat$SiteID!=site,]

    
    daily_st_range <- aggregate(soiltemp ~ as_date(timestamp) + SensorID + SiteID,fulldat,
                                FUN=function(x) max(x,na.rm=T) - min(x,na.rm=T))
    names(daily_st_range) <- c("timestamp","SensorID","SiteID","soiltemp")
    daily_st_range$timestamp <- as_datetime(daily_st_range$timestamp)
    
    
    site_st_range <- daily_st_range[daily_st_range$SiteID==site,]
    ref_st_range <- daily_st_range[daily_st_range$SiteID!=site,]


    
    st_plot <- ggplot(data,aes(x=timestamp,y=soiltemp,color=SensorID,group=SensorID)) +
      geom_line(data=ref_data,color="grey",size=0.1,alpha=0.4) +
      geom_line() +
      scale_color_manual(values=cols) +
      scale_x_datetime(date_labels="%b %Y") +
      theme_classic() +
      theme(legend.position="none",
            axis.text.x=element_blank()) +
      labs(y="st",x="",title=paste0(site,": ",unique(data$SensorID)))
    
    range_st_plot <- ggplot(site_st_range,aes(x=timestamp,y=soiltemp,color=SensorID,group=SensorID)) +
      geom_line(data=ref_st_range,color="grey",size=0.1,alpha=0.4) +
      geom_line() +
      scale_color_manual(values=cols) +
      scale_x_datetime(date_labels="%b %Y") +
      theme_classic() +
      theme(legend.position="none",
            axis.text.x=element_blank()) +
      labs(y="st range",x="")
    
    
    vmcs_plot <- ggplot(data,aes(x=timestamp,y=vmc_Surf,color=SensorID,group=SensorID)) +
      geom_line(data=ref_data,color="grey",size=0.1,alpha=0.4) +
      geom_line() +
      scale_color_manual(values=cols) +
      scale_x_datetime(date_labels="%b %Y") +
      theme_classic() +
      theme(legend.position="none",
            axis.text.x=element_blank()) +
      labs(y="vmcs",x="")
    
    vmcd_plot <- ggplot(data,aes(x=timestamp,y=vmc_Deep,color=SensorID,group=SensorID)) +
      geom_line(data=ref_data,color="grey",size=0.1,alpha=0.4) +
      geom_line() +
      scale_color_manual(values=cols) +
      scale_x_datetime(date_labels="%b %Y") +
      theme_classic() +
      theme(legend.position="none",
            axis.text.x=element_blank()) +
      labs(y="vmcd",x="")
    
    vmc_diff_plot <- ggplot(data,aes(x=timestamp,y=vmc_diff,color=SensorID,group=SensorID)) +
      geom_line(data=ref_data,color="grey",size=0.1,alpha=0.4) +
      geom_line() +
      scale_color_manual(values=cols) +
      scale_x_datetime(date_labels="%b %Y") +
      theme_classic() +
      theme(legend.position="none") +
      labs(y="vmc diff",x="")
    
    
    if("st" %in% errors$datatype){
      st_err <- errors[errors$datatype=="st",]
      st_plot <- st_plot + geom_rect(data=st_err,
                                     aes(xmin=remove_start,xmax=remove_end,
                                         ymin=-Inf,ymax=Inf),
                                     alpha=0.1,
                                     fill="red",
                                     inherit.aes=F)
      
      range_st_plot <- range_st_plot + geom_rect(data=st_err,
                                                 aes(xmin=remove_start,xmax=remove_end,
                                                     ymin=-Inf,ymax=Inf),
                                                 alpha=0.1,
                                                 fill="red",
                                                 inherit.aes=F)
      
      
      
      print(paste0("st_err: ", st_err$reason))
    }
    if("vmcs" %in% errors$datatype){
      vmcs_err <- errors[errors$datatype=="vmcs",]
      vmcs_plot <- vmcs_plot + geom_rect(data=vmcs_err,
                                         aes(xmin=remove_start,xmax=remove_end,
                                             ymin=-Inf,ymax=Inf),
                                         alpha=0.1,
                                         fill="red",
                                         inherit.aes=F)
      print(paste0("vmcs_err: ", vmcs_err$reason))
    }
    if("vmcd" %in% errors$datatype){
      vmcd_err <- errors[errors$datatype=="vmcd",]
      vmcd_plot <- vmcd_plot + geom_rect(data=vmcd_err,
                                         aes(xmin=remove_start,xmax=remove_end,
                                             ymin=-Inf,ymax=Inf),
                                         alpha=0.1,
                                         fill="red",
                                         inherit.aes=F)
      print(paste0("vmcd_err: ", vmcd_err$reason))
    }
    
    full_plot <- st_plot / range_st_plot / vmcs_plot / vmcd_plot / vmc_diff_plot
    
    if(!is.na(checks$Check_Date_1)) {
      full_plot <- full_plot & 
        geom_vline(data=checks,aes(xintercept=as_datetime(Check_Date_1))) &
        annotate(geom="text",label="Ch1",y=Inf,
                 x=as_datetime(checks$Check_Date_1),
                 vjust=1,hjust=1)
      print(paste0("Ch1: ",checks$Check_Notes_1))
    }
    if(!is.na(checks$Check_Date_2)) {
      full_plot <- full_plot & 
        geom_vline(data=checks,aes(xintercept=as_datetime(Check_Date_2))) &
        annotate(geom="text",label="Ch1",y=Inf,
                 x=as_datetime(checks$Check_Date_2),
                 vjust=1,hjust=1)
      print(paste0("Ch2: ",checks$Check_Notes_2))
    }
    
    print(full_plot)
    
  } 
  
  
  
  
  
  # function to check a set of sensors
  CheckSet <- function(fulldata) {

    sites <- unique(fulldata$SiteID)
    
    for(i in 1:length(sites)){
      ID_Errors(sites[i],fulldata)
      readline("press enter for next plot")
    }
    
  }
  
  # function to zoom in on a section
  ZoomIn <- function(site,fulldata,start_date,end_date){
    short_data <- fulldata[fulldata$timestamp >= start_date & 
                           fulldata$timestamp <= end_date,]
  
    plot <- ID_Errors(site,short_data)
    
    print(plot & scale_x_datetime(limits=c(as_datetime(start_date),
                                           as_datetime(end_date))))
  }
  

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
                    
 
