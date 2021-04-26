# Visualization of cleaned, calibrated Smokies soil data
# Dec 2020

#### setup ####
  # packages
    library(lubridate)
    library(ggplot2)
    library(patchwork)
    library(GGally)
    library(lme4)
  
  # paths
    Data_path <- "C:/Users/Jordan/Desktop/Smokies Data/"
  
  # import data
    all_data <- read.csv(paste(Data_path,"CleanSoilData/all_clean_soil.csv",sep=""),stringsAsFactors=F)
    
    all_data$timestamp <- ymd_hms(all_data$timestamp)
    
    gis_data <- read.csv(paste(Data_path,"SiteGISdat.csv",sep=""))[,c("SiteID","max_CC","Elev","log_TCI","Totrad","strdist")]
    
    #all_data <- all_data[all_data$SiteID != "BFR1",]
    
    all_data <- merge(gis_data,all_data,by="SiteID")
    
  # remove anomalously high sensors
    sens_min <- aggregate(vmc_Surf ~ SensorID + SiteID, all_data, min, na.rm=T)
    sens_max <- aggregate(vmc_Surf ~ SensorID + SiteID, all_data, max, na.rm=T)
    
    hist(sens_min$vmc_Surf,breaks=30)
    # using min=0.2 as a reasonable cutoff for now
    hist(sens_max$vmc_Surf,breaks=30)
    # this looks pretty good although may also need a low cutoff
    # most of the very low ones are short-live A sensors

    
    bad_sensors <- sens_min[sens_min$vmc_Surf>0.2,c("SensorID","SiteID")]
    
    clean_data <- all_data[-which(cbind(all_data$SensorID,all_data$SiteID)%in%cbind(bad_sensors$SensorID,bad_sensors$SiteID)),]
    
#### figures ####
    st <- ggplot(clean_data,aes(x=timestamp,y=soiltemp,color=Elev,group=interaction(SensorID,SiteID))) +
            geom_line(size=0.05) +
            theme_classic() +
            scale_x_datetime(date_breaks="2 months",date_labels="%b %Y") +
            xlab("") + ylab("soil temperature (C)") +
            scale_color_distiller(name="Elevation (m)",palette="RdYlBu",direction=1) +
            theme(text=element_text(size=16))
    vmcs <- ggplot(clean_data,aes(x=timestamp,y=vmc_Surf,color=Elev,group=interaction(SensorID,SiteID))) +
              geom_line(size=0.05) +
              theme_classic() +
              scale_x_datetime(date_breaks="2 months",date_labels="%b %Y") +
              xlab("") + ylab("Surface moisture (volumetric)") +
              scale_color_distiller(name="Elevation (m)",palette="RdYlBu",direction=1) +
              theme(text=element_text(size=16))
    
    st/vmcs + plot_layout(guides="collect")
    
    nov <- clean_data[clean_data$timestamp>ymd("2019-11-1") & clean_data$timestamp<ymd("2019-12-1"),]
    
    
    st <- ggplot(nov,aes(x=timestamp,y=soiltemp,color=Elev,group=interaction(SensorID,SiteID))) +
              geom_line(alpha=0.6) +
              theme_classic() +
              scale_x_datetime(date_breaks="1 week",date_labels="%d%b") +
              xlab("") + ylab("soiltemp C") +
              theme(text=element_text(size=16))
    vmcs <- ggplot(nov,aes(x=timestamp,y=vmc_Surf,color=log_TCI,group=interaction(SensorID,SiteID))) +
              geom_line(alpha=0.6) +
              theme_classic() +
              scale_x_datetime(date_breaks="1 week",date_labels="%d%b") +
              xlab("") + ylab("Surface vmc") +
              scale_color_viridis_c()+
              theme(text=element_text(size=16))
    
    st/vmcs
    
    may <- clean_data[clean_data$timestamp>ymd("2020-05-1") & clean_data$timestamp<ymd("2020-06-1"),]
    
    st <- ggplot(may,aes(x=timestamp,y=soiltemp,color=Elev,group=interaction(SensorID,SiteID))) +
              geom_line(alpha=0.6) +
              theme_classic() +
              scale_x_datetime(date_breaks="1 week",date_labels="%d%b") +
              xlab("") + ylab("soiltemp C") +
              theme(text=element_text(size=16))
    vmcs <- ggplot(may,aes(x=timestamp,y=vmc_Surf,color=log_TCI,group=interaction(SensorID,SiteID))) +
              geom_line(alpha=0.6) +
              theme_classic() +
              scale_x_datetime(date_breaks="1 week",date_labels="%d%b") +
              xlab("") + ylab("Surface vmc") +
              scale_color_viridis_c()+
              theme(text=element_text(size=16))
    
    st/vmcs

    
#### summary figs ####
    may_mean <- aggregate(vmc_Surf ~ SensorID + SiteID + log_TCI + Elev + max_CC + Totrad, may, mean, na.rm=T)
    nov_mean <- aggregate(vmc_Surf ~ SensorID + SiteID + log_TCI + Elev + max_CC + Totrad, nov, mean, na.rm=T)
    
    all_mean <- aggregate(vmc_Surf ~ SensorID + SiteID + log_TCI + Elev + max_CC + Totrad, clean_data, mean, na.rm=T)
    
    
    ggplot(may_mean,aes(x=log_TCI,y=vmc_Surf,color=Elev)) +
      geom_point() +
      geom_smooth(method="lm") +
      theme_classic()
    
    ggplot(nov_mean,aes(x=log_TCI,y=vmc_Surf,color=Elev)) +
      geom_point() +
      geom_smooth(method="lm") +
      theme_classic()
    
    ggplot(all_mean,aes(x=log_TCI,y=vmc_Surf,color=Elev)) +
      geom_point() +
      geom_smooth(method="lm") +
      theme_classic()

    
    
    clean_data$day <- yday(clean_data$timestamp)
    
    test <- clean_data[clean_data$SiteID != "BFR1",] # to see how it looks w/o super high TCI site
    
    ggplot(test,aes(x=log_TCI,y=vmc_Surf,group=day)) +
      geom_point(aes(color=FullID)) +
      geom_line(stat="smooth",method="lm",se=F,alpha=0.1,size=0.5,color="black") +
      theme_classic() +
      theme(legend.position="none")
    
    
    ggpairs(nov_mean[,3:7])
    
    
    novmod <- lm(vmc_Surf ~ scale(Elev) + scale(log_TCI),nov_mean)
    summary(novmod)
    
    allmod <- lmer(vmc_Surf ~ scale(Elev) + scale(log_TCI) + (1|day),clean_data)
    
    mod_data <- clean_data[,c("SensorID","timestamp","vmc_Surf","Elev","log_TCI")]
    mod_data$day <- yday(mod_data$timestamp)
    mod_data <- mod_data[complete.cases(mod_data),]
    
    
    bigmod <- lmer(vmc_Surf ~ scale(Elev) + scale(log_TCI) + (1|SensorID) + (1|day),
                   mod_data,REML=F,na.action="na.fail")
    summary(bigmod)
  
    