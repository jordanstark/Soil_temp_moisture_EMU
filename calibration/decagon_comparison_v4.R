# comparing all vwc calibration data


#### setup ####
# libraries
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(MuMIn)
library(lme4)
library(nlme)

# paths
dec_path <- "C://Users/Jordan/Desktop/SoilMoistureCalib/decagon_test_dat/"
calib_path <- "C:/Users/Jordan/Desktop/SoilMoistureCalib/"

#function to import EMU data
CleanEMUDat <- function(dat) {
  errlines <- str_detect(dat,paste("err","loop","read",sep="|"))
  
  cleandat <- dat[errlines==F]
  cleandat <- str_split(cleandat,":",simplify=T)[,2] # remove line numbers
  cleandat <- cleandat[2:length(cleandat)] # remove header
  
  # convert to data frame
  alldf <- data.frame(str_split(cleandat,",",simplify=T),stringsAsFactors=F)
  names(alldf) <- c("y","mo","d","h","m","vmc_Deep_raw","vmc_Surf_raw","soiltemp","power","ID")
  
  # set numeric format for all columns except ID
  alldf[,1:9] <- sapply(alldf[,1:9],as.numeric)
  
  
  # remove NA values and format timestamp
  alldf$timestamp <- ymd_hm(paste(alldf$y,"/",alldf$mo,"/",alldf$d," ",alldf$h,":",alldf$m,sep=""),quiet=T)
  alldf <- alldf[alldf$m==0,] #remove measurements not on the hour
  alldf[,c("y","mo","d","h","m")] <- NULL
  
  alldf$ID <- factor(alldf$ID)
  if(length(levels(alldf$ID)) >1) warning("Multiple ID values in file")
  if(length(levels(alldf$ID))==1) alldf$ID <- levels(alldf$ID) # to fill in NAs
  alldf$ID <- factor(alldf$ID)
  
  # calculate power
  alldf$volts <- alldf$power/1024 * 6.5
  
  # remove NA values for each sensor
  alldf$stErr <- ifelse(alldf$soiltemp==-127,T,F) #missing soil temp values
  alldf$soiltemp[alldf$stErr==T] <- NA
  alldf$vmc_Deep_raw[alldf$stErr==T] <- NA
  alldf$vmc_Surf_raw[alldf$stErr==T] <- NA #st required for vmc reading
  
  alldf$voltErr <- ifelse(alldf$volts<2,T,F) #not reading voltage
  alldf$volts[alldf$voltErr] <- NA
  
  #alldf$vmc_dErr <- ifelse(alldf$vmc_Deep_raw>30000 | alldf$vmc_Deep_raw<10000,T,F)
  #alldf$vmc_Deep_raw[alldf$vmc_dErr] <- NA
  
  #alldf$vmc_sErr <- ifelse(alldf$vmc_Surf_raw>30000 | alldf$vmc_Surf_raw<10000,T,F)
  #alldf$vmc_Surf_raw[alldf$vmc_sErr] <- NA
  
  alldf$frozen <- ifelse(alldf$soiltemp <= 0, T,F)
  alldf$vmc_Deep_raw[alldf$frozen==T] <- NA
  alldf$vmc_Surf_raw[alldf$frozen==T] <- NA
  
  # remove raw values
  alldf$power <- NULL
  alldf$stErr <- NULL
  alldf$voltErr <- NULL
  alldf$vmc_dErr <- NULL
  alldf$vmc_sErr <- NULL
  alldf$frozen <- NULL
  
  return(alldf)
}


#### import and prep data ####
## scale data
  dldat <- read.csv(paste(calib_path,"microcontroller_data.csv",sep=""),stringsAsFactors=F)
  dldat$start_date <- ymd(dldat$start_date)
  dldat$timestamp <- ymd_hms(dldat$timestamp)
  all_dat <- dldat[dldat$SiteID != "LG04" & dldat$SiteID != "AT1.5",] #LG04 looks crazy, AT1.5 goes negative...
  tare_dat <- all_dat[all_dat$SiteID=="empty",]
  
  
  # metadata
  metadata <- read.csv(paste(calib_path,"calib_metadata_clean.csv",sep=""),stringsAsFactors=F)
  metadata$start_date <- dmy(metadata$start_date)
  tare_metadata <- read.csv(paste(calib_path,"tare_metadata.csv",sep=""),stringsAsFactors=F)
  tare_metadata$start_timestamp <- dmy_hm(paste(tare_metadata$start_date, tare_metadata$start_time))
  tare_metadata$end_timestamp <- dmy_hm(paste(tare_metadata$end_date, tare_metadata$end_time))
  
  #### calculate and apply tare weights by temp ####
  tare_dat$chamber_temp <- NA
  
  for(i in 1:length(tare_dat$resist)) {
    if(tare_dat$timestamp[i]>ymd("2020-Aug-26")){
      tare_dat$chamber_temp[i] <- ifelse(anyNA(tare_metadata$start_timestamp<tare_dat$timestamp[i] & tare_metadata$end_timestamp>tare_dat$timestamp[i]),
                                         NA,
                                         tare_metadata$chamber_temp[tare_metadata$start_timestamp<tare_dat$timestamp[i] & tare_metadata$end_timestamp>tare_dat$timestamp[i]])
    }
  }
  
  tare_dat <- tare_dat[complete.cases(tare_dat),]
  
  tare_val <- aggregate(load ~ scale + chamber_temp,tare_dat,mean)
  names(tare_val) <- c("scale","chamber_temp","zero_factor")
  
  
  # set scale factors (determined empirically using hx711_calib_good sketch)
  tare_val$scale_factor <- NA
  
  tare_val$scale_factor[tare_val$scale=="round"] <- 412
  tare_val$scale_factor[tare_val$scale=="round2"] <- 456
  
  # combine data
  all_calibs <- merge(all_dat, metadata)
  all_calibs <- merge(all_calibs,tare_val)
  all_calibs$wet_soil_wt <- (all_calibs$load - all_calibs$zero_factor)/all_calibs$scale_factor
  
  # calculate vmc
  all_calibs$water_wt <- all_calibs$wet_soil_wt - all_calibs$dry_wt
  soil_volume <- mean(5.5^2,6^2) *  4.7 # in mL when filled to the inner line
  all_calibs$vmc <- all_calibs$water_wt/soil_volume #assuming 1g=1mL water
  
  scale_dat <- all_calibs
  scale_dat$obs_type <- "scale"
  
  scale_dat_clean <- scale_dat[,c("timestamp","ID","vmc","resist","soiltemp","obs_type")]
  
  ggplot(scale_dat_clean, aes(x=timestamp,y=vmc,color=soiltemp,group=ID)) +
    geom_line() +
    theme_classic()
  
  
## decagon calibration
  # import decagon data
  dec_dat <- read.csv(paste(dec_path,"decagon_vwc_datecorr_3Dec.csv",sep=""),stringsAsFactors=F)
  
  # select points on the hour and after install time
  dec_dat$Time <- mdy_hm(dec_dat$Time)
  dec_dat$minute <- minute(dec_dat$Time)
  dec_dat_clean <- dec_dat[dec_dat$minute==0,]
  dec_dat_clean$minute <- NULL
  
  # make long form and say which pot is which
  dec_long <- pivot_longer(dec_dat_clean,cols=starts_with("Sensor"),
                           names_to="SensorID",values_to="vwc_decagon")
  dec_long$potID <- NA
  dec_long$potID[dec_long$SensorID=="Sensor_1"] <- "pot1"
  dec_long$potID[dec_long$SensorID=="Sensor_2"] <- "pot2"
  dec_long$potID[dec_long$SensorID=="Sensor_3"] <- "pot3"
  dec_long$potID[dec_long$SensorID=="Sensor_4"] <- "pot4"
  
  
  # import EMU data
  E04_raw <- CleanEMUDat(readLines(paste(dec_path,"E04_3Dec20",sep="")))
  E04_raw$potID <- "pot1"
  
  E01_raw <- CleanEMUDat(readLines(paste(dec_path,"E01_3Dec20",sep="")))
  E01_raw$potID <- "pot2"
  
  E03_raw <- CleanEMUDat(readLines(paste(dec_path,"E03_3Dec20",sep="")))
  E03_raw$potID <- "pot3"
  
  C20_raw <- CleanEMUDat(readLines(paste(dec_path,"C20_3Dec20",sep="")))
  C20_raw$potID <- "pot4"
  
  All_emu_dat <- rbind(E04_raw,E01_raw,E03_raw,C20_raw)
  names(All_emu_dat) <- c("res_deep","res_surf","soiltemp","EMU_ID","Time","volts","potID")
  
  
  
  
  # combine all data
  alldat <- merge(All_emu_dat,dec_long,all=T)
  
  all_long <- alldat[,c("Time","potID","res_deep","res_surf","soiltemp","vwc_decagon")]
  all_long <- pivot_longer(all_long,cols=starts_with("res"),
                           values_to="resist",names_to="sensortype")
  
  baseline_dat <- all_long[all_long$Time < mdy_hm("10/29/2020 10:45"),]
  
  all_long <- all_long[all_long$Time > mdy_hm("10/29/2020 13:45") & all_long$Time < mdy_hm("12/3/2020 14:00"),] #installed data
  all_long <- all_long[all_long$potID != "pot4",] # broken sensor
  all_long <- all_long[complete.cases(all_long),]
  
  dec_dat <- all_long
  names(dec_dat) <- c("timestamp","potID","soiltemp","vmc","sensortype","resist")
  dec_dat$obs_type <- "decagon"
  
  
  
  # process decagon dat to remove temp effect on decagon probes
  dec_dat$TrialID <- NA

  dec_dat$TrialID[dec_dat$timestamp < ymd_hms("2020-11-03 12:00:00")] <- "t1"

  dec_dat$TrialID[dec_dat$potID=="pot1" & dec_dat$timestamp > ymd_hms("2020-11-05 11:00:00") & dec_dat$timestamp < ymd("2020-11-12")] <- "t2"
  dec_dat$TrialID[dec_dat$potID=="pot2" & dec_dat$timestamp > ymd_hms("2020-11-05 11:00:00") & dec_dat$timestamp < ymd("2020-11-12")] <- "t2"
  dec_dat$TrialID[dec_dat$potID=="pot3" & dec_dat$timestamp > ymd_hms("2020-11-06 15:00:00") & dec_dat$timestamp < ymd("2020-11-12")] <- "t2"
  
  dec_dat$TrialID[dec_dat$timestamp > ymd_hm("2020-11-23 00:00") & dec_dat$timestamp < ymd_hm("2020-12-01 12:00")] <- "t3"
    #removing some of beginning when decagon probe overwhelmed and end when stably dry
  
  dec_dat$ID <- paste(dec_dat$potID,dec_dat$sensortype,dec_dat$TrialID,sep=".")
  
  dec_dat_clean <- dec_dat[complete.cases(dec_dat),]
  
  dec_dat_clean <- dec_dat_clean[,c("timestamp","ID","vmc","resist","soiltemp","obs_type","TrialID","potID","sensortype")]
  
  
  
  dec_vartemp <- dec_dat[dec_dat$TrialID=="t2" | dec_dat$TrialID=="t3",]
  dec_vartemp$vmc_smooth <- NA
  
  dec_vartemp <- dec_vartemp[order(dec_vartemp$timestamp),]
  
  dec_bypot <- split(dec_vartemp,dec_vartemp$ID)
  
  for(i in 1:length(dec_bypot)) {
    dec_bypot[[i]]$vmc_smooth <- as.numeric(filter(dec_bypot[[i]]$vmc,rep(1,24))/24)
    
  }
  
  
  all_tempvar <- do.call(rbind,dec_bypot)
  
  ggplot(all_tempvar, aes(x=timestamp,color=ID)) +
    geom_line(aes(y=vmc), linetype=2) +
    geom_line(aes(y=vmc_smooth)) +
    theme_classic()
  
  
  all_tempvar$vmc <- NULL
  
  names(all_tempvar) <- c("timestamp","potID","soiltemp","sensortype","resist","obs_type","TrialID","ID","vmc")

  
  dec_tempinvar <- dec_dat_clean[dec_dat_clean$TrialID=="t1",]
  
  all_dec <- merge(all_tempvar,dec_tempinvar,all=T)
  
  all_dec <- all_dec[complete.cases(all_dec),]


#### combine data ####
  
  full_data <- merge(scale_dat_clean,all_dec,all=T)

  full_data$resist.sq <- full_data$resist^2
  full_data$resist.cu <- full_data$resist^3
  full_data$soiltemp.sq <- full_data$soiltemp^2
  full_data$soiltemp.cu <- full_data$soiltemp^3
  
  
  full_data <- full_data[order(full_data$timestamp),]

  
  ggplot(full_data,aes(x=resist,y=vmc,color=soiltemp)) +
    geom_path(aes(group=ID)) +
    theme_classic()

  ggplot(full_data,aes(x=resist,y=vmc,color=obs_type,group=ID)) +
    geom_path(alpha=0.5) +
    theme_classic()
  
  ggplot(all_dec,aes(x=timestamp,y=vmc,color=TrialID,group=ID)) +
    geom_point(alpha=0.5)+
    theme_classic()
  
#### model ####
  dec_only <- full_data[full_data$obs_type=="decagon",]
  dec_mod <- lmer(vmc ~ resist + resist.sq + resist.cu + soiltemp + soiltemp.sq + soiltemp.cu + soiltemp:resist + (1|ID), 
                  dec_only,na.action="na.fail",REML=F)
  dredge(dec_mod)
   # best is without slt.sq, then without slt.cu (delta 0.78), then full model (delta 1.98)
  best_dec <- lmer(vmc ~ resist + resist.sq + resist.cu + soiltemp + soiltemp.cu + soiltemp:resist + (1|ID), 
                   dec_only,na.action="na.fail",REML=F)
  
  
  scale_only <- full_data[full_data$obs_type=="scale",]
  scale_mod <- lmer(vmc ~ resist + resist.sq + resist.cu + soiltemp + soiltemp.sq + soiltemp.cu + soiltemp:resist + (1|ID), 
                    scale_only,na.action="na.fail",REML=F)
  dredge(scale_mod)
   # best is full model
  best_scale <- scale_mod
  

  
  
  
  full_mod <- lmer(vmc ~ resist + resist.sq + resist.cu + soiltemp + soiltemp.sq + soiltemp.cu + soiltemp:resist + (1|ID), 
                        full_data,na.action="na.fail",REML=F)
  dredge(full_mod)
  # best model is without slt.cu or slt.sq. Many within delta 2 but none more parsimonious (removing just slt.cu or just slt.sq)
  
  full_nlme <- lme(fixed = vmc ~ resist + resist.sq + resist.cu + soiltemp + soiltemp.sq + soiltemp.cu + soiltemp:resist, random = ~1|ID, 
                        full_data,na.action="na.fail",method="ML")
  
  summary(full_nlme) # none of the temp vars significant, resist also not sig
  
  simpler_nlme <- lme(fixed = vmc ~ resist + resist.sq + resist.cu + soiltemp + soiltemp:resist, random = ~1|ID, 
                      full_data,na.action="na.fail",method="ML")
  
  summary(simpler_nlme) # all significant except soiltemp
  
  anova(full_nlme,simpler_nlme)
  #not significantly different
  
  best_full <- lmer(vmc ~ resist + resist.sq + resist.cu + soiltemp + soiltemp:resist + (1|ID), 
                    full_data,na.action="na.fail",REML=F)
  dredge(best_full) #next best model removes interaction, delta AIC of 3.56
  
  # visualize model
  model_vis <- data.frame(resist = rep(seq(10000,25000,by=1000),each=6),
                          soiltemp = rep(c(10,15,20,25,30,35),16))
  
  model_vis$resist.sq <- model_vis$resist^2
  model_vis$resist.cu <- model_vis$resist^3
  
  model_vis$soiltemp.sq <- model_vis$soiltemp^2
  model_vis$soiltemp.cu <- model_vis$soiltemp^3
  
  
  model_vis$pred_scale <- predict(best_scale,model_vis,re.form=NA)
  model_vis$pred_dec <- predict(best_dec,model_vis,re.form=NA)
  model_vis$pred_full <- predict(best_full,model_vis,re.form=NA)
  
  
  model_vis$soiltemp_cat <- factor(model_vis$soiltemp)
  #model_vis$soiltemp <- NULL
  
  model_vis$mickley <- 0.52 - 2.46*10^-5 * model_vis$resist + 2.54*10^-10 * model_vis$resist.sq
  
  
  ggplot(model_vis,aes(x=resist)) +
    geom_line(aes(y=pred_full,color=soiltemp_cat),linetype=1) +
    geom_line(aes(y=pred_scale,color=soiltemp_cat),linetype=2) +
    geom_line(aes(y=pred_dec,color=soiltemp_cat),linetype=3) +
    geom_line(aes(y=mickley)) +
    theme_classic()

  
  ggplot(model_vis,aes(x=resist)) +
    geom_line(aes(y=pred_dec,color=soiltemp_cat)) +
    #geom_line(aes(y=mickley)) +
    geom_path(data=dec_only,aes(x=resist,y=vmc,group=ID),alpha=0.15) +
    theme_classic() + ggtitle("decagon only")
  
  ggplot(model_vis,aes(x=resist)) +
    geom_line(aes(y=pred_scale,color=soiltemp_cat)) +
    #geom_line(aes(y=mickley)) +
    geom_path(data=scale_only,aes(x=resist,y=vmc,group=ID),alpha=0.15) +
    theme_classic() + ggtitle("balance only")
  
  ggplot(model_vis,aes(x=resist)) +
    geom_line(aes(y=pred_full,color=soiltemp_cat)) +
    #geom_line(aes(y=mickley)) +
    geom_path(data=full_data,aes(x=resist,y=vmc,group=ID),alpha=0.15) +
    theme_classic() + ggtitle("all data")
  
  ggplot(model_vis,aes(x=resist)) +
    geom_line(aes(y=pred_full,color=soiltemp,group=soiltemp_cat)) +
    geom_line(aes(y=mickley),linetype=2) +
    #geom_path(data=full_data,aes(x=resist,y=vmc,group=ID,color=soiltemp),alpha=0.5) +
    theme_classic() + 
    theme(text=element_text(size=20)) +
    xlab("resistance reading") + ylab("volumetric moisture content") +
    scale_color_gradient(low="blue",high="red")
  
  ## visualize temp effect
  model_vis2 <- data.frame(resist = rep(seq(10000,26000,by=2000),each=26),
                          soiltemp = rep(seq(10,35,1),9))
  
  model_vis2$resist.sq <- model_vis2$resist^2
  model_vis2$resist.cu <- model_vis2$resist^3
  
  model_vis2$soiltemp.sq <- model_vis2$soiltemp^2
  model_vis2$soiltemp.cu <- model_vis2$soiltemp^3
  
  
  model_vis2$pred_scale <- predict(best_scale,model_vis2,re.form=NA)
  model_vis2$pred_dec <- predict(best_dec,model_vis2,re.form=NA)
  model_vis2$pred_full <- predict(best_full,model_vis2,re.form=NA)
  
  model_vis2$mickley <- 0.52 - 2.46*10^-5 * model_vis2$resist + 2.54*10^-10 * model_vis2$resist.sq
  
  model_vis2$resist_cat <- factor(model_vis2$resist)
  model_vis2$resist <- NULL
  
  ggplot(model_vis2,aes(x=soiltemp)) +
    geom_line(aes(y=pred_full,color=resist_cat),linetype=1) +
    #geom_line(aes(y=pred_scale,color=resist_cat),linetype=2) +
    #geom_line(aes(y=pred_dec,color=resist_cat),linetype=3) +
    #geom_line(aes(y=mickley)) +
    theme_classic()
  
  
## check model fit
  test_dat <- full_data
  test_dat$ID <- NULL
  test_dat$predicted <- predict(best_full,test_dat,re.form=NA)
  MAE <- mean(abs(test_dat$predicted-test_dat$vmc))
  MAE
  RMSE <- sqrt((sum((test_dat$predicted-test_dat$vmc)^2))/length(test_dat$predicted))
  RMSE
  # MAE = 0.033, RMSE = 0.038
  
  test_dat$resid <- test_dat$predicted - test_dat$vmc
  
  test_dat$ID <- full_data$ID
  
  test_dat <- test_dat[order(test_dat$timestamp),]
  
  
  ggplot(test_dat,aes(x=soiltemp,y=resid,color=ID)) +
    geom_path(aes(group=ID)) +
    geom_point() +
    theme_classic() + theme(legend.position="none")
  
  ggplot(test_dat,aes(x=resist,y=resid,color=ID)) +
    geom_path() +
    theme_classic() + theme(legend.position="none")
  
 # outputs
  coefs <- summary(best_full)$coefficients[,"Estimate"]
  #write.csv(coefs,paste(calib_path,"all_tests_coefs_23Mar.csv",sep=""),row.names=T)
  