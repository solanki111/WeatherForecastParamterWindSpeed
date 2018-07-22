library(GGally)
library(dplyr)
library(colortools)
library(ggplot2)
library(Metrics)

winds <- read.csv("C:/Users/solab/OneDrive/Documents/Data_Science_Notes/Dissertation_in_Stats/Initial_files/All_data.csv", header=T, row.names=NULL, na.strings=c(""))
summary(winds)
View(winds)

winds.df <- as.data.frame(winds)
winds.df$date <- as.Date(winds.df$date, format="%d-%m-%Y")
#winds.df$forecast_period <- as.numeric(winds.df$forecast_period)
#winds.df$station_id <- as.numeric(winds.df$station_id)
winds.df$wdir_fore <- as.numeric(levels(winds.df$wdir_fore)[winds.df$wdir_fore]) 
winds.df$wdir <- as.numeric(levels(winds.df$wdir)[winds.df$wdir]) 
winds.df$wspeed <- as.numeric(levels(winds.df$wspeed)[winds.df$wspeed]) 
winds.df$wspeed_fore <- as.numeric(levels(winds.df$wspeed_fore)[winds.df$wspeed_fore])


#To convert knots to m/s mltiply by 0.514444444
winds.df$wspeed <- (winds.df$wspeed*0.514444444)
View(winds.df)

# Subsetting and selecting forecast period 1
winds.subset <- winds.df[ ,1:7]
winds.fp1 <- winds.subset[winds.subset$forecast_period==1,]
winds.fp10 <- winds.subset[winds.subset$forecast_period==10,]
winds.fp1.st1 <- winds.fp1[winds.fp1$station_id==1,]

winds.fp1.st1.cc <- winds.fp1.st1[complete.cases(winds.fp1.st1), ]
winds.fp1.cc <- winds.fp1[complete.cases(winds.fp1), ]
winds.fp10.cc <- winds.fp10[complete.cases(winds.fp10), ]

winds.fp10.cc$year <- format(winds.fp10.cc$date, format = "%Y")
winds.fp10.cc$year <- as.factor(as.character(winds.fp10.cc$year))


#aggregate of wind speed by column year & station Id
winds.fp10.cc.agg.year <- aggregate(winds.fp10.cc, 
                                   by=list(year=winds.fp10.cc$year,
                                           std_id=winds.fp10.cc$station_id), 
                                   FUN=mean)
print(head(winds.fp10.cc.agg.year))

print(ggplot(winds.fp10.cc.agg.year, 
             aes(year, wspeed, group=std_id, color=as.factor(std_id))) +
        geom_line() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) +
        labs( x = "Years", color = "Station Id")+ 
        scale_y_continuous("Observed Wind Speed", limits=c(0, 8)) +
        ggtitle(paste("Observed Wind speed over the years of all weather stations for Forecast Period: 10"
        )))

View(winds.fp10.cc.agg.year)
View(winds.fp1.cc.agg.year)
View(winds.fp1.st1.cc)


#Creating extra columns like months in character n value and year 
winds.fp1$months <- months(winds.fp1$date)
winds.fp1$months <- as.factor(as.character(winds.fp1$months))
winds.fp1$month_value <- format(winds.fp1$date, format = "%m")
winds.fp1$month_value <- as.numeric(as.character(winds.fp1$month_value))
winds.fp1$year <- format(winds.fp1$date, format = "%Y")
winds.fp1$year <- as.factor(as.character(winds.fp1$year))


#Removing incomplete rows
winds.fp1.cc <- winds.fp1[complete.cases(winds.fp1), ]
winds.fp1.cc$year_month <- paste(winds.fp1.cc$year, sprintf("%02d", winds.fp1.cc$month_value), sep="-" )


#Taking mean of wind speed in different scenarios like...
#aggregate of wind speed by column year_mon & station Id
winds.fp1.cc.agg.year_mon <- aggregate(winds.fp1.cc, 
                                  by=list(year_month=winds.fp1.cc$year_month,
                                          std_id=winds.fp1.cc$station_id),
                                  FUN=mean)

#aggregate of wind speed by column year & station Id
winds.fp1.cc.agg.year <- aggregate(winds.fp1.cc, 
                                   by=list(year=winds.fp1.cc$year,
                                           std_id=winds.fp1.cc$station_id), 
                                   FUN=mean)


#aggregate of wind speed by column month value & station Id
winds.fp1.cc.agg.mon <- aggregate(winds.fp1.cc, 
                                  by=list(month_value=winds.fp1.cc$month_value,
                                          std_id=winds.fp1.cc$station_id), 
                                  FUN=mean)
##aggregate of wind speed by column month value & year
winds.fp1.cc.agg.std_id <- aggregate(winds.fp1.cc, 
                                  by=list(month_value=winds.fp1.cc$month_value,
                                          year=winds.fp1.cc$year), 
                                  FUN=mean)

#View data
View(winds.fp1.cc.agg.year_mon)
View(winds.fp1.cc.agg.mon)
View(winds.fp1.cc.agg.year)
View(winds.fp1.cc.agg.std_id)


# Plots to visualise the above scenarios
winds.fp1.cc.agg.year_mon2 <- winds.fp1.cc.agg.year_mon[winds.fp1.cc.agg.year_mon$year_month > "2011-12", ] 

ggplot(winds.fp1.cc.agg.year_mon2, 
  aes(year_month, wspeed, group=std_id, color=as.factor(std_id))) +
  geom_line() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) +
  labs(title='Forecast Period 1: Observed Wind speed for year 2013 for all weather stations', 
       x = "Year-Month", 
       y = "Observed Wind Speed", 
       color = "Station Id")

ggplot(winds.fp1.cc.agg.year, 
       aes(year, wspeed, group=std_id, color=as.factor(std_id))) +
  geom_line() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) +
  labs(title='Forecast Period 1: Observed Wind speed over the years for all weather stations', 
        x = "Years", 
        #y = "Observed Wind Speed", 
        color = "Station Id")+
        scale_y_continuous("Observed Wind Speed", limits=c(0, 8))

ggplot(winds.fp1.cc.agg.mon, 
       aes(as.factor(month_value), wspeed, group=std_id, color=as.factor(std_id))) +
  geom_line() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) +
  labs(title='Forecast Period 1: Observed Wind speed over the months for all weather stations', 
       x = "Months", 
       y = "Observed wind speed", 
       color = "Station Id")

#winds.fp1.cc.agg.std_id$months <- months(winds.fp1.cc.agg.std_id$date)
ggplot(winds.fp1.cc.agg.std_id, 
       aes(as.factor(month_value), wspeed, group=year, color=year)) +
  geom_line() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) +
  labs(title='Forecast Period 1: Observed Wind speed as per months over the years', 
       x = "Months", 
       y = "Observed wind speed", 
       color = "Years")


#--------------------------------------------------------------------------#
#FP2_wind_speed_by_year_months_over_2_years_for_all_stations
#To find out the missing data for all forecast periods and all weather stations

for(fp in 1:10) {
  print(paste("For forecast period:",fp))
  for(stat in 1:25) {
      
      winds.fp <- winds.subset[winds.subset$forecast_period==fp,]
      winds.fp.cc <- winds.fp[complete.cases(winds.fp), ]
      winds.fp.stat <- winds.fp.cc[winds.fp.cc$station_id==stat,]
      print(paste("For Station id:",stat))
      print(min(winds.fp.stat$date))
      print(max(winds.fp.stat$date))
  }
}

#To make the yearly distribution of observed wind speed for all weather stations
for(fp in 1:10) {
  print(paste("For forecast period:",fp))

      #Filtering as per the forecast period
      winds.fp <- winds.subset[winds.subset$forecast_period==fp,]
      
      #Removing incomplete rows
      winds.cc.fp <- winds.fp[complete.cases(winds.fp), ]
      
      #Creating extra columns like months in character n value and year 
      winds.cc.fp$year <- format(winds.cc.fp$date, format = "%Y")
      winds.cc.fp$year <- as.factor(as.character(winds.cc.fp$year))
      
      #Taking mean of wind speed in different scenarios like...
      #aggregate of wind speed by column year & station Id
      winds.cc.agg.fp <- aggregate(winds.cc.fp, 
                                         by=list(year=winds.cc.fp$year,
                                                 std_id=winds.cc.fp$station_id), 
                                         FUN=mean)
      # Plots to visualise graphically
      print(ggplot(winds.cc.agg.fp, 
            aes(year, wspeed, group=std_id, color=as.factor(std_id))) +
            geom_line() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) +
            labs( x = "Years", color = "Station Id")+ 
            scale_y_continuous("Observed Wind Speed", limits=c(0, 8)) +
            ggtitle(paste("Observed Wind speed over the years of all weather stations for Forecast Period:", fp
            )))

}

#To make the yearly distribution of predicted wind speed for all weather stations
for(fp in 1:10) {
  print(paste("For forecast period:",fp))
  
  #Filtering as per the forecast period
  winds.fp <- winds.subset[winds.subset$forecast_period==fp,]
  
  #Removing incomplete rows
  winds.cc.fp <- winds.fp[complete.cases(winds.fp), ]
  
  #Creating extra columns like months in character n value and year 
  winds.cc.fp$year <- format(winds.cc.fp$date, format = "%Y")
  winds.cc.fp$year <- as.factor(as.character(winds.cc.fp$year))
  
  #Taking mean of wind speed in different scenarios like...
  #aggregate of wind speed by column year & station Id
  winds.cc.agg.fp <- aggregate(winds.cc.fp, 
                               by=list(year=winds.cc.fp$year,
                                       std_id=winds.cc.fp$station_id), 
                               FUN=mean)
  # Plots to visualise graphically
  print(ggplot(winds.cc.agg.fp, 
               aes(year, wspeed_fore, group=std_id, color=as.factor(std_id))) +
          geom_line() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) +
          labs( x = "Years", color = "Station Id")+ 
          scale_y_continuous("Predicted Wind Speed", limits=c(0, 8)) +
          ggtitle(paste("Predicted Wind speed over the years of all weather stations for Forecast Period:", fp
          )))
  
}

#Visualising scatter plot between observed and predicted wind speed for each forecast periods
#and also doing error computation
for(fp in 1:10) {
  print(paste("For forecast period:",fp))
  
  #Filtering as per the forecast period
  winds.fp <- winds.subset[winds.subset$forecast_period==fp,]
  
  #Removing incomplete rows
  winds.fp.cc <- winds.fp[complete.cases(winds.fp), ]
  
  winds.fp.cc.agg <- aggregate(winds.fp.cc, 
                                by=list(date=winds.fp.cc$date), 
                                FUN=mean)

  #gp<- ggplot(data= winds.fp.cc.agg, mapping= aes(wspeed_fore, wspeed)) + 
  #        geom_point(color="blue") +
  #        geom_abline(intercept = 0, slope = 1, color="darkred") +
  #        theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) +
  #        scale_x_continuous("Predicted Wind Speed", limits=c(0, 20)) +
  #        scale_y_continuous("Observed Wind Speed", limits=c(0, 20)) +
  #        ggtitle(paste("Observed & Forecasted Wind Speed for forecast period:", fp))
  
  #Saving plots to a file
  #ggsave(gp, file=paste0("Obs&Fore_scatter_plot_fp",fp,".png"), width = 14, height = 10, units = "cm")
  
  ##Error computation
  print(paste("mse: ", mse(winds.fp.cc.agg$wspeed, winds.fp.cc.agg$wspeed_fore)))
  print(paste("mae: ", mae(winds.fp.cc.agg$wspeed, winds.fp.cc.agg$wspeed_fore)))
  print(paste("rmse: ", rmse(winds.fp.cc.agg$wspeed, winds.fp.cc.agg$wspeed_fore)))
  
}


for(fp in 1:10) {
  print(paste("For forecast period:",fp))
  for(stat in 1:25) {
    
    winds.fp <- winds.subset[winds.subset$forecast_period==fp,]
    #winds.fp.cc <- winds.fp[complete.cases(winds.fp), ]
    winds.fp.stat <- winds.fp[winds.fp$station_id==stat,]
    winds.fp.stat.cc <- winds.fp.stat[complete.cases(winds.fp.stat), ]
    print(paste("For Station id:", stat))
    
    gp2<- ggplot(data= winds.fp.stat.cc, mapping= aes(wspeed_fore, wspeed)) + 
            geom_point(color="blue") +
            geom_abline(intercept = 0, slope = 1, color="darkred") +
            theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5)) +
            scale_x_continuous("Predicted Wind Speed", limits=c(0, 20)) +
            scale_y_continuous("Observed Wind Speed", limits=c(0, 20)) +
            ggtitle(paste("Observed & Forecasted Wind Speed for forecast period:", fp,
                          " and station id:", stat))
    
    #Saving plots to a file
    ggsave(gp2, file=paste0("Obs&Fore_scatter_plot_fp",fp,"_st",stat,".png"), width = 14, height = 10, units = "cm")
    
    ##Error computation
    print(paste("mse: ", mse(winds.fp.stat.cc$wspeed, winds.fp.stat.cc$wspeed_fore)))
    print(paste("mae: ", mae(winds.fp.stat.cc$wspeed, winds.fp.stat.cc$wspeed_fore)))
    print(paste("rmse: ", rmse(winds.fp.stat.cc$wspeed, winds.fp.stat.cc$wspeed_fore)))
    
  }
}
