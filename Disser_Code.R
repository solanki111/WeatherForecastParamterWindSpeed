library(GGally)
library(dplyr)
library(colortools)
library(ggplot2)

winds <- read.csv("C:/Users/../data.csv", header=T, row.names=NULL, na.strings=c(""))
summary(winds)
View(winds)

winds.df <- as.data.frame(winds)
winds.df$date <- as.Date(winds.df$date, format="%d-%m-%Y")
winds.df$forecast_period <- as.numeric(winds.df$forecast_period)
winds.df$station_id <- as.numeric(winds.df$station_id)
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
winds.fp1.st1 <- winds.fp1[winds.fp1$station_id==1,]
View(winds.fp1.st1.cc)
View(winds.fp1)
summary(winds.fp1)
sapply(winds.fp1.st1,function(x) sum(is.na(x)))
sapply(winds.fp1.st1, function(x) length(unique(x)))
winds.fp1.st1$year <- format(winds.fp1.st1$date, format = "%Y")
winds.fp1.st1$month <- format(winds.fp1.st1$date, format = "%m")
winds.fp1.st1.cc <- winds.fp1.st1[complete.cases(winds.fp1.st1), ]

ggplot(winds.fp1.st1.cc, aes(x = month, y = wspeed, group = year)) +
  geom_line(aes(colour = year)) +
  theme_classic() + 
  scale_color_manual(values = year_pal)

# Performing data wrangling
sapply(winds.fp1,function(x) sum(is.na(x)))
sapply(winds.fp1, function(x) length(unique(x)))

#Removing incomplete rows
winds.fp1.cc <- winds.fp1[complete.cases(winds.fp1), ]
winds.fp1.st1.cc <- winds.fp1.st1[complete.cases(winds.fp1.st1), ]
sapply(winds.fp1.cc,function(x) sum(is.na(x)))
sapply(winds.fp1.cc, function(x) length(unique(x)))

#
winds.fp1.cc$year <- format(winds.fp1.cc$date, format = "%Y")
winds.fp1.cc$year <- as.numeric(as.character(winds.fp1.cc$year))
winds.fp1.cc$station_id <- as.character(as.character(winds.fp1.cc$station_id))

ggplot(winds.fp1.cc, 
        aes(winds.fp1.cc$year, wspeed, group=winds.fp1.cc$station_id, color=winds.fp1.cc$station_id )) +
        geom_line() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=0.5))

ggplot(winds.fp1.cc.agg, aes(x = month, y = wspeed, group = year)) +
  geom_line(aes(colour = year)) +
  theme_classic() + 
  scale_color_manual(values = year_pal)


View(winds.fp1.cc)
View(winds.fp1.st1.cc)

winds.fp1.cc.agg <- aggregate(winds.fp1.cc, 
                              by=list(date=winds.fp1.cc$date), 
                              FUN=mean)

winds.fp1.cc.agg.st <- aggregate(winds.fp1.cc, 
                                 by=list(st=winds.fp1.cc$station_id), 
                                 FUN=mean)

winds.fp1.cc.agg <- winds.fp1.cc.agg[ ,c(1,3,5,6,7,8)]
View(winds.fp1.cc.agg)
View(winds.fp1.cc.agg.st)


plot(wspeed ~ wspeed_fore, 
     data = winds.fp1.cc.agg, 

     main="Plot b/w observed & forecast (Wind Speed)",
     col="darkred", 
     ylab="Observed Wind Speed",
     xlab="Forecast Wind Speed")

abline(0,1)


# Extract month and year and store in separate columns
winds.fp1.cc.agg$year <- format(winds.fp1.cc.agg$date, format = "%Y")
winds.fp1.cc.agg$month <- format(winds.fp1.cc.agg$date, format = "%m")

# Create a colour palette using the `colortools` package 
year_pal <- sequential(color = "red", percentage = 5, what = "value")

# Make the plot to see year wise distribution
ggplot(winds.fp1.cc.agg, aes(x = month, y = wspeed, group = year)) +
  geom_line(aes(colour = year)) +
  theme_classic() + 
  scale_color_manual(values = year_pal)

#Converting year and month to numeric
winds.fp1.cc.agg$year <- as.numeric(as.character(winds.fp1.cc.agg$year))
winds.fp1.cc.agg$month <- as.numeric(as.character(winds.fp1.cc.agg$month))
## Comparison of Time series between observed Wind speed & forecasted Wind speed
# Transform wspeed to `ts` class
wspeed.ts <- ts(winds.fp1.cc.agg$wspeed,
                start=2007, end=2014,
                frequency=12)

# Identifying whic model to use
decompose(wspeed.ts)

# Seasonal decomposition as its additive
wspeed.seas <- stl(wspeed.ts, s.window="period")

# Transform wspeed forecast to `ts` class
wspeed.ts.fore <- ts(winds.fp1.cc.agg$wspeed_fore,
                     start=2007, end=2014,
                     frequency=12)

# Identifying whic model to use
decompose(wspeed.ts.fore)

# Seasonal decomposition as its additive
wspeed.seas.fore <- stl(wspeed.ts.fore, s.window="period")

#Generate plots
# time series plot

par(mfrow=c(1,2))
plot(wspeed.ts,
     main="Observed Wind Speed",
     col="navy", 
     ylab="Observed Wind Speed")


plot(wspeed.ts.fore,
     main="Forecasted Wind Speed",
     col="darkred", 
     ylab="Forecasted Wind Speed")

#Stl plot
plot(wspeed.seas,
     main="Observed Wind Speed Seasonal",
     col="darkred")


plot(wspeed.seas.fore,
     main="Forecasted Wind Speed Seasonal",
     col="darkred")

monthplot(wspeed.ts, 
          choice = "seasonal",
          main="Observed Wind Speed monthly",
          col="darkred")

monthplot(wspeed.ts.fore,
          choice = "seasonal",
          main="Forecatsed Wind Speed monthly",
          col="darkred")

#------------------------------------------------------------------------------#
## Comparison of Time series between observed & forecast Wind direction
#
# For ploting wind direction, need coordinates..
library(ggplot2)
ggplot(winds.fp1.cc$wdir, 
       aes(x = Lon , 
           y = Lat, 
           fill = mean_wind, 
           angle = wind_dir, 
           radius = scales::rescale(mean_wind, c(.2, .8)))) +
  geom_raster() +
  geom_spoke(arrow = arrow(length = unit(.05, 'inches'))) + 
  scale_fill_distiller(palette = "RdYlGn") + 
  coord_equal(expand = 0) + 
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal')


#
#
#


par(mfrow=c(1,2))
wdir.ts <- ts(winds.fp1.cc$wdir,
                start=2007, end=2014,
                frequency=12)
plot(wdir.ts,
     main="Observed Wind Direction",
     col="navy", 
     ylab="Wind Direction (Observed)")

wdir.ts.fore <- ts(winds.fp1.cc$wdir_fore,
                     start=2007, end=2014,
                     frequency=12)
plot(wdir.ts.fore,
     main="Predicted Wind Direction",
     col="darkred", 
     ylab="Forecasted Wind Direction (Predicted)")

# If required, a subset can also be made for the time series for any time period
#myts2 <- window(myts, start=c(2014, 6), end=c(2014, 12)) 

# Identifying whic model to use
decompose(wdir.ts)
decompose(wdir.ts.fore)

# Seasonal decomposition as its additive
wdir.seas <- stl(wdir.ts, s.window="period")
plot(wdir.seas,
     main="Observed Wind Direction",
     col="darkred")

wdir.seas.fore <- stl(ts.wdir.fore, s.window="period")
plot(wdir.seas.fore,
     main="Observed Wind Direction Forecast",
     col="darkred")

#----------------------------------------------------------------------------#
# Visualising the time series plot for observed wind speed as per station ids
# Visualising per year
summary(winds.fp1.cc)
par(mfrow=c(3,4))
for(stat in 1:12) {
  
  a.stat <- winds.fp1.cc[winds.fp1.cc$station_id==stat,]
  # The ts function of R helps us to
  # construct a time series
  plot(ts(a.stat$wspeed,
          start=2007, end=2014,
          frequency=1),
          ylim = range(c(0,12)),
          main=paste("Plot for station Id:", stat),
          xlab="Year", ylab="Observed Wind Speed",
          col="navy")
}

for(stat in 13:24) {
  
  a.stat <- winds.fp1.cc[winds.fp1.cc$station_id==stat,]
  # The ts function of R helps us to
  # construct a time series
  plot(ts(a.stat$wspeed,
          start=2007, end=2014,
          frequency=1),
          ylim = range(c(0,12)),
          main=paste("Plot for station Id:", stat),
          xlab="Year", ylab="Observed Wind Speed",
          col="navy")
}

for(stat in 25:26) {
  
  a.stat <- winds.fp1.cc[winds.fp1.cc$station_id==stat,]
  # The ts function of R helps us to
  # construct a time series
  plot(ts(a.stat$wspeed,
          start=2007, end=2014,
          frequency=1),
          ylim = range(c(0,12)),
          main=paste("Plot for station Id:", stat),
          xlab="Year", ylab="Observed Wind Speed",
          col="navy")
}

#----------------------------------------------------------------------------#
# Visualising the time series plot for forecasted wind speed as per station ids
# Visualising per year
par(mfrow=c(3,4))
for(stat in 1:12) {
  
  a.stat <- winds.fp1.cc[winds.fp1.cc$station_id==stat,]
  # The ts function of R helps us to
  # construct a time series
  plot(ts(a.stat$wspeed_fore,
          start=2007, end=2014,
          frequency=1),
       main=paste("Plot for station Id:", 
                  stat),
       xlab="Year", ylab="Forecasted Wind Speed",
       col="navy")
}

for(stat in 13:24) {
  
  a.stat <- winds.fp1.cc[winds.fp1.cc$station_id==stat,]
  # The ts function of R helps us to
  # construct a time series
  plot(ts(a.stat$wspeed_fore,
          start=2007, end=2014,
          frequency=1),
       main=paste("Plot for station Id:", 
                  stat),
       xlab="Year", ylab="Forecated Wind Speed",
       col="navy")
}

for(stat in 25:26) {
  
  a.stat <- winds.fp1.cc[winds.fp1.cc$station_id==stat,]
  # The ts function of R helps us to
  # construct a time series
  plot(ts(a.stat$wspeed_fore,
          start=2007, end=2014,
          frequency=1),
       main=paste("Plot for station Id:", 
                  stat),
       xlab="Year", ylab="Forcasted Wind Speed",
       col="navy")
}

#----------------------------------------------------------------------------#
# Visualizing the time series plot for observed wind direction as per station ids
par(mfrow=c(3,4))
for(stat in 1:12) {
  
  a.stat <- winds.fp1.cc[winds5$station_id==stat,]
  # The ts function of R helps us to
  # construct a time series
  plot(ts(a.stat$wdir,
          start=2007, end=2014,
          frequency=1),
       main=paste("Plot for station Id:", 
                  stat),
       xlab="Year", ylab="Wind Direction",
       col="navy")
  
}

for(stat in 13:24) {
  
  a.stat <- winds.fp1.cc[winds.fp1.cc$station_id==stat,]
  # The ts function of R helps us to
  # construct a time series
  plot(ts(a.stat$wdir,
          start=2007, end=2014,
          frequency=1),
       main=paste("Plot for station Id:", 
                  stat),
       xlab="Year", ylab="Wind Direction",
       col="navy")
  
}

for(stat in 25:26) {
  
  a.stat <- winds.fp1.cc[winds.fp1.cc$station_id==stat,]
  # The ts function of R helps us to
  # construct a time series
  plot(ts(a.stat$wdir,
          start=2007, end=2014,
          frequency=1),
       main=paste("Plot for station Id:", 
                  stat),
       xlab="Year", ylab="Wind Direction",
       col="navy")
  
}

#--------------------------------------------------------------------------------#
## Additionally, exponenial models can also be applied
# simple exponential - models level
fit1 <- HoltWinters(wspeed.ts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit2 <- HoltWinters(tseries, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(tseries)
#
# predictive accuracy
library(forecast)
accuracy(fit1)
accuracy(fit2)
accuracy(fit3)
#
#
#---------------------------------------------------------------------------------#
#Matrix verification
#
tab.wspeed <- table("Observed Value"=winds.fp1.cc$wspeed, "Predicted value"=winds.fp1.cc$wspeed_fore)
head(tab.wspeed)
observed.value.missclassified <- sum(diag(tab.wspeed))/length(winds.fp1.cc$wspeed)
#
correct.pred.prop <- tab.wspeed[2,c(1)] / length(winds.fp1.cc$wspeed)
#
error_rate <- 1- mean(winds.fp1.cc$wspeed_fore == winds.fp1.cc$wspeed)
#
#
#---------------------------------------------------------------------------------#
# Fitting multiple models
library(forecast)
x <- ts(winds.fp1.cc.agg$wspeed, 
        start=c(winds.fp1.cc.agg$year[1], winds.fp1.cc.agg$month[1]),
        end = 2014,
        frequency=12)

x <- ts(winds.fp1.cc.agg$wspeed_fore, 
        start=c(winds.fp1.cc.agg$year[1], winds.fp1.cc.agg$month[1]),
        end = 2014,
        frequency=12)


train_x <- window(x, end=c(2012, 2))
test_x <- window(x, start=c(2012, 3))
#test.data <- window(wspeed.ts, start=c(2012, 1), end=c(2014, 12))
#train.data <- window(wspeed.ts, start=c(2007, 1), end=c(2011, 12))
#View(x)
#View(test_x)

# Model using multiple methods - arima, expo smooth, theta, random walk, structural time series
models <- list(
  mod.arima = auto.arima(train_x, ic='aicc', stepwise=FALSE),
  mod.exp = ets(train_x, ic='aicc', restrict=FALSE),
  mod.neural = nnetar(train_x, p=12, size=25),
  mod.tbats = tbats(train_x, ic='aicc', seasonal.periods=12),
  mod.bats = bats(train_x, ic='aicc', seasonal.periods=12),
  mod.stl = stlm(train_x, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod.sts = StructTS(train_x),
  mod.naive <- naive(x, 12),
  mod.rwf <- rwf(train_x, h=11)
)

forecasts <- lapply(models, forecast, 12)
par(mfrow=c(4, 2))
for(f in forecasts){
  plot(f)
  lines(test_x, col='red')
}

#arima
mod.arima <- forecast(auto.arima(train_x),h=11)
accuracy(mod.arima, test_x)

#exponential smoothing
mod.ets <- forecast(ets(train_x),h=11)
accuracy(mod.ets, test_x)

#theta
mod.tht <- thetaf(train_x, h=11)
accuracy(mod.tht, test_x)

#random walk
mod.rwf <- rwf(train_x, h=11)
accuracy(mod.rwf, test_x)

#structts
mod.struc <- forecast(StructTS(train_x),h=11)
accuracy(mod.struc, test_x)


## Cross validation Test
devtools::install_github('zachmayer/cv.ts')
library(cv.ts)

ctrl <- tseriesControl(stepSize=1, maxHorizon=12, minObs=36, fixedWindow=TRUE)
models <- list()

models$arima = cv.ts(
  x, auto.arimaForecast, tsControl=ctrl,
  ic='aicc', stepwise=FALSE)

models$exp = cv.ts(
  x, etsForecast, tsControl=ctrl,
  ic='aicc', restrict=FALSE)

models$neural = cv.ts(
  x, nnetarForecast, tsControl=ctrl,
  nn_p=6, size=5)

models$tbats = cv.ts(
  x, tbatsForecast, tsControl=ctrl,
  seasonal.periods=12)

models$bats = cv.ts(
  x, batsForecast, tsControl=ctrl,
  seasonal.periods=12)

models$stl = cv.ts(
  x, stl.Forecast, tsControl=ctrl,
  s.window=12, ic='aicc', robust=TRUE, method='ets')

models$sts = cv.ts(x, stsForecast, tsControl=ctrl)

models$naive = cv.ts(x, naiveForecast, tsControl=ctrl)

models$theta = cv.ts(x, thetaForecast, tsControl=ctrl)


res_overall <- lapply(models, function(x) x$results[13,-1])
res_overall <- Reduce(rbind, res_overall)
row.names(res_overall) <- names(models)
res_overall <- res_overall[order(res_overall[,'MAPE']),]
round(res_overall, 2)

library(reshape2)
res <- lapply(models, function(x) x$results$MAPE[1:12])
res <- data.frame(do.call(cbind, res))
res$horizon <- 1:nrow(res)
res <- melt(res, id.var='horizon', variable.name='model', value.name='MAPE')
res$model <- factor(res$model, levels=row.names(res_overall))
ggplot(res, aes(x=horizon, y=MAPE, col=model)) +
  geom_line(size=2) + theme_bw() +
  theme(legend.position="top") +
  scale_color_manual(values=c(
    "#1f78b4", "#ff7f00", "#33a02c", "#6a3d9a",
    "#e31a1c", "#b15928", "#a6cee3", "#fdbf6f",
    "#b2df8a")
  )



##Error computation
#
library(Metrics)
mse(winds.fp1.cc.agg$wspeed, winds.fp1.cc.agg$wspeed_fore)
mae(winds.fp1.cc.agg$wspeed, winds.fp1.cc.agg$wspeed_fore)
mape(winds.fp1.cc.agg$wspeed, winds.fp1.cc.agg$wspeed_fore)
rmse(winds.fp1.cc.agg$wspeed, winds.fp1.cc.agg$wspeed_fore)

mae <- sum(abs(winds.fp1.cc.agg$wspeed-winds.fp1.cc.agg$wspeed_fore))/length(winds.fp1.cc.agg$wspeed)
msb <- sum(winds.fp1.cc.agg$wspeed-winds.fp1.cc.agg$wspeed_fore)/length(winds.fp1.cc.agg$wspeed)
mae
msb



tab = table("Observed Value"= winds.fp1.cc.agg$wspeed, 'Forecasted value'= winds.fp1.cc.agg$wspeed_fore)
prop.table(tab)
misClasificError <- mean(winds.fp1.cc.agg$wspeed_fore != winds.fp1.cc.agg$wspeed)
print(paste('Accuracy', (1-mean(pred.EG != expected.EG))))
