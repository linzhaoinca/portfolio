install.packages("RMySQL")
library(RMySQL)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Iris Example
## Lists attributes contained in a table
dbListFields(con,'iris')
## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

## Submeter Project
## Using the dbListFields() to learn the attributes associated with the yr_2006 table.
dbListFields(con,'yr_2006')
## Select Date, Time and the 3 sub-meter attributes
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, 
                      Sub_metering_2, Sub_metering_3 FROM yr_2006")

yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, 
                      Sub_metering_2, Sub_metering_3 FROM yr_2007")

yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, 
                      Sub_metering_2, Sub_metering_3 FROM yr_2008")

yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, 
                      Sub_metering_2, Sub_metering_3 FROM yr_2009")

yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, 
                      Sub_metering_2, Sub_metering_3 FROM yr_2010")

## Investigate each new DF
## Use str(), summary(), head() and tail() with each data frame
str(yr_2006)  
summary(yr_2006) 
head(yr_2006)    
tail(yr_2006)    

install.packages("dplyr")
library(dplyr)

## Combine tables into one dataframe using dplyr
newDF <- bind_rows(yr_2007, yr_2008, yr_2009)

## Preprocessing
## Combine Date and Time attribute values in a new attribute column
updated_newDF <-cbind(newDF,paste(newDF$Date,newDF$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name 
colnames(updated_newDF)[6] <-"DateTime"

## Move the DateTime attribute within the dataset
updated_newDF <- updated_newDF[,c(ncol(updated_newDF), 1:(ncol(updated_newDF)-1))]
head(updated_newDF)

## Convert DateTime from character to POSIXct 
updated_newDF$DateTime <- as.POSIXct(updated_newDF$DateTime, "%Y%m/%d %H:%M:%S")

## Add the time zone
attr(updated_newDF$DateTime, "tzone") <- "Europe/Paris"

## Inspect the data types
str(updated_newDF)

install.packages("lubridate")

library(lubridate)

## Extract "Year" information from DateTime using the Lubridate "year" function and create an attribute for year
## Create "year", "quarter", "month" etc attribute with lubridate
updated_newDF$year <- year(updated_newDF$DateTime)
updated_newDF$quarter <- quarter(updated_newDF$DateTime)
updated_newDF$month <- month(updated_newDF$DateTime)
updated_newDF$week <- week(updated_newDF$DateTime)
updated_newDF$weekDay <- wday(updated_newDF$DateTime) 
updated_newDF$day <- day(updated_newDF$DateTime)
updated_newDF$hour <- hour(updated_newDF$DateTime)
updated_newDF$minute <- minute(updated_newDF$DateTime)

## Inspect the data types
str(updated_newDF)
summary(updated_newDF)
head(updated_newDF)
tail(updated_newDF)
---------------------------------------------------------------------------------------------
diff <- make_difftime(days = 31) #difftime 
as.interval(diff, ymd("2009-01-01"))
as.interval(diff, ymd("2009-02-01"))

dur <- duration(days = 31) #duration 
as.interval(dur, ymd("2009-01-01")) 
as.interval(dur, ymd("2009-02-01"))

per <- period(months = 1) #period 
as.interval(per, ymd("2009-01-01")) 
as.interval(per, ymd("2009-02-01"))
###############################################################################################################
###############################################################################################################
## TASK 2
## Plot all of sub-meter 1
plot(yr_2007$Sub_metering_1, col=blues9)
plot(newDF$Sub_metering_1, col=blues9)

## Subset the second week of 2008 - All Observations
houseWeek <- filter(updated_newDF, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)
install.packages("plotly")
library(plotly)
packageVersion('plotly')

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(updated_newDF, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
  xaxis = list(title = "Time"),
  yaxis = list (title = "Power (watt-hours)"))

# Reducing Granularity

## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(updated_newDF, year == 2008 & month == 1 & day == 9 & 
                       (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
## My trial 1
## Subset week 2 from year 2008 with 30 Minutes frequency
week2 <- filter(updated_newDF, year == 2008 & month == 1 & week == 2 & (minute == 0 | minute == 30 ))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 30 Minute frequency
plot_ly(week2, x = ~week2$DateTime, y = ~week2$Sub_metering_1, name = 'Kitchen', 
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~week2$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~week2$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption of Week 2, 2008",
         xaxis = list(title = "Time in every 30 minutes"),
         yaxis = list (title = "Power (watt-hours)"))

## My trial 2
## Subset the summmer 3 month for 2008 - 60 Minutes frequency
summer2008 <- filter(updated_newDF, year == 2008 & month > 5 & month < 10 & (minute == 0 | minute == 60))
summer2007 <- filter(updated_newDF, year == 2007 & month > 5 & month < 10 & (minute == 0 | minute == 60))
summer2009 <- filter(updated_newDF, year == 2009 & month > 5 & month < 10 & (minute == 0 | minute == 60))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(summer2008, x = ~summer2008$DateTime, y = ~summer2008$Sub_metering_1, name = 'Kitchen', 
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~summer2008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~summer2008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption for Summer 2008",
         xaxis = list(title = "Time in every 60 minutes"),
         yaxis = list (title = "Power (watt-hours)"))
#############################################################################################
## TIME SERIES
## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(updated_newDF, weekDay == "2" & hour == 20 & minute == 1)
                                          
## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, 
                         start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
library(ggplot2)
library(ggfortify)
install.packages("labeling")  # Have to install otherwise cannot use autoplot function
library(labeling)
autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 3")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly, main = "Sub-meter 3", col="red")
--------------------------------------------------------------------------------------------
## My trial 3 (weekly--"Saturday") for SubMeter 1 
## Subset to 1 observation per week on Saturday at 8:00pm for 2007, 2008 and 2009
house070809weekly_Sat <- filter(updated_newDF, weekDay == 7 & hour == 20 & minute == 0)

## Create TS object with SubMeter 1
tsSM1_070809weekly_Sat <- ts(house070809weekly_Sat$Sub_metering_1, frequency=52, start=c(2007,1))
autoplot(tsSM1_070809weekly_Sat)

## Plot sub-meter 1 with autoplot - add labels, color
autoplot(tsSM1_070809weekly_Sat, ts.colour = 'red', xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 1")

## Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809weekly_Sat, main = "Sub-meter 1-Kitchen", col="red")
--------------------------------------------------------------------------------------------
## My trial 4 Daily 365 days for Sub-meter 2 ####
## Subset to one observation per day at 8:00pm for 2007, 2008 and 2009
house070809daily <- filter(updated_newDF, hour ==20 & minute == 0)

## Create TS object with SubMeter 2
tsSM2_070809daily <- ts(house070809daily$Sub_metering_2, frequency=365, start=c(2007,1))

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM2_070809daily, ts.colour = 'red', xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 2")

## Plot sub-meter 2 with plot.ts
plot.ts(tsSM2_070809daily, main = "Sub-meter 2-Laundry Room", col="red")
-----------------------------------------------------------------------------------
## My trial 5 (weekly--"Sunday") for Sub-Meter 1 
## Subset to 1 observation per week on Saturday at 8:00pm for 2007, 2008 and 2009
house070809weekly_Sun <- filter(updated_newDF, weekDay == 1 & hour == 20 & minute == 0)

## Create TS object with SubMeter 2
tsSM1_070809weekly_Sun <- ts(house070809weekly_Sun$Sub_metering_1, frequency=52, 
                           start=c(2007,1), end=c(2009,12))
autoplot(tsSM1_070809weekly_Sun)

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM1_070809weekly_Sun, ts.colour = 'red', xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 1")

## Plot sub-meter 2 with plot.ts
plot.ts(tsSM1_070809weekly_Sun, main = "Sub-meter 2-Kitchen", col="red")

------------------------------------------------------------------------------------
## My trail 6 Sub-Meter 2 (one month of data, taken at 18:00pm per day)
## Reduce granularity! change the object from daily to monthly, from minute data to hourly, and more. 
## Plot all of sub-meter 1
plot(updated_newDF$Sub_metering_2)

## Install tibbletime & subset to 1 hour interval
install.packages("tibbletime")
library(tibbletime)
updated_newDF <- as_tbl_time(updated_newDF, index = DateTime)
updated_newDF
newDF_1h <- as_period(updated_newDF, '1 hour')
newDF_1h
plot(newDF_1h$Sub_metering_2)

## March 2007
house07Monthly <- filter(newDF_1h, year == 2007 & month ==3)
tsSM2_07Monthly <- ts(house07Monthly$Sub_metering_2, frequency =24)
autoplot(tsSM2_07Monthly, ts.colour = 'red', xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 2 for March 2007" )

## March 2008
house08Monthly <- filter(newDF_1h, year == 2008 & month ==3)
tsSM2_08Monthly <- ts(house08Monthly$Sub_metering_2, frequency =24)
autoplot(tsSM2_08Monthly, ts.colour ='red', xlab = "Time",
         ylab = "Watt Hours", main = "Sub-meter 2 for March 2008")

## March 2009
house09Monthly <- filter(newDF_1h, year == 2009 & month ==3)
tsSM2_09Monthly <- ts(house09Monthly$Sub_metering_2, frequency=24)
autoplot(tsSM2_09Monthly, ts.colour = 'red', xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 2 for March 2009")

####################################################################################
## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
install.packages("forecast")
library(forecast)
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")
------------------------------------------------------------------------------------

fitSM308Monthly <- tslm(tsSM2_08Monthly ~ trend + season)  
summary(fitSM308Monthly)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM308Monthly <- forecast(fitSM308Monthly, h=20)
## Plot the forecast for sub-meter 3 
plot(forecastfitSM308Monthly)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c08Monthly <- forecast(fitSM308Monthly, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c08Monthly, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")
  
----------------------------------------------------------------------------------------------------  
## Apply time series linear regression to the sub-meter 1 ts object and use summary to obtain R2 and RMSE from the model you built
# My trial 7 (weekly--"Saturday") for SubMeter 1 
## Subset to 1 observation per week on Satursday at 8:00pm for 2007, 2008 and 2009
fitSM1 <- tslm(tsSM1_070809weekly_Sat ~ trend + season) 
summary(fitSM1)

## Create the forecast for sub-meter 1. Forecast ahead 20 time periods 
forecastfitSM1 <- forecast(fitSM1, h=24)
## Plot the forecast for sub-meter 1 
plot(forecastfitSM1)

## Create sub-meter 1 forecast with confidence levels 80 and 90
forecastfitSM1c <- forecast(fitSM1, h=24, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

---------------------------------------------------------------------------------------
## My trail 8 Sub-Meter 2 (daily data for 365 days and from 2007-2009, taken at 18:00pm per day)

fitSM2 <- tslm(tsSM2_070809daily ~ trend + season) 
summary(fitSM2)

## Create the forecast for sub-meter 2. Forecast ahead 24 (1 day) periods 
forecastfitSM2 <- forecast(fitSM2, h=24)
## Plot the forecast for sub-meter 2
plot(forecastfitSM2)

## Create sub-meter 2 forecast with confidence levels 80 and 90
forecastfitSM2c <- forecast(fitSM2, h=24, level=c(80,90))

## Plot sub-meter 2 forecast, limit y and add labels
plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

#####################################################################################
## Decompose Sub-meter 3 into trend, seasonal and remainder
  
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly$trend)
summary(components070809SM3weekly$seasonal)
summary(components070809SM3weekly$random)
--------------------------------------------------------------------------------------
## My trial 9 (weekly--"Saturday") for SubMeter 1 
## Subset to 1 observation per week on Satursday at 8:00pm for 2007, 2008 and 2009
## Decompose Sub-meter 1 into trend, seasonal and remainder
  
components070809SM1weekly_Sat <- decompose(tsSM1_070809weekly_Sat)
## Plot decomposed sub-meter 1 
plot(components070809SM1weekly_Sat)
## Check summary statistics for decomposed sub-meter 1 
summary(components070809SM1weekly_Sat$trend)
summary(components070809SM1weekly_Sat$seasonal)
summary(components070809SM1weekly_Sat$random)
--------------------------------------------------------------------------------------
## My trail 10 Sub-Meter 2 (daily data for 365 days and from 2007-2009, taken at 18:00pm per day)
## Decompose Sub-meter 2 into trend, seasonal and remainder
  
components070809SM2daily <- decompose(tsSM2_070809daily) 
## Plot decomposed sub-meter 2
plot(components070809SM2daily)
## Check summary statistics for decomposed sub-meter 2
summary(components070809SM2daily$trend) 
summary(components070809SM2daily$seasonal) 
summary(components070809SM2daily$random) 
#########################################################################################
# Remove Seasonal Components
## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Simple Exponential Smoothing & Plot (if do not set beta= false, gamma= false, then it is Triple exponential smoothing)
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

tsSM3_HW070809 <- HoltWinters(tsSM3_070809weekly, beta=FALSE, gamma=FALSE) 
plot(tsSM3_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))
-------------------------------------------------------------------------------------------------
## My trail 11 Sub-Meter 1 ((weekly--"Saturday") for SubMeter 1 
## Subset to 1 observation per week on Satursday at 8:00pm for 2007, 2008 and 2009
## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM1_070809weekly_Sat - components070809SM1weekly_Sat$seasonal
autoplot(tsSM1_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_070809Adjusted))

## Holt Winters Simple Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))

tsSM1_HW070809 <- HoltWinters(tsSM1_070809weekly_Sat, beta=FALSE, gamma=FALSE) 
plot(tsSM1_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")

## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))
----------------------------------------------------------------------------------------------------
## My trail 12 Sub-Meter 2 (daily data for 365 days and from 2007-2009, taken at 18:00pm per day)
## Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM2_070809daily - components070809SM2daily$seasonal
autoplot(tsSM2_070809Adjusted)

## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_070809Adjusted))

## Holt Winters Simple Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25))

tsSM2_HW070809 <- HoltWinters(tsSM2_070809daily, beta=FALSE, gamma=FALSE) 
plot(tsSM2_HW070809, ylim = c(0, 25))

## HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM2_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")

## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))

