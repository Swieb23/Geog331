#Creating assert function
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#loading principal dataset
datW <- read.csv("Z:\\mloranty/data/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#importing additional sensor info
    sensorInfo <-   read.csv("Z:\\mloranty/data/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)
colnames(datW) <-   colnames(sensorInfo)

#installing packages to help with timestamp data
#install.packages(c("lubridate")) (already installed)

library(lubridate)

#converting to standardized date format - m/d/y + more date formatting
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))

#QUESTION 4
datW[datW$air.tempQ1 < 8,]  
datW[datW$air.tempQ1 > 33,]  

#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")

#make the points semi-transparent so that frequency may be observed
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#QUESTION 5
assert(length(lightscale) == length(datW$lightning.acvitivy))

#QUESTION 6
#PART 1 - filtering out unwanted values from wind speed measurements
datW$wind.speedQ1 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))
#testing using assert function
assert(length(datW$wind.speedQ1) == length(datW$lightning.acvitivy & datW$precipitation))
assert(all(is.na(datW$wind.speedQ1[datW$precipitation >= 2 & datW$lightning.acvitivy > 0])))
assert(all(is.na(datW$wind.speedQ1[datW$precipitation > 5])))
#PART 2 - PLOTTING
plot(datW$DD , datW$wind.speedQ1, pch =19, xlab = "Day of Year", ylab = "Wind Speed (M/s)",
     type="b")

#QUESTION 7
#I'm going to let both graphs be visible at the same time
par(mfrow=c(2,1))
#let's start by comparing soil and air temperature to see if results align. Immediately, i'm leaning towards yes.
plot(datW$DD, datW$air.temperature,
     type = "l",          
     col = "blue",
     lwd = 2,
     xlab = "Day of Year",
     ylab = "Temperature (°C)",
     ylim = range(c(datW$air.temperature, datW$soil.temp), na.rm = TRUE))
lines(datW$DD, datW$soil.temp,
      col = "tomato",
      lwd = 2)
legend("topright",
       legend = c("Air Temp", "Soil Temp"),
       col = c("blue", "tomato"),
       lty = 1,
       lwd = 2)
#looks like measurements are fairly consistent. Let's look at moisture and precipitation
plot(datW$DD, datW$precipitation,
     type = "p",
     col = "blue",
     pch = 16,
     xlab = "Day of Year",
     ylab = "Precipitation (mm)",
     ylim = range(datW$precipitation, na.rm = TRUE))

par(new = TRUE)

plot(datW$DD, datW$soil.moisture,
     type = "l",
     col = "forestgreen",
     axes = FALSE,
     xlab = "", ylab = "",
     ylim = range(datW$soil.moisture, na.rm = TRUE))

axis(side = 4, col = "forestgreen", col.axis = "forestgreen")
mtext("Soil Moisture (m³/m³)", side = 4, line = 3, col = "forestgreen")

legend("topright",
       legend = c("Precipitation", "Soil Moisture"),
       col = c("blue", "forestgreen"),
       pch = c(16, NA),
       lty = c(NA, 1),
       pt.cex = 1.2,
       lwd = c(NA, 2),
       bty = "n")

#QUESTION 8
#start by making the requested table
means_table <- data.frame(
  avg_air_temp     = mean(datW$air.tempQ2, na.rm = TRUE),
  avg_soil_moist   = mean(datW$soil.moisture, na.rm = TRUE),
  avg_soil_temp    = mean(datW$soil.temp, na.rm = TRUE),
  avg_wind_speed   = mean(datW$wind.speedQ1, na.rm = TRUE)
)
#next we will acquire total precipitation
total_precip <- sum(datW$precipitation, na.rm = TRUE)
total_precip
#finally let's look at the number of observations collected
obs_table <- data.frame(
  air_temp_obs     = sum(!is.na(datW$air.tempQ2)),
  soil_moist_obs   = sum(!is.na(datW$soil.moisture)),
  soil_temp_obs    = sum(!is.na(datW$soil.temp)),
  wind_speed_obs   = sum(!is.na(datW$wind.speedQ1)),
  precip_obs       = sum(!is.na(datW$precipitation))
)
#and finally, the number of days in the study:
#found in excel file. 
#QUESTION 9
par(mfrow=c(2,2))
#graphs for each variable
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation (mm)",
     type="p", pch=19)
plot(datW$DD, datW$air.temperature, type = "l",col = "blue", xlab = "Day of Year",
     ylab = "Air Temperature (°C)")
plot(datW$DD , datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temperature (°C)",
     type="l", pch=19)
plot(datW$DD , datW$soil.moisture, xlab = "Day of Year", ylab = "Meters^3 per Meter^3",
     type="l", pch=19)
