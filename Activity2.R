#script for fall25 homework 2

#ACTIVITY 2

#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm

heights[1]

#look at R's built-in descriptions of matrix function uses
help(matrix)

#byrow determines the way in which matrix values are arranged
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#look at particular values within the matrix's grid
Mat.bycol[1,2]

#dataframes act similarly to matrices, but allow for colums with different
#forms of media (numbers, characters, etc)

#filepath to open dataframe
datW<-read.csv("Z:/mloranty/data/noaa_weather/2011124.csv",
               stringsAsFactors = T)
str(datW)

#vectors satisfying activity requirements
ex_char<-c("apple", "banana", "cherry", "date", "elderberry")
ex_num<-c(5.54,4.67,87.5,23.22,90.4)
ex_int<-c(1L,2L,3L,4L,5L)
ex_fac<-factor(c("low", "medium", "high", "medium", "low"))

#giving myself each individual station name
unique(datW$NAME)

#getting the average high temp for aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate avg daily temp
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

averageTemp<-aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp
#assigning column names to the aggregated total here
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#making names numerics to not be as long
datW$siteN <- as.numeric(datW$NAME)

#set up a histogram for site 1, aberdeen
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

help(hist)
help(paste)

hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#histogram for site 2
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="purple",
     border="black")

abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#hist for site 3
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="green",
     border="blue")
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#hist for site 4
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="black",
     border="red")
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#making all 4 histograms viewable at once
par(mfrow=c(2,2))

help(dnorm)

#now I will begin my exploration of probability analysis.
#Tracking probability of subzero temps below.
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#Tracking probability of temps below 5 degrees C.
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#By subtracting the 2nd pnorm from the first, I can exactly specify probability of temps between 0-5 degrees.
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
                                                        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#finding the probability of temps over 20 by subtracting the pnorm from 1
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Using qnorm determines the temperature at which highs enter the 95th percentile.
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#now, we will add 4 to the mean temp and calculate the probability of temps above the current threshold for extreme highs.
1 - pnorm(18.51,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)+4,
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#having done this, we will move on to a histogram of daily precipitation for Aberdeen.

hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     xlim = c(0,100),
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average Precipitation (mm)", 
     ylab="Relative frequency",
     col="green",
     border="blue")

#now we will move on to plotting average annual precipitation. 
#format all dates in the same way
datW$DATE <- as.Date(datW$DATE)
#now, we will create years by taking only the Y element
datW$YEAR <- format(datW$DATE, "%Y")
#create annual precipitation aggregate
annual_totals <-aggregate(datW$PRCP, by=list(siteN = datW$siteN, YEAR = datW$YEAR), FUN=sum, na.rm=TRUE)
colnames(annual_totals)[3] <- "PRCP_ANN"
colnames(annual_totals)[1] <- "siteN"

hist(annual_totals$PRCP_ANN[annual_totals$siteN == 1],
     freq = FALSE,
     main = print("Aberdeen, WA US"),
     xlab="Annual Precipitation",
     ylab="Relative Frequency",
     col="green",
     border="blue")

#finally, we move to find the mean annual precipitation for all sites. 
mean(annual_totals$PRCP_ANN[annual_totals$siteN == "1"], na.rm=TRUE)
mean(annual_totals$PRCP_ANN[annual_totals$siteN == "2"], na.rm=TRUE)
mean(annual_totals$PRCP_ANN[annual_totals$siteN == "3"], na.rm=TRUE)
mean(annual_totals$PRCP_ANN[annual_totals$siteN == "4"], na.rm=TRUE)
mean(annual_totals$PRCP_ANN[annual_totals$siteN == "5"], na.rm=TRUE)


unique(datW$NAME)
