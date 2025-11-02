#install.packages(ggplot2)
library(lubridate)

datH <- read.csv("Z:\\data/hw5_data/stream_flow_data.csv",
                 na.strings = c("Eqp"))
datP <- read.csv("Z:\\data/hw5_data/2049867.csv")
datD <- datH[datH$discharge.flag == "A",]


#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))        

##########################################################################
#######QUESTION 3 AND 4
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
nrow(datP)
nrow(datH)
nrow(datD)

#formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

dev.new(width=8,height=8)
par(mai=c(1,1,1,1))
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)
############################################################################
#######QUESTION 5, 6
# Bigger margins
par(mai=c(1,1,1,1))

#Main plot: mean discharge with SD polygon
plot(aveF$doy, aveF$dailyAve, 
     type="l", 
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,200),           # increased y-axis limit
     xaxs="i", yaxs="i",     # remove gaps
     axes=FALSE)

#SD polygon
polygon(c(aveF$doy, rev(aveF$doy)), 
        c(aveF$dailyAve - sdF$dailySD, rev(aveF$dailyAve + sdF$dailySD)), 
        col=rgb(0.392, 0.584, 0.929, 0.2), 
        border=NA)

#Axes
axis(1, seq(0,360, by=40), labels=seq(0,360, by=40))
axis(2, seq(0,200, by=20), seq(0,200, by=20), las=2)

#Legend
legend("topright", c("Mean","1 standard deviation","2017 Obs"), 
       lwd=c(2,NA,NA), 
       col=c("black", rgb(0.392, 0.584, 0.929,0.2), "red"), 
       pch=c(NA,15,16),
       bty="n")

#2017 observations
dat2017 <- subset(datD, year == 2017)
points(dat2017$doy, dat2017$discharge, 
       col=rgb(1,0,0,0.6), # semi-transparent red
       pch=16,
       cex=0.5)     

#########################################################################
#QUESTION 7
#aggregate precipitation data counts by year and day of year
prcp_counts <- aggregate(datP$HPCP, 
                         by = list(year = datP$year, doy = datP$doy), 
                         FUN = length)
#########PART A
#rename count column
names(prcp_counts)[3] <- "n_obs"
#create binary variable
prcp_counts$full_24h <- prcp_counts$n_obs == 24
#create full days variable by sorting by true
full_days <- subset(prcp_counts, full_24h == TRUE)

########PART B
datD_plot <- merge(datD, 
                   prcp_counts[, c("year", "doy", "full_24h")], 
                   by = c("year", "doy"), 
                   all.x = TRUE)
datD_plot$full_24h[is.na(datD_plot$full_24h)] <- FALSE

#set bigger margins
par(mai=c(1,1,1,1))

#plot all discharge points
plot(datD_plot$decDay, datD_plot$discharge, 
     type="p", pch=16, cex=0.4,          # smaller discharge points
     col = "black",
     xlab = "Decimal Day of Year",
     ylab = expression(paste("Discharge ft"^"3","/ sec")),
     ylim = c(0, max(datD_plot$discharge, na.rm = TRUE)))

#add 24-hour precipitation days on top (larger, colored)
points(datD_plot$decDay[datD_plot$full_24h],
       datD_plot$discharge[datD_plot$full_24h],
       col = "blue", pch=16, cex=0.8)    # larger, stands out

#add legend
legend("topright", legend = c("Full 24h precipitation", "Other days"),
       col = c("blue", "black"), pch=16, bty="n")

##########################################################################
##########QUESTION 8
hydroD <- datD[datD$doy >=328 & datD$doy < 330 & datD$year == 2013,]
hydroP <- datP[datP$doy >= 328 & datP$doy < 330 & datP$year == 2013,]

yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl, yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     axes=TRUE)
#indicate precipitation
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

##########################################################################
##########QUESTION 9

#isolate desired years
dat2016 <- subset(datD, year == 2016)
dat2017 <- subset(datD, year == 2017)

#part A - plot 2016
#define seasons via days in year
dat2016$season <- with(dat2016, ifelse(
  doy >= 80 & doy <= 171, "Spring",
  ifelse(doy >= 172 & doy <= 265, "Summer",
         ifelse(doy >= 266 & doy <= 354, "Fall",
                "Winter")))
)
#plot 2016 violin chart
library(ggplot2)

ggplot(dat2016, aes(x = season, y = discharge, fill = season)) +
  geom_violin(trim = FALSE, color = "black") +
  labs(
    title = "Streamflow Distributions by Season (2016)",
    x = "Season",
    y = "Streamflow (discharge units)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

#part B - plot 2017
#define the seasons via days in year
dat2017$season <- with(dat2017, ifelse(
  doy >= 80 & doy <= 171, "Spring",
  ifelse(doy >= 172 & doy <= 265, "Summer",
         ifelse(doy >= 266 & doy <= 354, "Fall",
                "Winter")))
)
#plot 2017 violin chart
ggplot(dat2017, aes(x = season, y = discharge, fill = season)) +
  geom_violin(trim = FALSE, color = "black") +
  labs(
    title = "Streamflow Distributions by Season (2017)",
    x = "Season",
    y = "Streamflow (discharge units)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")