#load data
Regiondata.df = read.csv (file="Data_RegionEx.csv", header = TRUE ) 
#observe the data first
View(Regiondata.df) 

library(ggplot2)
library(dplyr)
library(statsr)

#Compute the mean, median, 90th percentile, and 
#standard deviation of the number of arrival delay minutes of RegionEx's flights. Do the same for MDA's flights. 
#How do the two airlines compare?

##Select RegionEx’s flights
regionex.delay <- Regiondata.df[which(Regiondata.df$Airline == 'RegionEx'), ]
typeof(regionex.delay$Arrival.delay.in.minutes)
##Convert integer to numeric in order to calculate
regionex.delay$Arrival.delay.in.minutes <- as.numeric(as.character(regionex.delay$Arrival.delay.in.minutes))
##Calculate the result

regionex.delay %>%
  summarise(mean_rdelay = mean(Arrival.delay.in.minutes), median_rdelay = median(Arrival.delay.in.minutes), rninty_percent = quantile(Arrival.delay.in.minutes, c(.90)) ,rsd_rdelay = sd(Arrival.delay.in.minutes), n = n())

##Select MDA’s flights and exclude the flight that cancelled
MDA.delay <- Regiondata.df[which(Regiondata.df$Airline == 'MDA' & Regiondata.df$Actual.arrival.time != "Cancelled"), ]
##Check the type of column before calculation
typeof(MDA.delay$Arrival.delay.in.minutes)
##Convert integer to numeric in order to calculate
MDA.delay$Arrival.delay.in.minutes <- as.numeric(as.character(MDA.delay$Arrival.delay.in.minutes))
##Calculate the result
MDA.delay %>%
  summarise(mean_MDAdelay = mean(Arrival.delay.in.minutes), median_MDAdelay = median(Arrival.delay.in.minutes), MDAninty_percent = quantile(Arrival.delay.in.minutes, c(.90)) ,MDAsd_rdelay = sd(Arrival.delay.in.minutes), n = n())

##Inspect the distribution of RegionEx's arrival delays by constructing a histogram of the number of arrival delay minutes of RegionEx's flights.
#Do the same for MDA's flights. 

hist(regionex.delay[,8], main="Histogram for arrival delay of RegionEx's flights",xlab="Minutes Delayed",ylim=c(0,200))
hist(MDA.delay[,8], main="Histogram for arrival delay of MDA's flights", xlab="Minutes Delayed", ylim=c(0,75))

##Using the FAA definition of a "late" flight, what percentage of RegionEx's September flights were "late"? What percentage of MDA's September flights were "late"? What percentage were "on-time" for each airline, according to the FAA definition? (Note: The data
#already incorporates the FAA definition and calls it "Delay indicator").

#Count delay indicator 1 for regionex flight and assign it to the a variable
r.delayvalue <-sum(regionex.delay$Delay.indicator== "1")
#Count delay indicator 0 for regionex flight and assign it to the a variable
r.ontimevalue <-sum(regionex.delay$Delay.indicator=="0")
#Sum up to obtain the total flights for calculation
r.total=r.delayvalue + r.ontimevalue
#Calculate percent late for regionex flight
r.percent.late=r.delayvalue*100/r.total
r.percent.late
#Calculate percent ontime for regionex flight
r.percent.ontime=r.ontimevalue*100/r.total
r.percent.ontime

#Count delay indicator 1 for MDA flight and assign it to the a variable
m.delayvalue <-sum(MDA.delay$Delay.indicator== "1")
#Count delay indicator 0 for regionex flight and assign it to the a variable
m.ontimevalue <-sum(MDA.delay$Delay.indicator=="0")
#Sum up to obtain the total flights for calculation
m.total=m.delayvalue + m.ontimevalue
#Calculate percent late for regionex flight
m.percent.late=m.delayvalue*100/m.total
m.percent.late
#Calculate percent ontime for regionex flight
m.percent.ontime=m.ontimevalue*100/m.total
m.percent.ontime 

##Compare the performance of the two airlines on each flight leg by calculating the descriptive statistics (mean, median, 90th percentile, standard deviation) of delay minutes for each of the four routes. 
#Calculate the % delay flights for each of the route.

##DFW-MSY route

##select the correct flight data and exclude the flight cancelled
DFWtoMSY<- Regiondata.df[ which(Regiondata.df$Origin.airport == 'DFW' 
                                & Regiondata.df$Destination.airport == 'MSY' & Regiondata.df$Actual.arrival.time != "Cancelled"), ]
##Convert integer to numeric in order to calculate
DFWtoMSY$Arrival.delay.in.minutes <- as.numeric(as.character(DFWtoMSY$Arrival.delay.in.minutes))
##Calculate
DFWtoMSY %>%
  group_by(Airline) %>%
  summarise(mean_delay = mean(Arrival.delay.in.minutes), median_delay = median(Arrival.delay.in.minutes), ninty_percent = quantile(Arrival.delay.in.minutes, c(.90)) ,sd_delay = sd(Arrival.delay.in.minutes), percent_delay = sum(Delay.indicator == "1") / n() *100, n = n())

##MSY-DFW route

##select the correct flight data and exclude the flight cancelled
MSYtoDFW<- Regiondata.df[ which(Regiondata.df$Origin.airport == 'MSY' 
                                & Regiondata.df$Destination.airport == 'DFW' & Regiondata.df$Actual.arrival.time != "Cancelled"), ]
##Convert integer to numeric in order to calculate
MSYtoDFW$Arrival.delay.in.minutes <- as.numeric(as.character(MSYtoDFW$Arrival.delay.in.minutes))

MSYtoDFW %>%
  group_by(Airline) %>%
  summarise(mean_delay = mean(Arrival.delay.in.minutes), median_delay = median(Arrival.delay.in.minutes), ninty_percent = quantile(Arrival.delay.in.minutes, c(.90)) ,sd_delay = sd(Arrival.delay.in.minutes), percent_delay = sum(Delay.indicator == "1") / n() *100, n = n())

##MSY-PNS route

##select the correct flight data and exclude the flight cancelled
MSYtoPNS<- Regiondata.df[ which(Regiondata.df$Origin.airport == 'MSY' 
                                & Regiondata.df$Destination.airport == 'PNS' & Regiondata.df$Actual.arrival.time != "Cancelled"), ]
##Convert integer to numeric in order to calculate
MSYtoPNS$Arrival.delay.in.minutes <- as.numeric(as.character(MSYtoPNS$Arrival.delay.in.minutes))

MSYtoPNS %>%
  group_by(Airline) %>%
  summarise(mean_delay = mean(Arrival.delay.in.minutes), median_delay = median(Arrival.delay.in.minutes), ninty_percent = quantile(Arrival.delay.in.minutes, c(.90)) ,sd_delay = sd(Arrival.delay.in.minutes), percent_delay = sum(Delay.indicator == "1") / n() *100, n = n())

##PNS-MSY route

##select the correct flight data and exclude the flight cancelled
PNStoMSY<- Regiondata.df[ which(Regiondata.df$Origin.airport == 'PNS' 
                                & Regiondata.df$Destination.airport == 'MSY' & Regiondata.df$Actual.arrival.time != "Cancelled"), ]
##Convert integer to numeric in order to calculate
PNStoMSY$Arrival.delay.in.minutes <- as.numeric(as.character(PNStoMSY$Arrival.delay.in.minutes))

PNStoMSY %>%
  group_by(Airline) %>%
  summarise(mean_delay = mean(Arrival.delay.in.minutes), median_delay = median(Arrival.delay.in.minutes), ninty_percent = quantile(Arrival.delay.in.minutes, c(.90)) ,sd_delay = sd(Arrival.delay.in.minutes), percent_delay = sum(Delay.indicator == "1") / n() *100, n = n())

###Consider only the RegionEx flights. 
#Prepare a scatter plot of arrival delay minutes versus number of passengers. Your scatter plot should consist of 240 data points, one for each flight in the data set where the vertical coordinate is arrival delay minutes of that flight and the horizontal coordinate is the number of passengers. What is the correlation coefficient between arrival delay
#minutes and number of passengers for RegionEx's flights? 

##Scatter plot for Average Delay in minutes Vs No. of passengers

plot(regionex.delay$Arrival.delay.in.minutes ~ regionex.delay$Number.of.passengers, xlab="No. of passengers" , ylab="Arrival Delay in Minutes")

##Corellation Coefficient

cor(regionex.delay$Arrival.delay.in.minutes,regionex.delay$Number.of.passengers)

##Compare the scheduled flight durations for the two airlines on each of their four routes.
#Compare the actual flight durations.

##DFW to MS: Route 1
##Select flights for Route 1
Route1<- Regiondata.df[ which(Regiondata.df$Origin.airport == 'DFW' 
                              & Regiondata.df$Destination.airport == 'MSY' & Regiondata.df$Actual.arrival.time != "Cancelled"), ]

##Loading numbers in Hours : Minutes format for calculations:

Route1$Scheduled.departure.time <- as.POSIXct(Route1$Scheduled.departure.time, format="%H:%M")
Route1$Scheduled.arrival.time <- as.POSIXct(Route1$Scheduled.arrival.time, format="%H:%M")
Route1$Actual.arrival.time <- as.POSIXct(Route1$Actual.arrival.time, format="%H:%M")

##Display Scheduled and Actual Departure durations for respective airlines:

Route1 %>%
  group_by(Airline) %>%
  summarise(mean_SD_R1= mean((Scheduled.arrival.time)- (Scheduled.departure.time)), mean_AD_R1 = mean((Actual.arrival.time) - (Scheduled.departure.time)))

## Filtering out just the RegionEx flights from DFW to MSY (Route 1)
New_delay_times_R1Rx<- Route1[ which(Route1$Airline == 'RegionEx'), ]
New_delay_times_R1Mda = Route1[ which(Route1$Airline == 'MDA'), ]

## If scheduled flight duration is made equal for both airlines, 
## the delay times for RegionEx flight will be reduced by increase in flight duration which is 10 mins
## new_delay_R1Rx shows delay times for RegionEx airlines on route1, in case Scheduled durations for both airlines were made equal.

new_delay_R1Rx<- as.numeric(as.character(New_delay_times_R1Rx$Arrival.delay.in.minutes))-10
new_delay_R1Mda = as.numeric(as.character(New_delay_times_R1Mda$Arrival.delay.in.minutes))
## Based on the FAA definition of late flights find out revised number of delayed flight for RegionEx on Route 1:
no_of_delayed_flights_R1Rx <- new_delay_R1Rx[ which(new_delay_R1Rx > 15)]
no_of_delayed_flights_R1MDA = new_delay_R1Mda [ which (new_delay_R1Mda >15)]

length(no_of_delayed_flights_R1Rx) ##Total no. of delayed flights of RegionEx on this route

length(no_of_delayed_flights_R1MDA)##Total no. of delayed flights of MDA on this route

(length(no_of_delayed_flights_R1Rx)/ length(new_delay_R1Rx))##delay ratio of RegionEx on this route

(length(no_of_delayed_flights_R1MDA)/ length(new_delay_R1Mda))## delay ratio of MDA on this route

##MSY to DFW: Route 2

##Select flights for Route 2

Route2<- Regiondata.df[ which(Regiondata.df$Origin.airport == 'MSY' 
                              & Regiondata.df$Destination.airport == 'DFW' & Regiondata.df$Actual.arrival.time != "Cancelled"), ]

##Loading numbers in Hours : Minutes format for calculations:

Route2$Scheduled.departure.time <- as.POSIXct(Route2$Scheduled.departure.time, format="%H:%M")
Route2$Scheduled.arrival.time <- as.POSIXct(Route2$Scheduled.arrival.time, format="%H:%M")
Route2$Actual.arrival.time <- as.POSIXct(Route2$Actual.arrival.time, format="%H:%M")


##Display Scheduled and Actual Departure durations for respective airlines:

Route2 %>%
  group_by(Airline) %>%
  summarise(mean_SD_R2= mean((Scheduled.arrival.time)- (Scheduled.departure.time)), mean_AD_R2 = mean((Actual.arrival.time) - (Scheduled.departure.time)))

## Filtering out just the RegionEx flights from MSY to DFW (Route 2)
New_delay_times_R2Rx<- Route2[ which(Route2$Airline == 'RegionEx'), ]


New_delay_times_R2Mda = Route2[ which(Route2$Airline == 'MDA'), ]


## If scheduled flight duration is made equal for both airlines, 
## the delay times for RegionEx flight will be reduced by increase in flight duration which is 10 mins
new_delay_R2Rx<- as.numeric(as.character(New_delay_times_R2Rx$Arrival.delay.in.minutes))-10


new_delay_R2Mda = as.numeric(as.character(New_delay_times_R2Mda$Arrival.delay.in.minutes))

## Based on the FAA definition of late flights find out revised number of delayed flight for RegionEx on Route 2
no_of_delayed_flights_R2Rx <- new_delay_R2Rx[ which(new_delay_R2Rx > 15)]
no_of_delayed_flights_R2MDA = new_delay_R2Mda [ which (new_delay_R2Mda >15)]
length(no_of_delayed_flights_R2MDA)

length(no_of_delayed_flights_R2Rx)


length(no_of_delayed_flights_R2Rx)/length(new_delay_R2Rx)## delay ratio for RegionEx on this route after revised 
length(no_of_delayed_flights_R2MDA)/length(new_delay_R2Mda)## delay ratio for RegionEx on this route after revised 


##MSY to PNS: Route 3

##Select flights for Route 3
Route3<- Regiondata.df[ which(Regiondata.df$Origin.airport == 'MSY' 
                              & Regiondata.df$Destination.airport =='PNS' & Regiondata.df$Actual.arrival.time != "Cancelled"), ]

##Loading numbers in Hours : Minutes format for calculations:

Route3$Scheduled.departure.time <- as.POSIXct(Route3$Scheduled.departure.time, format="%H:%M")
Route3$Scheduled.arrival.time <- as.POSIXct(Route3$Scheduled.arrival.time, format="%H:%M")
Route3$Actual.arrival.time <- as.POSIXct(Route3$Actual.arrival.time, format="%H:%M")

##Display Scheduled and Actual Departure durations for respective airlines:

Route3 %>%
  group_by(Airline) %>%
  summarise(mean_SD_R3= mean((Scheduled.arrival.time)- (Scheduled.departure.time)), mean_AD_R3 = mean((Actual.arrival.time) - (Scheduled.departure.time)))

## Filtering out just the RegionEx flights from MSY to DFW (Route 3)
New_delay_times_R3Rx<- Route3[ which(Route3$Airline == 'RegionEx'), ]

New_delay_times_R3Mda = Route3[ which(Route3$Airline == 'MDA'), ]


## If scheduled flight duration is made equal for both airlines, 
## the delay times for RegionEx flight will be reduced by increase in flight duration which is 5 mins

new_delay_R3Rx<- as.numeric(as.character(New_delay_times_R3Rx$Arrival.delay.in.minutes))-5
new_delay_R3Mda = as.numeric(as.character(New_delay_times_R3Mda$Arrival.delay.in.minutes))


## Based on the FAA definition of late flights find out revised number of delayed flight for RegionEx on Route 3
no_of_delayed_flights_R3Rx <- new_delay_R3Rx[ which(new_delay_R3Rx > 15)]
no_of_delayed_flights_R3Mda <- new_delay_R3Mda[ which(new_delay_R3Mda > 15)]
length(no_of_delayed_flights_R3Mda)

length(no_of_delayed_flights_R3Rx)

length(no_of_delayed_flights_R3Rx)/length(new_delay_R3Rx)##delay ratio for RegionEx after revised

length(no_of_delayed_flights_R3Mda)/length(new_delay_R3Mda)##delay ratio for RegionEx after revised

##PNS to MSY : Route 4

##Select flights for Route 4

Route4<- Regiondata.df[ which(Regiondata.df$Origin.airport == 'PNS' 
                              & Regiondata.df$Destination.airport =='MSY' & Regiondata.df$Actual.arrival.time != "Cancelled"), ]

##Loading numbers in Hours : Minutes format for calculations:

Route4$Scheduled.departure.time <- as.POSIXct(Route4$Scheduled.departure.time, format="%H:%M")
Route4$Scheduled.arrival.time <- as.POSIXct(Route4$Scheduled.arrival.time, format="%H:%M")
Route4$Actual.arrival.time <- as.POSIXct(Route4$Actual.arrival.time, format="%H:%M")

##Display Scheduled and Actual Departure durations for respective airlines:
Route4 %>%
  group_by(Airline) %>%
  summarise(mean_SD_R4= mean((Scheduled.arrival.time)- (Scheduled.departure.time)), mean_AD_R4 = mean((Actual.arrival.time) - (Scheduled.departure.time)))

## Filtering out just the RegionEx flights from PNS to MSY (Route 4)
New_delay_times_R4Rx<- Route4[ which(Route4$Airline == 'RegionEx'), ]
New_delay_times_R4Mda = Route4[ which(Route4$Airline == 'MDA'), ]


## If scheduled flight duration is made equal for both airlines, 
## the delay times for RegionEx flight will be reduced by increase in flight duration which is 5 mins
new_delay_R4Rx<- as.numeric(as.character(New_delay_times_R4Rx$Arrival.delay.in.minutes))-5
new_delay_R4Mda = as.numeric(as.character(New_delay_times_R4Mda$Arrival.delay.in.minutes))

## Based on the FAA definition of late flights find out revised number of delayed flight for RegionEx on Route 1
no_of_delayed_flights_R4Rx <- new_delay_R4Rx[ which(new_delay_R4Rx > 15)]
no_of_delayed_flights_R4Mda <- new_delay_R4Mda[ which(new_delay_R4Mda > 15)]
length(no_of_delayed_flights_R4Mda)
length(no_of_delayed_flights_R4Rx)

length(no_of_delayed_flights_R4Rx)/length(new_delay_R4Rx)##delay ratio for RegionEx after revised
length(no_of_delayed_flights_R4Mda)/length(new_delay_R4Mda)##delay ratio for MDA



##What other factors should Marion Volero take into consideration regarding the data analysis?

Regiondata.df %>%
  group_by(Day.of.Week) %>%
  summarise(flight_delayed = sum(Delay.indicator==1), flight_on_time = sum(Delay.indicator==0), n = n())
