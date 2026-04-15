#ARIMA models
#Stationarity and differencing

#loading the necessary libraries
library(fpp3)
library(urca)
#Extract Google's closing price for 2018
google_2018 = gafa_stock %>% 
  filter(Symbol == "GOOG", year(Date)== 2018)

#visualize the closing prices
google_2018 %>% autoplot(Close)+labs(
  y = "Google closing stock price ($US)"
)

#looking at the ACF
google_2018 %>% 
  ACF(Close) %>% 
  autoplot()

#looking at the difference(yt-yt-1)
google_2018 %>% 
  autoplot(difference(Close))+
  labs(
    y = "Change in Google stock price ($US)"
  )

#looking at the ACF of this to see if it resembles white noise
google_2018 %>% 
  ACF(difference(Close)) %>% 
  autoplot()

PBS

#Example two
#Using australian pharmaceutical data
a10 = PBS %>% 
  filter(ATC2 == "A10") %>% 
  summarise(
    Cost = sum(Cost)/ 1e6
  )
a10

#plot the data
a10 %>% autoplot(Cost)

#To deal with the variance in the data, we take the log of cost
a10 %>% autoplot(log(Cost))+labs(y = "Cost (Millions)", title = "Stabilized variance")

#To deal with the seasonality, we take seasonal differences
#We do achieve stationarity with this
a10 %>% 
  autoplot(log(Cost)%>% 
             difference(12))+
  labs(y = "Cost (Millions)")

#h02
#Extract the necessary data
h02 = PBS %>% 
  filter(ATC2 == "H02") %>% 
  summarise(
    Cost = sum(Cost)/ 1e6
  )
h02

#plot the cost to see if its stationary or not
h02 %>% autoplot(Cost) + labs(
  y = "Cost (Millions)"
)

#We deal with the variation in data by using a log transformation
h02 %>% autoplot(log(Cost))+
  labs(
    y = "Cost (Millions)"
  )

#We then tackle seasonality using the difference function and a first order difference
#Recommended to use the below order, first the seasonal difference, then the differencing
h02 %>% autoplot(log(Cost) %>% difference(12) %>% difference(1))+
  labs( y = "Cost (Millions)")

#unit root tests for determining stationarity
#using KPSS test, its the one that is appropriate for forecasting
#if <0.05, reject that series is stationary
google_2018 %>% 
  features(Close, unitroot_kpss)


#you can also use ndiffs to check how many differences you need to make it stationary
google_2018 %>% 
  features(Close, unitroot_ndiffs)

#Automatically selecting differences
seasonal_differences = h02 %>% mutate(
  log_cost = log(Cost)) %>% 
  features(log_cost, unitroot_nsdiffs)
seasonal_differences

#factoring in the seasonality
h02 %>% 
  mutate(
    d_log_cost = difference(log(Cost), 12
  )) %>% 
  features(
    d_log_cost, unitroot_ndiffs
  )
