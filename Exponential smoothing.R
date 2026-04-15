#Methods with trend
#Loading the necessary libraries
#Also contains my attempts of solns in Ex 8 of FPP ed.3
library(fpp3)
library(tidyverse)
library(ggplot2)

#Kenyan population
#Smooth exponential with trend(Holt's linear method)
ke_economy = global_economy %>% 
  filter(Code == "KEN") %>% 
  mutate( pop = Population/1e6)
ke_economy

#define the model
ke_fit = ke_economy %>% 
  model(
    ANN = ETS(pop~ error("A") + trend("A") + season("N"))
  )
ke_fit
report(ake_fit)

#plotting the components of the model
components(ke_fit) %>% autoplot()

#combining the components
components(ke_fit) %>% 
  left_join(fitted(ke_fit), by = c("Country", ".model", "Year"))

#produce forecasts and plot them
ke_fit %>% forecast(h = 10) %>%  autoplot(ke_economy)+
  labs(y = "Millions", title = "Kenyan Population")

#Damped trended method
ke_damped = ke_economy %>% model(
  Holt = ETS(pop~ error("A") + trend("Ad") + season("N"))
) %>% forecast(h = 20) %>% 
  autoplot(ke_economy)
ke_damped

#fitting simple, linear and damped models to the data
ke_sld = ke_economy %>% 
  filter(Year <= 2010) %>% 
  model(
    ses = ETS(pop~ error("A") + trend("N") + season("N")),
    holt = ETS(pop ~ error("A") + trend("A") + season("N")),
    damped = ETS(pop ~ error("A") + trend ("Ad") + season("N"))
  )
ke_sld

#checking the parameters/coefficients using tidy and metrics of accuracy
tidy(ke_sld)
accuracy(ke_sld)

#Models with seasonality
aus_holidays = tourism %>% 
  filter(Purpose == "Holiday") %>% 
  summarise(
    Total_trips = sum(Trips)
  )
aus_holidays

#Define and fit model
aus_mable = aus_holidays %>% 
  model(
    additive = ETS(Total_trips~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Total_trips ~ error("M") + trend("A") + season("M"))
  )
aus_mable

#produce forecasts of the model
aus_fc = aus_mable %>% forecast()
aus_fc

#Holt winters damped method
#Works best with daily data, very well
#Extract the data of interest
sth_cross_ped <- pedestrian |>
  filter(Date >= "2016-07-01",
         Sensor == "Southern Cross Station") |>
  index_by(Date) |>
  summarise(Count = sum(Count)/1000)

#Fit the model to the training set data and forecast two weeks ahead
sth_cross_ped |>
  filter(Date <= "2016-07-31") |>
  model(
    hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))
  ) |>
  forecast(h = "2 weeks") |>
  autoplot(sth_cross_ped |> filter(Date <= "2016-08-14")) +
  labs(title = "Daily traffic: Southern Cross",
       y="Pedestrians ('000)")

#Forecasting with ETS
#Extract the holiday data from the tourism tsibble
aus_holiday = tourism %>% 
  filter(Purpose == "Holiday") %>% 
  summarise(
    Trips = sum(Trips/1e3)
  )
aus_holiday

#Define and fit model
aus_model = aus_holiday %>% 
  model(
    ETS(Trips)
  )
aus_model
report(aus_model)

#plot the components of the model
components(aus_model) |>
  autoplot() +
  labs(title = "ETS(M,N,A) components")

#forecast
aus_model %>% forecast() %>% autoplot(aus_holiday)

#pigs in aus livestock
#Extract the data of interest
aus_pigs = aus_livestock %>% 
  filter(Animal == "Pigs", State == 'Victoria') %>% 
  select(-State, -Animal)
aus_pigs

#Estimate the simple exponential smoothing model
aus_pigs_fit = aus_pigs %>% 
  model(
    ETS(Count)
  )
aus_pigs_fit
report(aus_pigs_fit)

#produce forecasts
aus_fc = aus_pigs_fit %>% 
  forecast(h= 4)
aus_fc

#visualise the forecasts
autoplot(aus_fc,aus_pigs)

#Let us try and fit Holt Winters damped method
aus_damped = aus_pigs %>% 
  model(
    ETS(Count ~ error("M") + trend("Ad") + season("M"))
  )
aus_damped
report(aus_damped)

#global economy
#Kenyan exports
kenya_exports = global_economy %>% 
  filter(Country == "Kenya", !is.na(Exports)) %>% 
  select(-Country, - Code, -GDP, -CPI, -Imports, - Population, -Growth)
kenya_exports

#plot the main features of the series
kenya_exports %>% autoplot()+labs(
  title = "Kenya Export Data"
)

#Define an ETS(A,N,N) to forecast the series
kenya_ann = kenya_exports %>% 
  model(
    ETS(Exports~ error("A") + trend("N") + season("N"))
  )
kenya_ann
report(kenya_ann)

#forecast using the model and plot the forecasts
kenya_fc = kenya_ann %>% 
  forecast()
kenya_fc

#plot the forecasts
kenya_fc %>% autoplot(kenya_exports)

#compute the RMSE values for the training data
accuracy(kenya_ann)

#visualize the components
components(kenya_ann) %>% autoplot()

#use the ETS(A,A,N) model
kenya_aan = kenya_exports %>% 
  model(
    ETS(Exports~ error("A") + trend("A") + season("N"))
  )
kenya_aan

#check the model parameters and model selection metrics
report(kenya_aan)

#produce forecasts and visualize in a timeplot
aan_fc = kenya_aan %>% 
  forecast()
aan_fc
aan_fc %>% autoplot(kenya_exports)+
  labs(title = "Kenya Export Forecasts using ETS(AAN)")

#check the accuracy
accuracy(kenya_aan)

#Gas from australian production tsibble
aus_gas = aus_production %>% select(Gas)
aus_gas

#Plot the data to see the main features
aus_gas %>% autoplot()

#Find an ETS model
aus_ets = aus_gas %>% 
  model(ETS(Gas ~ error("M") + trend("Ad") + season("M")))
aus_ets

#produce forecasts and visualize
aus_ets %>% forecast() %>% autoplot(aus_gas)

#check the smoothing parameters
report(aus_ets)

#check the accuracy metrics
accuracy(aus_ets)

#Monthly Australian Retail data
set.seed(12345675)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))
myseries

#Exploring the department stores data
myseries %>% autoplot()

#Applying the Holt winters multiplicative method
myseries_hw = myseries %>% 
  model(ETS(Turnover ~ error("M") + trend("A") + season("M")))
myseries_hw

#forecast and plot
myseries_fc = myseries_hw %>% forecast(h = 1)
myseries_fc

#plot
myseries_fc%>% autoplot(myseries)

#check the parameters estimated
report(myseries_hw)

#check the accuracy
accuracy(myseries_hw)

#Experiment with making the trend damped
myseries_damped = myseries %>% 
  model(ETS( Turnover ~ error("M") + trend("Ad") + season("M")))
myseries_damped

#check the estimated parameters and the accuracy metrics
report(myseries_damped)
accuracy(myseries_damped)

#produce forecasts
damped_forecasts = myseries_damped %>% 
  forecast(h = 1)
damped_forecasts

#visualize the forecasts
damped_forecasts %>% autoplot(myseries)

#check the residuals from the damped method
gg_tsresiduals(myseries_damped)
gg_tsresiduals(myseries_hw)

#now find the test set, train the model upto 2010
myseries_train = myseries %>% filter(year(Month) <= 2010)
myseries_train

#train the model on the training set data
myseries1 = myseries_train %>% 
  model(
    ETS(Turnover ~ error("M") + trend("Ad") + season("M"))
  )
myseries1

#checking the model estimation
report(myseries1)

#produce forecasts
myseries1_fc = myseries1 %>% 
  forecast(h = 1)
myseries1_fc

#visualize the forecasts
myseries1_fc %>% autoplot(myseries_train)

#Check the accuracy metrics
accuracy(myseries1_fc,myseries)

#compare this with the seasonal naive method
#The damped method still has a lower RMSE than the seasonal naive
seasonal_series = myseries_train %>% 
  model(
    seasonal_naive = SNAIVE(Turnover)
  )
seasonal_series

#produce forecasts
seasonal_fc = seasonal_series %>% forecast(h = 1)
seasonal_fc

seasonal_fc %>% autoplot(myseries_train)
#check the accuracy of the forecasts
accuracy(seasonal_fc, myseries)

report(seasonal_series)
#Using a Box-cox transformation, apply to STL then ETS
#Box cox transformation
#pull the lambda feature first
lambda_guer = myseries %>% 
  features(Turnover, features = "guerrero") %>% 
  pull(lambda_guerrero)
lambda_guer

#perform the box cox transformation,using lambda from above to stabilize variance
transfomed_series = myseries %>% 
  mutate(
    transformed = box_cox(Turnover, lambda = lambda_guer)
  )
transfomed_series

#visualize the transformed values
transfomed_series %>% autoplot(transformed)+labs(
  title = "Box cox transformations of Turnover",
  y = "Turnover"
)
transfomed_series

#Do STL decomposition
series_STL = transfomed_series %>% 
  model(STL(transformed))
series_STL
components(series_STL)
augment(series_STL)

#Extract the seasonally adjusted data
stl_adjusted = components(series_STL) %>% mutate(
  seasonal_adjust = transformed - season_year
) %>% 
  select(-.model,-State, -Industry)
stl_adjusted

#Extract the training set

#Fit an ETS model
series_ets = stl_adjusted %>% 
  filter(year(Month) <= 2010) %>% 
  model(
    ets = ETS(season_adjust)
  )
series_ets
tail(myseries)

#check the metrics
report(series_ets)

#produce forecasts and plot them
series_ets_fc = series_ets %>% 
  forecast(h= 1 )
series_ets_fc

#plot
series_ets %>% components()
tail(stl_adjusted)

#check the accuracy of the forecasts
accuracy(series_ets_fc, stl_adjusted)

