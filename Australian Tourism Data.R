#Analyzing arrivals from New zealand to Australia
#loading the necessary library
library(fpp3)

#Extract the arrivals from new zealand
newzealand = aus_arrivals %>% 
  filter(Origin == "NZ") %>% 
  mutate(non_na_time = row_number()) %>% 
  update_tsibble(index = non_na_time, regular = TRUE)
newzealand

#Visualize the data 
newzealand %>% autoplot(Arrivals)

#Extract the training set
#Withhold last two years of available data
newzealand_train = newzealand %>% 
  mutate(
    yearr = year(Quarter),
    Quarterr = quarter(Quarter)
  ) %>% 
  filter(Quarterr < 4 , yearr <= 2010 ) %>% 
  select(Quarter, Origin, Arrivals ) %>% 
  mutate(non_na_time = row_number()) %>% 
  update_tsibble(index = non_na_time, regular = TRUE)
newzealand_train
tail(newzealand_train)

newzealand_train %>% autoplot(Arrivals)

#Define and fit Holt winters Multiplicative method
newzealand_model = newzealand_train %>% 
  model(
    ets = ETS(Arrivals ~ error("M") + trend("A") + season("N")))
newzealand_model

#check the level,alpha,beta components
report(newzealand_model)

#produce forecasts
newzfc = newzealand_model %>% 
  forecast(h = 8)
newzfc

#plot these forecasts
newzfc %>% autoplot(newzealand_train) +autolayer(newzfc)

#plot the components of the model
components(newzealand_model) %>% 
  autoplot()

#check the accuracy of the forecasts on the test data
accuracy(newzfc, newzealand)

#Fitting an additive ETS model to the log transformed series
newa = newzealand_train %>% 
  model(ETS(log(Arrivals)))
newa

#forecast using the model
newa_fc = newa %>% forecast(h= 8)
newa_fc

#plot the forecasts
newa_fc %>% autoplot(newzealand_train)

#check the accuracy of the models
accuracy(newa_fc, newzealand)

#seasonality check
gg_season(newzealand_train)

#Fitting a seasonal naive model
news = newzealand_train %>% model(
    RW = RW(Arrivals ~ drift()),
    mean = MEAN(Arrivals),
    Naive = NAIVE(Arrivals)
  )
news

#produce forecasts
newsfc = news %>% forecast(h = 8) 
newsfc

#plot the forecasts
newsfc %>% autoplot(newzealand_train)

#check the accuracy
accuracy(newsfc, newzealand)
newzealand_train %>% autoplot()
