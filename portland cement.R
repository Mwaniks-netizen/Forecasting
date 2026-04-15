#loading the necessary libraries
#Cement production
library(fpp3)

#Extract the recent production of cement
portland_cement = aus_production %>% select(Cement) %>% 
  filter(year(Quarter)> 1990)
portland_cement

portland_cement %>% filter(!is.na(Cement))
#plot the production
portland_cement %>% autoplot(Cement)
#stretch the tsibble
portland_stretch = portland_cement %>% 
  stretch_tsibble(
  .init = 20, .step = 1)
view(portland_stretch)

#Define and fit an ETS model
portland_ets = portland_stretch %>% 
  model(
    ets = ETS(Cement)
  )
portland_ets
view(portland_ets)
#Define and fit a Seasonal Naive model
portland_seasonal = portland_stretch %>% 
  model(
    snaive = SNAIVE(Cement)
  )
portland_seasonal

#produce one year forecasts with the ETS model
portland_ets_fc = portland_ets %>% 
  forecast(h = 4)
portland_ets_fc
view(portland_ets_fc)
#produce one year forecasts with seasonal naive
portland_seasonal_fc = portland_seasonal %>% 
  forecast(h = 4)
portland_seasonal_fc

#check the forecast accuracy metrics for 4 step ahead only
#ETS
accuracy(portland_ets_fc, portland_cement)
accuracy(portland_seasonal_fc, portland_cement)

#check AICc
portland_seasonal %>% glance()
portland_ets %>% glance()
