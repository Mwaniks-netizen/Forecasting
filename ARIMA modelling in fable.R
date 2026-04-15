#ARIMA Modelling in fable
library(fpp3)

#Export data of Tanzania
central = global_economy %>% 
  filter(Code == "CAF") %>% 
  filter(!is.na(Exports))
central

#plot a timeplot
central %>% autoplot(Exports)+ labs(title = "Central African Republic Exports",
                                     y = "%GDP")
#Differencing
central %>% gg_tsdisplay(difference(Exports),plot_type = "partial")

#Fit a model
central_fit = central %>% model(
  arima013 = ARIMA(Exports ~ pdq(0,1,3)),
  arima210 = ARIMA(Exports ~ pdq(2,1,0)),
  stepwise = ARIMA(Exports),
  searrch = ARIMA(Exports, stepwise = FALSE)
)
central_fit

#Lengthen the dataset
central_fit %>% 
  pivot_longer(!Country,
               names_to = "model_name",
               values_to = "Orders" )
  
#Model selection
glance(central_fit) %>% 
  arrange(AICc) %>% 
  select(.model:BIC)

#lets investigate the model we have found
central_fit %>% 
  select(searrch) %>% 
  gg_tsresiduals()

#ljung box test to check if residuals and ACF plots are okay
augment(central_fit) %>% 
  filter(.model == "searrch") %>% 
  features(.innov, ljung_box, lag = 10, dof = 3)#lag 10 because its nonseasonal data

#forecast using the model
central_fit %>% 
  forecast(h = 5) %>% 
  filter(.model =="searrch") %>% 
  autoplot(central)
