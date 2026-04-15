library(fpp3)
install.packages("GGally")
library(GGally)
#using the tslm function for linear reg
#simple linear regression
fit_cof = us_change %>% 
  model(lm = TSLM(Consumption~ Income))
fit_cof
report(fit_cof)

#plot the relationship
ggplot(us_change,
       aes(x = Income, y = Consumption))+
  geom_point()+
  geom_smooth(method = "lm", se= FALSE)
us_change %>% 
  select(-Consumption, -Income) %>% 
  pivot_longer(-Quarter) %>% 
  ggplot(aes(Quarter, value, colour = name))+
  geom_line()+
  facet_grid(name~., scales= "free_y")+
  guides(colour = "none")

#visualizing the relationship
us_change %>% 
  GGally::ggpairs(columns = 2:6)

#multiple linear regression
multiple_fit = us_change %>% 
  model(lm = TSLM(Consumption ~ Income + Production + Savings + Unemployment))
report(multiple_fit)

#visualizing
augment(multiple_fit) %>% 
  ggplot(aes(x= Quarter))+
  geom_line(aes(y = Consumption, colour = "Data"))+
  geom_line(aes(y = .fitted, colour = "Fitted"))+
  labs(
    y = NULL,
    title = "Percentage change in US Consumption"
  )+
  scale_color_manual(values = c(Data = "blue", Fitted = "green"))+
  guides(colour = guide_legend(title = NULL))

#evaluating the regression model
#checking residuals
multiple_fit %>% gg_tsresiduals()

#Residual plotted against predictors(should show no pattern)
us_change %>% 
  left_join(residuals(multiple_fit), by = "Quarter") %>% 
  pivot_longer(Income:Unemployment,
               names_to = "Regressor",
               values_to = "x" ) %>% 
  ggplot(aes(x = x, y = .resid))+
  geom_point()+
  facet_wrap(~Regressor, scales = "free_x")

#Residuals plotted against fitted values
augment(multiple_fit) %>% 
  ggplot(aes(x = .resid, y = .fitted))+
  geom_point()
