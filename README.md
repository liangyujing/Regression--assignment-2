# Clear workspace
rm(list=ls())

# Load the ExperienceSampling_Group7.csv
attach(ExperienceSampling_Group7)
my_data<-ExperienceSampling_Group7
summary(my_data)

# label factors
str(my_data)
before_exam<-factor(beepnum<24)
after_exam<-factor(beepnum>23)
PA<-factor(labels = c("PA")

# 1. descriptive statistics
# mean, sd
aggregate(my_data, list(beepnum,PA), mean)
aggregate(my_data, list(beepnum,PA), sd)

# 2. Visualizing data
#2.1 Loess
library(ggplot2)
ggplot(my_data, aes(beepnum, PA) ) +
  geom_point() +
  stat_smooth()

# 3. Building regression model
#3.1 Fitting a linear model (degree 1)
lm1 <- lm(PA ~ beepnum, my_data)
coef(lm1)
summary(lm1)
#the model explains about 4.4% of the variability of the data. 
confint(lm1)

#3.2 Fitting a polynomial of degree 2
lm2 <- lm(PA ~ beepnum + I(beepnum^2))
summary(lm2)
#3 t-tests seem to indicate that all the coefficients are statistically significant. 
#the model explains about 39% of the variability of the data. 

#3.3 Fitting a polynomial of degree 3
lm3 <- lm(PA ~ beepnum + I(beepnum^3))
summary(lm3)
#3 t-tests seem to indicate that all the coefficients are statistically significant. 
#the model explains about 34% of the variability of the data. 

# 4 Information criteria
AIC(lm1,lm2,lm3)
BIC(lm1,lm2,lm3)
#Models with lowest AIC and/or BIC will be preferred. 
#Here, both criteria agree for rejecting lm1 with high confidence. 
#both AIC and BIC has a very slight preference for lm2. 
