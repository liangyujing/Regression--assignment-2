##Appendix II: R-Script
##Instruction: Run code line by line

# Clear workspace
rm(list=ls())
update.packages(checkBuilt = TRUE)

# Load the ExperienceSampling_Group7.csv
attach(ExperienceSampling_Group7)
my_data<-ExperienceSampling_Group7
summary(my_data)

# Label factors
str(my_data)
beepnum = my_data$beepnum
PA = my_data$PA

# 1. Visualizing data
## Scatter Plot
scatter.smooth(x=beepnum, y=PA, main="PA ~ beepnum")

# 2. Plotting the data     
# Fitting the models
Dummy = ifelse(beepnum<24,0,1)  ## DUMMY (0,1)
## DEGREE(2,3,4,5,6,10)
fit1 <- lm(formula = my_data$PA~my_data$beepnum*Dummy)
fit2 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,2,raw=TRUE))
fit3 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,3,raw=TRUE))
fit4 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,4,raw=TRUE))
fit5 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,5,raw=TRUE))
fit6 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,6,raw=TRUE))
fit10<- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,10,raw=TRUE))
## SUMMARY(可以得到T分数，判断系数是否显著；也可以得到R分数和R adjusted)
summary(fit1); summary(fit2); summary(fit3);summary(fit4);
summary(fit5);summary(fit6);summary(fit10);
anova(fit1,fit2,fit3,fit4,fit5,fit6,fit10)

# Visualising the models  ##画图
library(splines)
plot(PA, main = "Visualisation of Models", xlab="beepnum",pch=20)
lines(stats::lowess(PA), col='black')
lines(predict(fit1), col='red')
lines(predict(fit2), col='green')
lines(predict(fit3), col='orange')
lines(predict(fit10), col='steelblue')
##  角标：依次为名称，位置，颜色，大小。
legend(x = "bottomright", legend = c("Lowess","Linear fit",
                                     "Quadratic fit","Cubic fit",
                                     "Overfitted(Degree 10)"),
       lwd = rep(3,4),
       col = c("black","red","green","orange","steelblue"),
       text.width = 17,cex = 0.80)


# 3. R squared and adjusted R squared
list.1 <- c(seq(1,8),15) # The iteration numbers

#Retrieve
r.squared.list.1 <- lapply(list.1, function(x)
  summary(lm(my_data$PA~poly(my_data$beepnum*Dummy,x,raw=T)))$r.squared)
adj.r.squared.list.1 <- lapply(list.1, function(x)
  summary(lm(my_data$PA~poly(my_data$beepnum*Dummy,x,raw=T)))$adj.r.squared)

#ggplot2  ## 画图
library(ggplot2)
my.data.plot <- data.frame(list.1,unlist(r.squared.list.1),unlist(adj.r.squared.list.1))
colnames(my.data.plot) <- c("Degrees","RSquared","AdjRSquared")
ggplot(my.data.plot, aes(Degrees)) +
  geom_line(aes(y = RSquared, colour = "RSquared")) +
  geom_point(aes(y = RSquared,colour = "RSquared"), shape = 17) +
  geom_line(aes(y = AdjRSquared, colour = "AdjRSquared")) +
  geom_point(aes(y = AdjRSquared,colour = "AdjRSquared"), shape = 5)+
  xlab("Degrees") +
  ylab("Combined R Values") +
  ggtitle("AdjR^2 and R^2")


# 4. Cross validation  ##画图
##perform K-fold cross-validation with K=10
library(boot)
set.seed(1)
deltas <- rep(NA, 10)
for (i in 1:10) {
  fit <- glm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy, i), data =
               my_data)
  deltas[i] <- cv.glm(my_data, fit, K = 10)$delta[1]
}
lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,2,raw=TRUE))
plot(1:10, deltas, xlab = "Degree of polynomial", ylab = "CV MSE", type = "l")
d.min <- which.min(deltas)
points(which.min(deltas), deltas[which.min(deltas)], col = "red", cex = 2, pch = 20)
##D=8 is the optimal degree for the polynomial.


# 5. Information criteria
#calculating AIC and BIC values of each model
AIC(fit1,fit2,fit3,fit4,fit5,fit6,fit10)
BIC(fit1,fit2,fit3,fit4,fit5,fit6,fit10)
#fit5 has lowest AIC and BIC.
#fit3 is slightly higher than fit5.
#both AIC and BIC have a preference for lm5.


# 6. Predicting PA at beepnum = 91   ##基于选出来的最佳公式代入，进行预测
lm3 <- lm(my_data$PA~my_data$beepnum+poly(beepnum*Dummy,3,raw=TRUE))
#bestfit model
y <- my_data$PA
a <- my_data$beepnum
lm3 <- lm(y~a+poly(a,3,raw=TRUE))
predict(lm3,newdata = data.frame(a=91))
predict(lm3, newdata = data.frame(a=91),interval = "confidence")
##the 95% confidence interval associated with a beepnum of 91 is (60.92 85.42).


#7. Checking assumptions
# the normality assumption  ##QQ plot
plot(lm3,2)

# evaluating Nonlinearity   ## 线性
# component + residual plot aka partial-residual plots
##install.packages("haven")
library(car)
crPlots(fit3, data = my_data)
##You should see a pink line that models the residuals of your predictor against your
##dependent variable (i.e., the loess line). The blue striped line represents the line of best
##fit. If your pink line seems to be similarly linear as your blue striped line, you’re good. If
##the pink line appears curved relative to the blue striped line, you likely have a linearity
##problem.

# outliers   ## 极端值
library(car)
outlierTest(fit3,my_data) # Bonferonni p-value for most extreme obs
##10 outliers

# homoscedasticity assumption
ncvTest(fit3)
##p < .05, suggesting that our data is not homoscedastic.

# 8. Citation
citation(package = "base", lib.loc = NULL, auto = NULL)
readCitationFile(file, meta = NULL)
