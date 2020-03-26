delivery_time<- read.csv(file.choose())
attach(delivery_time)
View (delivery_time)
plot(Delivery.Time)
plot(delivery_time$Delivery.Time,delivery_time$Sorting.Time)
cor(Delivery.Time,Sorting.Time)

reg<-lm(Delivery.Time~Sorting.Time)
summary(reg)
reg$fitted.values
confint(reg , level = 0.95)
predict(reg, interval = "predict")
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(delivery_time))
sqrt(mean(reg$residuals^2))
library(ggplot2)
ggplot(data = delivery_time, aes(x = Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery_time, aes(x=Sorting.Time, y=reg))

reg_log<-lm(Delivery.Time~log(Sorting.Time))
summary(reg_log)
reg$fitted.values
confint(reg_log,level = 0.95)
predict(reg_log,interval = "predict")

plot(Sorting.Time,log(Delivery.Time))
cor(Sorting.Time,log(Delivery.Time))
reg_exp<-lm(log(Delivery.Time)~(Sorting.Time))
summary(reg_log)
