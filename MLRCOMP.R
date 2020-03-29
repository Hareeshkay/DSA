computerdata<-read.csv(file.choose())
View(computerdata)
attach(computerdata)
summary(computerdata)
computerdata[1]<-NULL
View(computerdata)
pairs(computerdata)
plot(computerdata)
model.cd<-lm(computerdata$price~computerdata$speed+computerdata$hd+computerdata$ram+computerdata$screen+computerdata$cd+computerdata$multi+computerdata$premium,data=computerdata)
summary(model.cd)
#this model is not effective and adoptable because R^2 Value is less
