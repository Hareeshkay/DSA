COROLLA <-read.csv(file.choose())
View(COROLLA)
attach(COROLLA)
#excluded unwated coloumns
COROLLA<-COROLLA[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(COROLLA)
summary(COROLLA)
pairs(COROLLA)
plot(COROLLA)
cor(COROLLA)
library(corpcor)
cor2pcor(cor(COROLLA))
#there is a negative correlation between some of the input varibales and output variable, and there is no collinearity problem exists here
model.corrola<-lm(COROLLA$Price~COROLLA$Age_08_04+COROLLA$KM+COROLLA$HP+COROLLA$cc+COROLLA$Doors+COROLLA$Gears+COROLLA$Quarterly_Tax,data=COROLLA)
summary(model.corrola)
plot(model.corrola)

#this is the final mode. R^2 value and adjusted R^2 value looks good and when km and Age increase price decreases