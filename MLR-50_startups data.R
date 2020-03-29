Startups<-read.csv(file.choose())
View(Startups)
attach(Startups)
summary(Startups)
#to find the correlation between output(profit) and input variables(r&d,administraion,marketing spend)
pairs(Startups)
plot(Startups)
cor(Startups)
Startups[4]<-NULL  # We have excluded state coloumn since its not numerical
library(corpcor)
cor2pcor(cor(Startups))
#there is no collinearity problem here input varibales are higly dependand
#defining my model
model.profit<-lm(Startups$Profit~Startups$R.D.Spend+Startups$Administration+Startups$Marketing.Spend,data = Startups)
summary(model.profit)
plot(model.profit)


#this is my final mode residula values and fitted values are almost on the same line no much fluctuations in my model for residuals
