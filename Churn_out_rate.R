emp_data<-read.csv(file.choose())
View(emp_data)
attach(emp_data)
plot(emp_data$Churn_out_rate,emp_data$Salary_hike)
cor(Salary_hike,Churn_out_rate)

reg_lm<-lm(emp_data$Churn_out_rate~emp_data$Salary_hike)
summary(reg_lm)
reg_lm$fitted.values
confint(reg_lm, interval=0.95)
predict(reg_lm,level="predict")

reg_log<-lm(Churn_out_rate~log(Salary_hike))
summary(reg_log)            
reg_log$fitted.values
confint(reg_log,interval=0.95)
predict(reg_log,level = "predict")
plot(log(Salary_hike),Churn_out_rate)
cor(log(Salary_hike),Churn_out_rate)

reg_exp<-lm(log(Churn_out_rate)~Salary_hike)
summary(reg_exp)
cor(reg_exp)
reg_exp$fitted.values
confint(reg_exp,interval=0.95)
predict(reg_exp,level = "predict")
cor(log(Churn_out_rate),Salary_hike)
plot(log(Churn_out_rate),Salary_hike)
