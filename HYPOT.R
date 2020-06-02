#####################cutlets################
cutlet<-read.csv(file.choose())#importing the data set
View(cutlet)
shapiro.test(cutlet$Unit.A) #normality test for both units
shapiro.test(cutlet$Unit.B)
var.test(cutlet$Unit.A,cutlet$Unit.B)#finding th variance,
#p-value = 0.3136 p high null fly> equal variances
t.test(cutlet$Unit.A,cutlet$Unit.B,alternative = "two.sided",conf.level = 0.95,correct="true") #t test
#mean of x mean of y 
#7.019091  6.964297 
#p-value = 0.4723 null fly
t.test(cutlet$Unit.A,cutlet$Unit.B,alternative = "greater",var.equal = T)
#p-value = 0.2361
# alternative = "greater means true difference is greater than 0
#p high null fly, 
#there is no significant difference in the diameters of Unit A and Unit B


##############LABORATORY-TAT###########
laboratory<-read.csv(file.choose()) #importing the dataset for laboratory TAT
View(laboratory)
Stackeddata<-stack(laboratory) #shaping the dataset to stack view
View(Stackeddata)
var.test(Stackeddata$values,Stackeddata$ind) #vairance test,p high null fly
anovaresult<-aov(values~ind,data=Stackeddata) #y is continous and x is discrete in more than two>anova test
summary(anovaresult)
#p hig null fly, no significant difference in TAT


#################BUYER RATIO##################
Buyer<-read.csv(file.choose())
View(Buyer)
stck<-stack(Buyer)
View(stck)
attach(stck)
#y is discrete and x is also descrete with more than two variable
table(values,ind)
chisq.test(table(values,ind))
#P HIGH NULL FLY there is no significant difference in male and female

#############customer order form############
costomer<-read.csv(file.choose())
View(costomer)
attach(customer)
Costomer$India <- ifelse(Costomer$India=='Defective',0,1)
Costomer$Phillippines <- ifelse(Costomer$Phillippines=='Defective',0,1)
Costomer$Indonesia <- ifelse(Costomer$Indonesia=='Defective',0,1)
Costomer$Malta <- ifelse(Costomer$Malta=='Defective',0,1)
View(Costomer)
Costomer <- stack(Costomer)
View(Costomer)
colnames(Costomer)<- c("values","country")
View(costomer)
table(values,Country)
chisq.test(table(Costomer))

##################################Fantaloons##############
Faltoons <- read.csv(file.choose())
attach(Faltoons)
View(Faltoons)
table(Weekdays)
table(Weekend)
prop.test(x=c(113,167),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
