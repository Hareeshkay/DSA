library(arules) #these packasges are needed for aprioriy algorithm
library(arulesViz) #package for visualisation
groceries<-read.transactions(file.choose(),format="basket") #since groceries dataset contains names we need to make it into transactions format
class(groceries)
inspect(groceries[1:20]) 

groceries_rule<-apriori(groceries,parameter = list(support=0.003,confidence=0.06,minlen=2)) #setting this rule for the first time
inspect(groceries_rule[1:10]) #inspecting the number of associations and count
plot(groceries_rule) #plotting the first rule

groceries_rule1<-apriori(groceries,parameter = list(support=0.001,confidence=0.10,minlen=4))#second rule with minimum support values and max minlen
inspect(groceries_rule1[1:10]) #inspecting the first 10 among the rules
plot(groceries_rule1)

groceries_rule2<-apriori(groceries,parameter = list(support = 0.005,confidence = 0.05,minlen=1))#third rule with more support value and medium level of confidence with less minlen value
inspect(groceries_rule2[1:10])
plot(groceries_rule2)
