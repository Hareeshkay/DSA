crimedata<-read.csv(file.choose()) #uploading the dataset
View(crimedata)
attach(crimedata)

str(crimedata) #trying to know the structure of the data set
mydata<-crimedata[,2:5] #including only integer values and excluding first coloumn
View(mydata)
norm_data<-scale(mydata) #normalising the data
dist_data<-dist(norm_data,method = "euclidean") #trying to find the distance between data points
dist_data
plot(dist_data) #ploting the distances

fit<-hclust(dist_data, method = "complete") #creating my model with hclust
fit$labels
plot(fit)  #ploted a dentogram
groups<-cutree(fit,k=5) #using cuttree function to segregate
class(groups)
membership<-as.matrix(groups)
table(membership) #Assigning membership
final<-cbind(crimedata,membership) #final model
View(final)

kcs<-kmeans(norm_data,5)#k means clustering model
str(kcs)
table(kcs$cluster)
final1<-cbind(crimedata,kcs$cluster)
View(final1) #final model in k means clustering 
