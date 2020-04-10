library(readxl) #dataset is in xl format so using the library of xl here
airline<-read_xlsx(file.choose()) #uploading the dataset
View(airline)
str(airline)

normd1<-scale(airline)#normalising the dataset values
View(normd1)
dist<-dist(normd1,method="euclidean") #finding the distance between data points
dist

model<-hclust(dist,method = "complete") #using hcluster builidng the model
model$labels
plot(model) #plotting the dendrogram
groups<-cutree(model,k=5)  #grouping it through cuttree
class(groups)
membership<-as.matrix(groups) #assigning membership values to groups
table(membership)
final<-cbind(airline,membership)  #binding the input data with memebership table
View(final) #final mode

kcs<-kmeans(normd1,5)#k means clustering model
str(kcs)
table(kcs$cluster) #creating table
final1<-cbind(airline,kcs$cluster) #binding the data alongwith kcluster
View(final1) #final model in k means clustering
