wine<-read.csv(file.choose()) #importing the file 
View(wine)
attach(wine)
help("princomp")
cor(wine) #finding correlation between the variables
pcaatm<-princomp(wine,cor = TRUE,scores = TRUE)  #finding principal component 
summary(pcaatm)
plot(pcaatm)
biplot(pcaatm)
View(pcaatm)
pcaatm$scores[,1:3] #taking the first three PC values
wine<-cbind(wine,pcaatm$scores[,1:3]) #binding the pc values with the dataset
View(wine)
clus_wine<-wine[,15:17] #preparing for clusturing the data and considering only pcs
View(clus_wine)
norm_clus<-scale(clus_wine) #normalising the data
View(norm_clus)
dist1<-dist(norm_clus,method = "euclidean")  #finding the distance between variables in pcs
dist1

fit1<-hclust(dist1,method="complete") #model building hclust
plot(fit1)
groups<-cutree(fit1,k=5) #using cuttree function
class(groups)
membership<-as.matrix(groups)
table(membership)
final <- data.frame(wine, membership)
View(final)

#k means clustering model building
fit <- kmeans(norm_clus, 5) # 5 cluster solution
str(fit)
table(fit$cluster)
final2<- data.frame(wine, fit$cluster) # append cluster membership
View(final2)
library(data.table)
setcolorder(final2, neworder = c("fit.cluster"))
View(final2)
aggregate(wine[1:178,2:17], by=list(fit$cluster), FUN=mean)

#finding the clusters with the original data
View(wine)
attach(wine)
normalized_data<-scale(wine[,1:14])#normalising the data
View(normalized_data)
dist2<-dist(normalized_data,method = "euclidean") #finding the distance
dist2
fit2<-hclust(dist2,method = "complete") #hclsut method for clustering
plot(fit2)
groups1<-cutree(fit2,k=5) #using cuttree function
class(groups1)
membership<-as.matrix(groups1)  #assigning membership
table(membership)
final1 <- data.frame(wine, membership)
View(final1) #final model with original data before pc

fit3 <- kmeans(normalized_data, 5) # 5 cluster solution #k clustering method
str(fit3)
table(fit3$cluster)
final3<- data.frame(wine, fit3$cluster) # append cluster membership
View(final3)
library(data.table)
setcolorder(final3, neworder = c("fit3.cluster"))
View(final3)
aggregate(wine[1:178,2:17], by=list(fit3$cluster), FUN=mean)
