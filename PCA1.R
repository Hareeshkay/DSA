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
fit1<-hclust(dist1,method="complete") #model building
plot(fit1)
groups<-cutree(fit1,k=5) #using cuttree function
class(groups)
membership<-as.matrix(groups)
table(membership)
final <- data.frame(wine, membership)
View(final)
