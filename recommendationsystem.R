#Installing and loading the libraries
#install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab") 
library(caTools)
bookl<-read.csv(file.choose()) #uploading the dataset of books
View(bookl)
class(bookl) #trying to know the dataset format
str(bookl) #trying to the table structre and coloumn structre
table(bookl$Book.Rating) #framing a table with rating of books using rating coloumn 
hist(bookl$Book.Rating) #plotting it
bookrating<-as(bookl, 'realRatingMatrix') #converting the dataset into realrating matrix
book_recomm_model1 <- Recommender(bookrating, method="POPULAR") #recommending the books based on popularity
recommended_items1 <- predict(book_recomm_model1, bookrating[1:20], n=10) #we can give inputs for book rating range and count
as(recommended_items1, "list")
book_recomm_model2 <- Recommender(bookrating, method="UBCF")#userbased collaberative filtering method
recommended_items2 <- predict(book_recomm_model2, bookrating[413:414], n=20)
as(recommended_items2, "list")

