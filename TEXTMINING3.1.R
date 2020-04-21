library(tm)
library(topicmodels)
library(slam)

x <- readLines(file.choose()) #importing the txt file guncontrol
str(x)
length(x)
x
#using tm package#doing data cleansing here
mydata.corpus <- Corpus(VectorSource(x))
mydata.corpus <- tm_map(mydata.corpus,removePunctuation)
my_stopwords <- readLines(file.choose())
mydata.corpus <- tm_map(mydata.corpus,removeWords,my_stopwords)
mydata.corpus <- tm_map(mydata.corpus,removeNumbers)
mydata.corpus <- tm_map(mydata.corpus,stripWhitespace)

#build a term document matrix
mydata.dtm3 <- TermDocumentMatrix(mydata.corpus)
as.matrix(mydata.dtm3) 

dim(mydata.dtm3) #finding the number of terms and docs in my dataset

dtm <- t(mydata.dtm3) #document term matrix
dtm$ncol #number of coloumns
dtm$nrow #number of rows
rowTotals <- apply(dtm,1,sum) #making a set for the number of words in every row
dtm.new <- dtm[rowTotals>0,] #number of words in rows more than 0 or more repeated words

lda <- LDA(dtm.new,10) #making up lda with words from the rows and from the first 1o rows

lterm <- terms(lda,10)
lterm #viewing the terms in first 10 rows from dtm

tops <- terms(lda)
tb <- table(names(tops),unlist(tops))
tb <- as.data.frame.matrix(tb)

cls <- hclust(dist(tb),method = 'ward.D2')
par(family ='HiraKakuProN-W3')
plot(cls)

#emotion mining#

library(syuzhet)
my_example_text <- readLines(file.choose()) #we have to import the file again guncontrol
s_v <- get_sentences(my_example_text)
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v,method = "bing")#using bing method to perform the sentiment analysis
head(sentiment_vector) 

sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

#plot
plot(sentiment_vector,type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h= 0, col= "red")

#to extract the sentance with most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

#more depth
poa_v <- my_example_text
poa_sent <- get_sentiment(poa_v, method = "bing")
plot(poa_sent,type = "h", main = "LOTR using transformed Values",
     xlab = "Narrative Time", ylab = "Emotinal Valence")

#percentage based figures
percent_vals <- get_percentage_values(poa_sent)

plot(percent_vals,type = "l", main = "Throw thr ring in the volcano using percentsge based means",
     xlab = "Narrative Time", ylab = "Emotinal Valence", col="red")


ft_values <- get_transformed_values(poa_sent,
                                    low_pass_size = 3,
                                    x_reverse_len = 100,
                                    scale_vals = TRUE,
                                    scale_range = FALSE)

plot(ft_values, type = "h", main = "LOTR using Transformed values",
     xlab = "Narrative time", ylab = "Emotional Valence",
     col="red")

nrc_data <- get_nrc_sentiment(s_v)  #using nrc method to build my model
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')

#subset
sad_items <- which(nrc_data$sadness>0)
head(s_v[sad_items])

barplot(sort(colSums(prop.table(nrc_data[,1:8]))), horiz = T, cex.names = 0.7,
        las= 1, main = "Emotions", xlab="Percentage",col = 1:8)

