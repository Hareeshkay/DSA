library(rvest) #package required for extraction
library(XML)
library(magrittr) #package required to read from html pages
JOKER <- NULL # finding joker movie review from IMDB
rev <- NULL
url <- "https://www.imdb.com/title/tt7286456/reviews?ref_=tt_urv"
murl <- read_html(as.character(paste(url,1, sep = "")))
rev <- murl %>% html_nodes(".show-more__control") %>% html_text() #USING html node as showmorecontrol
JOKER <- c(JOKER,rev)

write.table(JOKER, "JOKER.txt") #exporting the reveiws to wd
getwd()

library(syuzhet)#package required for sentiment analysis
SentVect<-readLines(file.choose())  #choosing the file which we got from reveiw extraction
SentVect1<-get_sentences(SentVect) 
class(SentVect1)
length(SentVect1)
str(SentVect1)
head(SentVect1)
Sentiment<-get_sentiment(SentVect1,method = "bing") #using bing method for analysis
head(Sentiment)
sum(Sentiment)
mean(Sentiment)
summary(Sentiment)

plot(Sentiment,type = "l", main = "plot trajecory",xlab = "Narrative time",ylab = "emotional valence")

#to extract the sentance with most negative emotional valence
negative <- SentVect1[which.min(Sentiment)]
negative

# and to extract most positive sentence
positive <- SentVect1[which.max(Sentiment)]
positive

#NRC method for sentiment analysis
nrc_data <- get_nrc_sentiment(SentVect1)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')

#subset
sad_items <- which(nrc_data$sadness>0)
head(SentVect1[sad_items])

barplot(sort(colSums(prop.table(nrc_data[,1:8]))), horiz = T, cex.names = 0.7,
        las= 1, main = "Emotions", xlab="Percentage",col = 1:8)
