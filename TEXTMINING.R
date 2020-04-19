install.packages("twitteR") #these packages are needed to extract data from twitter
library("twitteR")
install.packages("ROAuth") #this package is required for authentication
library("ROAuth")
cred <- OAuthFactory$new(consumerKey='43hHJkZa4snEM8vbikYur2C0V', # Consumer Key (API Key)
                         consumerSecret='T3nKt2VN2qtjMc3sHDRumSXfTuxSIDPbGIToFbBa2kFtgx7BKM', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

install.packages("base64enc") #running the package for encode/decode in 64 bit
library(base64enc)

install.packages("httpuv") #converting the raw vector
library(httpuv)

setup_twitter_oauth("43hHJkZa4snEM8vbikYur2C0V", # Consumer Key (API Key)
                    "T3nKt2VN2qtjMc3sHDRumSXfTuxSIDPbGIToFbBa2kFtgx7BKM", #Consumer Secret (API Secret)
                    "529590041-AJNiB9OMFcaZuCqzoN99fH4pyfUN8uJ9EXDBwY5B",  # Access Token
                    "frz56qjS1TXT1jmBcIWEmXGFLXQu1X3gUswyvMEdCMOfj")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('iamsrk', n = 3000,includeRts = T) #trying to get the first 3k tweets from sharukhkhan

TweetsDF <- twListToDF(Tweets) #setting an object for distribution function
View(TweetsDF)
write.csv(TweetsDF, "Tweets.csv") #exporting the tweets to working directory

#i got a csv file from tweets and removed unwanted information and copied only tweets text and manually made a txt file and imported again for sentiment analysis
#emotion mining#

library(syuzhet) #package required for sentiment analysis
my_example_text <- readLines(file.choose()) #import the srk txt, i have changed the csv file to txt by adding only the tweets
s_v <- get_sentences(my_example_text)
class(s_v) #trying to know the class
str(s_v) #structure of file
head(s_v) #first lines of the dataset

sentiment_vector <- get_sentiment(s_v,method = "bing") #we are using bing method for sentiment analysis
head(sentiment_vector) #first lines of the model

sum(sentiment_vector)  #finding out the sum,mean,summary after using the bing method
mean(sentiment_vector)
summary(sentiment_vector)

#plot
plot(sentiment_vector,type = "l", main = "Plot Trajectory", #using plot l for lines and trajecorty plot is used
       xlab = "Narrative Time", ylab = "Emotional Valence") #xaxis as narrative time and y as emotional valence
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

nrc_vector <- get_sentiment(s_v,method = "nrc") #using nrc model to do sentiment analysis
head(nrc_vector) #header of the dataset after nrc
nrc_data <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')

#subset
sad_items <- which(nrc_data$sadness>0)
head(s_v[sad_items])

barplot(sort(colSums(prop.table(nrc_data[,1:8]))), horiz = T, cex.names = 0.7,
           las= 1, main = "Emotions", xlab="Percentage",col = 1:8)
