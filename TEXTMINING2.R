library(rvest) #packages required for extraction
library(XML) 
library(magrittr) #package for searching in html pages 

# Amazon Reviews #############################
aurl <- "https://www.amazon.in/product-reviews/B01LXMHNMQ/ref=cm_cr_getr_d_paging_btm_4?ie=UTF8&reviewerType=all_reviews&showViewpoints=1&sortBy=recent&pageNumber=1"
amazon_reviews <- NULL  #creating a null directory to load the reviewdata
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,sep = "")))
  rev <- murl %>% #pipe command to do a quick search
    html_nodes(".a-text-bold") %>%  #searching in reviews with the html node as textbold
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"apple.txt")  #exporting the data into my WD,as txt file
getwd()

#for emotion mining
library(syuzhet) #this package is required
my_example_text <- readLines(file.choose()) #import apple.txt
s_v <- get_sentences(my_example_text) #this is the dataset for sentiment analysis 
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v,method = "bing") #using bing method for sentiment analysis
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

nrc_data <- get_nrc_sentiment(s_v) #nrc method for sentiment analysis, we can use either bing or nrc
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')

#subset
sad_items <- which(nrc_data$sadness>0)
head(s_v[sad_items])

barplot(sort(colSums(prop.table(nrc_data[,1:8]))), horiz = T, cex.names = 0.7,
        las= 1, main = "Emotions", xlab="Percentage",col = 1:8)
