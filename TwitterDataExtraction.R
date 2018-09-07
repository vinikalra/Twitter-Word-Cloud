install.packages(c("twitteR", "ROAuth", "base64enc", "httpuv", "tm", "SnowballC", "wordcloud", "RColorBrewer"))

library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

cred <- OAuthFactory(consumerKey ="hT8MN5GBo6lCRhlU0NNbLIjDs",
                     consumerSecret="LV2yxdO60skASAQDFPLmlElmzSydXnGHr40qXDv2TbJ9BN8qOR",
                     requestURL="https://api.twitter.com/oauth/request_token",
                     accessURL="https://api.twitter.com/oauth/access_token",
                     authURL="https://api.twitter.com/oauth/authorize")

#Consumer Key, Consumer Secret, Access Token, Access Secret
setup_twitter_oauth("hT8MN5GBo6lCRhlU0NNbLIjDs",
                    "LV2yxdO60skASAQDFPLmlElmzSydXnGHr40qXDv2TbJ9BN8qOR",
                    "782734221840310273-rP94j6y1BkmHspGQIoI6gY0EMscxk4M",
                    "2xAzHAcLTwRzwZhHZTsgLenEktH5Bxg0jlH21lwIhfLD8")


#Extracting Tweets based on particuar words
mach_tweets = searchTwitter('meghan', n=1500, lang = 'en', resultType = 'recent')
class(mach_tweets)
str(mach_tweets)
mach_tweets[1:10]

###Preparing the Data###

#Getting Texts from Tweets
mach_text = sapply(mach_tweets, function(x) x$getText())
mach_text[1]

#Creating a Corpus
myCorpus = Corpus(VectorSource(mach_text))
myCorpus
inspect(myCorpus[1])

#Removing Emojis
myCorpus = tm_map(myCorpus, content_transformer(gsub), pattern="\\W", replace=" ")

#Removing URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

#Transforming all the letters to lowercase
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#Removing Non-English letters and Space
removeAlpha <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeAlpha))

#Removing Punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

#Removing Numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

#Removing Stopwords
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))

#Removing WhiteSpaces
myCorpus <- tm_map(myCorpus, stripWhitespace)

#Clean Frequent words based on your case
myCorpus <- tm_map(myCorpus, removeWords, c("meghan", "markle", "wedding", "the"))


#Term Document Matrix to create Word Cloud

dtm <- TermDocumentMatrix(myCorpus)

m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq= v)
head(d, 20)

#Word Cloud

set.seed(4587)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 100, 
          random.order = FALSE, colors = brewer.pal(8, "Dark2"))
