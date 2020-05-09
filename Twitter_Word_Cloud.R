---
#Twitter Word Cloud Using R 
#NBA finals 2019
#June 2, 2019 
#Game 2 NBA Finals - Toronto Raptors
# Text mining of recent tweets stating #wethenorth, 
# Exploratory analysis of consumer sentiments 

# Keywords: #WeTheNorth” 
#Number of tweets = 1,000 
# Display 250 most frequent words 

install.packages("twitteR")
install.packages("RCurl")
install.packages("tm")
install.packages("wordcloud")
require(twitteR)
require(RCurl)
require(tm)
require(wordcloud)

#consumer_key <- ******
  consumer_secret <- ******
  access_token <- ******
  access_secret <-  ******
  setup_twitter_oauth(consumer_key, consumer_secret, access_token , access_secret)

#Log into Twitter at https://developer.twitter.com/en/apps
#Click on create App
#Insert consumer key…

#WeTheNorth_tweets = searchTwitteR("WeTheNorth", n=1000, lang="en")
#WeTheNorth_text = sapply(WeTheNorth_tweets, function(x) x$getText())

# Create a corpus
#WeTheNorth_corpus = Corpus(VectorSource(WeTheNorth_text))

# Inspect the corpus
#inspect(WeTheNorth_corpus[1])

# Clean the corpus by removing punctuation, numbers, and white spaces
#WeTheNorth_clean  <- tm_map(WeTheNorth_corpus, removePunctuation)
#WeTheNorth_clean  <- tm_map(WeTheNorth_clean, removeNumbers)
#WeTheNorht_clean  <- tm_map(WeTheNorth_clean, stripWhitespace)

# Create Word Cloud from cleaned data
#wordcloud(WeTheNorth_clean)

# Modify your Word Cloud
#wordcloud(WeTheNorth_clean, random.order = F, max.words = 250, scale = c(4, 0.5))

#wordcloud(WeTheNorth_clean, random.order = F, max.words = 250, scale = c(4, 0.5), color = rainbow(50))
#RMarkdown---
The Toronto Raptors just concluded a nail-biting game two of the NBA series. We live on the West Coast and people are not excited for our home team. Go Raptors, yay! As we followed the scores on our phones, we conducted a text mining of recent tweets to see what people were saying about “WeTheNorth” on Twitter. In technical terms we created a word cloud for the term “WeTheNorth” using at least 1,000 of the most recent tweets, showing 250 of the most frequent words used. The last 30 minutes of the game revealed these tweets:
  
Some of the words surrounding WeTheNorth were NBA finals, Barak Obama, presidential, dubnation, Raptors, NBA, CA, Warriors, chills, Jurassic Park, West, Mississauga, and of course cutie patootie Drake. Unfortunately, by about 8:50 p.m. EST, the Raptors suffered a heart-breaking loss, and these were some of the tweets surrounding Wethenorth, NBA finals, Curry, trade, hard, missed, damn, tough, golden, and f@#%! We are disappointed that our team lost tonight, but we are die-hard fans who will be tuning in for game 3 on Wednesday! 
  
#Additional section – Plot Frequencies
install.packages("httr")
install.packages("rtweet")
library("httr")
library("rtweet")

#the name of the twitter app created by you
#Text Mining for NBA Finals <- "tweet-analytics"
#consumer api key (replace the following sample with your key)
key <- ******
# api secret (replace the following with your secret)
#secret <- ******
# create token named "twitter_token"
token <- ******
  
access_secret <- ****** 
setup_twitter_oauth(consumer_key, consumer_secret, access_token , access_secret)
#app = Text Mining for NBA Finals,
#Downloading the tweets posted for WeTheNorth
WeTheNorth_tweets <- get_timeline("WeTheNorth", n = 1000)

#Use ggplot2 and lubridate library to plot charts and work with specific dates
install.packages("ggplot2")
install.packages("lubridate")
library("ggplot2")
library("lubridate")
ggplot(data = WeTheNorth(),
       aes(month(created_at, label=TRUE, abbr=TRUE),
           group=factor(year(created_at)), color=factor(year(created_at))))+
geom_line(stat="count") +
geom_point(stat="count") +
labs(x="Month", colour="Year") +
xlab("Month") + ylab("Number of tweets") +
theme_minimal()

#Chart by Plotting Only Year-Wise Tweet Counts
ggplot(data = WeTheNorth_tweets, aes(x = year(created_at))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Year") + ylab("Number of tweets") +
  scale_x_continuous (breaks = c(2006:2019)) +
  theme_minimal() +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4")

#Frequency Over 7 Days 
ggplot(data = WeTheNorth _tweets, aes(x = month(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Month") + ylab("Number of tweets") + 
  theme_minimal() +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4")

#Frequency Over 7 Days
ggplot(data = WeTheNorth _tweets, aes(x = wday(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Day of the week") + ylab("Number of tweets") + 
  theme_minimal() +
  scale_fill_gradient(low = "turquoise3", high = "darkgreen")

#Frequency over 7 days
# package to store and format time of the day
install.packages("hms")
# package to add time breaks and labels
install.packages("scales")
library("hms")
library("scales")
# Extract only time from the timestamp, i.e., hour, minute and second 
WeTheNorth_tweets$time <- hms::hms(second(WeTheNorth _tweets$created_at), 
                                   minute(WeTheNorth _tweets$created_at), 
                                   hour(WeTheNorth _tweets$created_at))
# Converting to `POSIXct` as ggplot isn’t compatible with `hms`
WeTheNorth_tweets$time <- as.POSIXct(WeTheNorth _tweets$time)
ggplot(data = WeTheNorth_tweets)+
  geom_density(aes(x = time, y = ..scaled..),
               fill="darkolivegreen4", alpha=0.3) + 
  xlab("Time") + ylab("Density") +
  scale_x_datetime(breaks = date_breaks("2 hours"), 
                   labels = date_format("%H:%M")) +
  theme_minimal()

#Sentiment Analysis
install.packages("syuzhet")
library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
tweet_text <- iconv(tweet_text, from="UTF-8", to="ASCII", sub="")
# removing retweets
tweet_text<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweet_text)
# removing mentions
tweet_text<-gsub("@\\w+","",tweet_text)
WeTheNorth_sentiment<-get_nrc_sentiment((tweet_text))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal() 



