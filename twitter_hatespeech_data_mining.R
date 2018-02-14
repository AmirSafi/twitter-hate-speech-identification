### Code from https://www.r-bloggers.com/how-to-use-r-to-scrape-tweets-super-tuesday-2016/

# Set working directory
setwd("~/Dropbox/MSIM/INFX573/Course_Project/twitter-hate-speech-identification")

# Install needed packages if necessary
pkgs <- c('twitteR','ROAuth', 'httr', 'stringr', 'plyr')
for (pkg in pkgs) {
        if(!(pkg %in% rownames(installed.packages()))) {
                install.packages(pkg)
        }
}

# Allows for error stack tracking 
options(error=traceback)
options(show.error.locations = TRUE)

# Load the libraries 
library(twitteR)
library(ROAuth)
library(httr)

## MAKE AN ACCOUNT AND ENTER IN YOUR CREDENTIALS FROM HERE:
# http://docs.inboundnow.com/guide/create-twitter-application/
# Set API Keys
api_key <- "xOnMCl2XZQf96M3S0E8ebGiEV"
api_secret <- "uSKcaXSklvu69wwLdymo0nZhgaKk6cHpiNTSGzas9Yg6hEq9NZ"
access_token <- "67433123-AZezkqDJswd011NJHjNTTDOwvCWyVfqmgFxd6MqsW"
access_token_secret <- "cZihVqQ2eKW5lnyS6IMaxh0gaQsLI54u1Euly82GUEGtw"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Grab random tweets before and after the election 
# The API is only giving me random tweets from today - Will look into this -Amir 
tweets_post_election <- searchTwitter('hashtag', since = '2016-11-08' , n=1500)
#tweets_pre_election <- searchTwitter('hashtag', since = '2016-01-01', until = '2016-11-08' , n=1500)
#tweets_post_election <- searchTwitter('hashtag', n=1500)

# Get the text of the tweets 
post_text <- laply(tweets_post_election, function(t) t$getText())
# Get the screen name of each persons tweet 
post_screenname <- laply(tweets_post_election, function(t) t$getScreenName())
# Get the latitude and longitude for each tweet 
post_latitude <- laply(tweets_post_election, function(t) t$getLatitude())
post_longitude <- laply(tweets_post_election, function(t) t$getLongitude())
# Get the retweet count for each tweet 
post_retweetcount <- laply(tweets_post_election, function(t) t$getRetweetCount())
# Other fields that are aviable include 
#getCreated, getFavoriteCount, getFavorited, getId, getIsRetweet, getLatitude, getLongitude,
#..  getReplyToSID, getReplyToSN, getReplyToUID, getRetweetCount, getRetweeted, getRetweeters, getRetweets,
#..  getScreenName, getStatusSource, getText, getTruncated, getUrls,

# Create a dataframe and save tweets to csv file 
tweets_post.df <- twListToDF(tweets_post_election)
dim(tweets_post.df)
colnames(tweets_post.df)
subset_tweets <- subset(tweets_post.df, select=c("text", "retweetCount", "retweeted", "created"))
write.csv(subset_tweets, file = 'tweets_post_election.csv')
