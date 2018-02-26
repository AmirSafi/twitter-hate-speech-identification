### Code from https://www.r-bloggers.com/how-to-use-r-to-scrape-tweets-super-tuesday-2016/

# Set working directory
#amir
#setwd("~/Dropbox/MSIM/INFX573/Course_Project/twitter-hate-speech-identification")
#richard
setwd("C:\\Users\\Richard\\Desktop\\MSIM Coursework\\Winter 2018\\INFX 573\\Final Project\\twitter-hate-speech-identification")

# Install needed packages if necessary
pkgs <- c('twitteR','ROAuth', 'httr', 'stringr', 'plyr', 'data.table')
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
library(data.table)

## MAKE AN ACCOUNT AND ENTER IN YOUR CREDENTIALS FROM HERE:
# http://docs.inboundnow.com/guide/create-twitter-application/
# Set API Keys
api_key <- "xOnMCl2XZQf96M3S0E8ebGiEV"
api_secret <- "uSKcaXSklvu69wwLdymo0nZhgaKk6cHpiNTSGzas9Yg6hEq9NZ"
access_token <- "67433123-AZezkqDJswd011NJHjNTTDOwvCWyVfqmgFxd6MqsW"
access_token_secret <- "cZihVqQ2eKW5lnyS6IMaxh0gaQsLI54u1Euly82GUEGtw"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


start <- paste('2018-02-', as.character(17:19), sep='')
end <- paste('2018-02-', as.character(18:25), sep='')

# Gather tweets between these dates
tweets <- data.frame()
for (i in 1:length(start)) {
    query <- searchTwitter('#', n=1000, since=start[i], until=end[i], lang='en')
    results <- data.frame(Text=unlist(lapply(query, function(t) t$getText())), 
              Date=do.call("c", lapply(query, function(t) t$getCreated())),
              Retweets=do.call("c", lapply(query, function(t) t$getRetweetCount())))
    tweets <- rbind(tweets, results)
}
write.csv(tweets, "random tweets.csv", row.names=F)


# Other fields that are aviable include 
#getCreated, getFavoriteCount, getFavorited, getId, getIsRetweet, getLatitude, getLongitude,
#..  getReplyToSID, getReplyToSN, getReplyToUID, getRetweetCount, getRetweeted, getRetweeters, getRetweets,
#..  getScreenName, getStatusSource, getText, getTruncated, getUrls
