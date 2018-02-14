### Code from https://www.r-bloggers.com/how-to-use-r-to-scrape-tweets-super-tuesday-2016/


#---- head ----#
# Install needed packages if necessary
pkgs <- c('twitteR','ROAuth', 'httr', 'stringr', 'plyr')
for (pkg in pkgs) {
    if(!(pkg %in% rownames(installed.packages()))) {
        install.packages(pkg)
    }
}
options(error=traceback)
options(show.error.locations = TRUE)
#---- /head ----#

library(twitteR)
library(ROAuth)
library(httr)

## MAKE AN ACCOUNT AND ENTER IN YOUR CREDENTIALS FROM HERE:
# http://docs.inboundnow.com/guide/create-twitter-application/
# Set API Keys
api_key <- "********"
api_secret <- "********"
access_token <- "****************"
access_token_secret <- "********"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# You are in.
# Grab latest tweets
tweets_sanders <- searchTwitter('@BernieSanders', n=1500)

# Loop over tweets and extract text
library(plyr)
feed_sanders = laply(tweets_sanders, function(t) t$getText())

######## Set working directory
setwd("C:\\Users\\Richard\\Desktop\\DS1_Research\\Data Mining")

# Hu and Liu opinion lexicon 5000 words
# Unpack it
# Read in dictionary of positive and negative works
yay = scan('opinion-lexicon-English/positive-words.txt',
                  what='character', comment.char=';')
boo = scan('opinion-lexicon-English/negative-words.txt',
                  what='character', comment.char=';')
# Add a few twitter-specific negative phrases
bad_text = c(boo, 'wtf', 'epicfail', 'douchebag')
good_text = c(yay, 'upgrade', ':)', '#iVoted', 'voted')

score.sentiment = function(sentences, good_text, bad_text, .progress='none') {
    require(plyr)
    require(stringr)
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array of scores back, so we use
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, good_text, bad_text) {
        
        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence = gsub('[[:punct:]]', '', sentence)
        sentence = gsub('[[:cntrl:]]', '', sentence)
        sentence = gsub('\\d+', '', sentence) # Tutorial messes up here. Needs 2 escapes
        #to remove emojis
        sentence <- iconv(sentence, 'UTF-8', 'ASCII')
        sentence = tolower(sentence)        
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, '\\s+')
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
        
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, good_text)
        neg.matches = match(words, bad_text)
        
        # match() returns the position of the matched term or NA
        # we just want a TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
        
        return(score)
    }, good_text, bad_text, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
}


### This references a field not defined
# # Call the function and return a data frame
# feelthabern <- score.sentiment(feed_sanders, good_text, bad_text, .progress='text')
# # Cut the text, just gets in the way
# plotdat <- feelthabern#[c("name", "score")]
# # Remove neutral values of 0
# plotdat <- plotdat[!plotdat$score == 0, ]
# 
# # Nice little quick plot
# qplot(factor(score), data=plotdat, geom="bar", 
#       xlab = "Sentiment Score")
