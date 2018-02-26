

library(jsonlite)

setwd(paste("C:\\Users\\Richard\\Desktop\\MSIM Coursework\\Winter 2018\\INFX 573",
            "\\Final Project\\twitter-hate-speech-identification\\data\\",
            "trump_tweet_data_archive-master\\Condensed", sep=''))
list.files()
t <- fromJSON("condensed_2018.json", flatten=T)
