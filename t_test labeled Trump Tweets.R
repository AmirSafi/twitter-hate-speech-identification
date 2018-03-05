
library(data.table)

setwd("C:/Users/Richard/Desktop/MSIM Coursework/Winter 2018/INFX 573/Final Project/twitter-hate-speech-identification/data")
list.files(pattern="*.csv")

raw <- read.csv("final_trump_labels.csv", stringsAsFactors=F)
t <- data.table(raw[,3:ncol(raw)])

names(t) <- c("tweet", "dt_str", "retweets", "followers", "label")

## Make datetime field
mo <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
months <- c(paste('0', 1:9, sep=''),'10','11','12')

dt_parts <- sapply(t$dt_str, function(row) strsplit(row, ' '))
year  <- unlist(lapply(dt_parts, function(part) part[6]))
month <- unlist(lapply(dt_parts, function(part) months[mo==part[2]]))
day   <- unlist(lapply(dt_parts, function(part) part[3]))
dtime <- unlist(lapply(dt_parts, function(part) part[4]))
# Call it 'dt_num'
t$dt_num <- as.numeric(chron(dates=paste(year,month,day,sep='-'), times=dtime, 
                             format=c('y-m-d', 'h:m:s')))

# They are most recent first, descending:
sum(diff(t$dt_num)>0)

# Create "is hate speech" label
t$is_hs <- t$label==0

# Run t test
t.test(retweets ~ is_hs, data=t, alternative="greater")
boxplot(retweets ~ is_hs, data=t)


# Start at January 1, 2013