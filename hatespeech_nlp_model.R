# Natural Language Processing

# Set working directory 
setwd("/Users/amir/Dropbox/MSIM/INFX573/Course_Project/twitter-hate-speech-identification")
# richard
setwd("C:/Users/Richard/Desktop/MSIM Coursework/Winter 2018/INFX 573/Final Project/twitter-hate-speech-identification")

# Install needed packages if necessary
pkgs <- c('jsonlite','tm','SnowballC', 'caTools', 'randomForest','glmnet', 'nnet','ROCR', 'rpart',
          'stringr', 'chron')
for (pkg in pkgs) {
        if(!(pkg %in% rownames(installed.packages()))) {
                install.packages(pkg)
        }
}
# Load the libraries 
library(jsonlite)
library(chron) # datetimes
library(tm)
library(SnowballC)
library(caTools)
library(randomForest)
# Elastic net model paths for some generalized linear models
library(glmnet)
# Multinomial regression 
library(nnet)
library (ROCR)
# Library for SVM classifier
library(e1071)
# Classification Tree with rpart
library(rpart)

# Importing the dataset
dataset_original = read.csv('data/labeled_data.csv', sep = ",")
colnames(dataset_original)
rows_original = nrow(dataset_original)


# Import trump tweets to fold into the matrix 
keeps <- c("tweet", "created_at", "retweet_count" , "followers_count")
trump_tweet_2017 <- fromJSON("data/master_2017.json", flatten=TRUE)
n = colnames(trump_tweet_2017)
colnames(trump_tweet_2017)[n == 'full_text'] = 'tweet'
colnames(trump_tweet_2017)[n == 'user.followers_count'] = 'followers_count'
trump_tweet_2017 = trump_tweet_2017[,keeps]

trump_tweet_2016 <- fromJSON("data/master_2016.json", flatten=TRUE)
n = colnames(trump_tweet_2016)
colnames(trump_tweet_2016)[n == 'text'] = 'tweet'
colnames(trump_tweet_2016)[n == 'user.followers_count'] = 'followers_count'
trump_tweet_2016 = trump_tweet_2016[,keeps]

trump_tweet_2015 <- fromJSON("data/master_2015.json", flatten=TRUE)
n = colnames(trump_tweet_2015)
colnames(trump_tweet_2015)[n == 'text'] = 'tweet'
colnames(trump_tweet_2015)[n == 'user.followers_count'] = 'followers_count'
trump_tweet_2015 = trump_tweet_2015[,keeps]

trump_tweet_2014 <- fromJSON("data/master_2014.json", flatten=TRUE)
n = colnames(trump_tweet_2014)
colnames(trump_tweet_2014)[n == 'text'] = 'tweet'
colnames(trump_tweet_2014)[n == 'user.followers_count'] = 'followers_count'
trump_tweet_2014 = trump_tweet_2014[,keeps]

trump_tweet_2013 <- fromJSON("data/master_2013.json", flatten=TRUE)
n = colnames(trump_tweet_2013)
colnames(trump_tweet_2013)[n == 'text'] = 'tweet'
colnames(trump_tweet_2013)[n == 'user.followers_count'] = 'followers_count'
trump_tweet_2013 = trump_tweet_2013[,keeps]

trump_tweet_2012 <- fromJSON("data/master_2012.json", flatten=TRUE)
n = colnames(trump_tweet_2012)
colnames(trump_tweet_2012)[n == 'text'] = 'tweet'
colnames(trump_tweet_2012)[n == 'user.followers_count'] = 'followers_count'
trump_tweet_2012 = trump_tweet_2012[,keeps]

trump_tweet_2011 <- fromJSON("data/master_2011.json", flatten=TRUE)
n = colnames(trump_tweet_2011)
colnames(trump_tweet_2011)[n == 'text'] = 'tweet'
colnames(trump_tweet_2011)[n == 'user.followers_count'] = 'followers_count'
trump_tweet_2011 = trump_tweet_2011[,keeps]

trump_tweet_2010 <- fromJSON("data/master_2011.json", flatten=TRUE)
n = colnames(trump_tweet_2010)
colnames(trump_tweet_2010)[n == 'text'] = 'tweet'
colnames(trump_tweet_2010)[n == 'user.followers_count'] = 'followers_count'
trump_tweet_2010 = trump_tweet_2010[,keeps]

trump_tweet_2009 <- fromJSON("data/master_2009.json", flatten=TRUE)
n = colnames(trump_tweet_2009)
colnames(trump_tweet_2009)[n == 'text'] = 'tweet'
colnames(trump_tweet_2009)[n == 'user.followers_count'] = 'followers_count'
trump_tweet_2009 = trump_tweet_2009[,keeps]

trump_tweet = rbind(trump_tweet_2017,trump_tweet_2016,trump_tweet_2015,trump_tweet_2014
                    ,trump_tweet_2013,trump_tweet_2012,trump_tweet_2009)

# "created_at"  "retweet_count" "followers_count" "tweet"



                     

colnames(trump_tweet)
rows_trump = nrow(trump_tweet)

# Import random tweets fold into the matrix 
# feb 17-24 2018
recent = read.csv("random tweets.csv")
colnames(recent)
rows_recent = nrow(recent)

# Joined dataset 
d1 = data.frame((as.factor(dataset_original$tweet)))
colnames(d1) = "tweet"
d2 = data.frame((as.factor(trump_tweet$tweet)))
colnames(d2) = "tweet"
d3 = data.frame(as.factor(recent$Text))
colnames(d3) = "tweet"

all_tweets = rbind(d1,d2,d3)

# Create a corpus
library(stringr)
all_tweets$tweet <- str_replace_all(all_tweets$tweet,"([^A-Za-z0-9 ])+", " ")
corpus = VCorpus(VectorSource(all_tweets$tweet))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
dataset = as.data.frame(as.matrix(dtm))

# Split original labeled and new tweets
training = dataset[1:rows_original,]
trump.dtm = dataset[(rows_original+1):(rows_original+rows_trump),]
recent.dtm = dataset[(rows_original+rows_trump +1):(rows_original+rows_trump+rows_recent),]

# Encoding the target feature as a factor
response = dataset_original$class
response = factor(dataset_original$class, levels = c(0, 1, 2))
# Target variable / true classification of the tweet 

# Splitting the dataset into the Training set and Test set
set.seed(1)
#split = sample.split(dataset$class, SplitRatio = 0.8)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)
#numCol = ncol(training_set)


# Fitting Several different classifiers on the training set
t1 = Sys.time()

# Random forest classifier 
classifier = randomForest(x = training,
                          y = response,
                          ntree = 1)
# Decision tree classifier 
tree_clf = rpart(response ~ . , method="class", data=training)

# Linear SVM 
svm_clf = svm(response ~ ., training)

# Logistic regression model 
logit_clf = multinom(response ~.,training)


print(difftime(Sys.time(), t1, units = 'mins'))
print(classifier)

# Predict trumps tweet 
trump_pred = predict(classifier, newdata = trump.dtm)
recent_pred = predict(classifier, newdata = recent.dtm)
trump_tweet$class = trump_pred
recent$class = recent_pred

# Verify rows match input trump tweet data
sum(diff(as.numeric(names(trump_pred)))!=1) # good!

as.data.frame(lapply(table(trump_tweet$class), function(x) x/nrow(trump_tweet)))



## Make datetime field
mo <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
months <- c(paste('0', 1:9, sep=''),'10','11','12')

dt_parts <- sapply(trump_tweet$created_at, function(row) strsplit(row, ' '))
year  <- unlist(lapply(dt_parts, function(part) part[6]))
month <- unlist(lapply(dt_parts, function(part) months[mo==part[2]]))
day   <- unlist(lapply(dt_parts, function(part) part[3]))
dtime <- unlist(lapply(dt_parts, function(part) part[4]))
# Call it 'dt_num'
trump_tweet$dt_num <- as.numeric(chron(dates=paste(year,month,day,sep='-'), times=dtime, 
                             format=c('y-m-d', 'h:m:s')))

# Can call as.chron(dt_num) to get date-looking things back
as.chron(17000) # use as.numeric to return to fractional days since 1970 Jan 1

# Plot # followers against time
tt <- trump_tweet
tt <- trump_tweet[seq(0,nrow(trump_tweet),by=10),]
with(subset(tt, dt_num>16500), plot(dt_num, followers_count))
# Mark dates
abline(v=as.numeric(chron('11/08/16')), col='purple', lwd=2) # Election
abline(v=as.numeric(chron('06/16/15')), col='blue', lwd=2) # Presidential Bid
abline(v=as.numeric(chron('05/03/16')), col='red', lwd=2) # Became presumptive nominee

with(tt, plot(dt_num, retweet_count, add=T))


# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-numCol])
# Making the Confusion Matrix
cm = table(test_set[, numCol], y_pred)
#
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n 
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1) 
