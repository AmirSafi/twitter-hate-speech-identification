# Natural Language Processing

# Set working directory 
setwd("/Users/amir/Dropbox/MSIM/INFX573/Course_Project/twitter-hate-speech-identification")

# Install needed packages if necessary
pkgs <- c('jsonlite','tm','SnowballC', 'caTools', 'randomForest','glmnet', 'nnet','ROCR')
for (pkg in pkgs) {
        if(!(pkg %in% rownames(installed.packages()))) {
                install.packages(pkg)
        }
}
# Load the libraries 
library(jsonlite)
library(tm)
library(SnowballC)
library(caTools)
library(randomForest)
# Elastic net model paths for some generalized linear models
library(glmnet)
# Multinomial regression 
library("nnet")
library (ROCR)

# Importing the dataset
dataset_original = read.csv('data/labeled_data.csv', sep = ",")
colnames(dataset_original)
rows_original = nrow(dataset_original)

# Import trump tweets to fold into the matrix 
trump_tweet <- fromJSON("data/condensed_2018.json", flatten=TRUE)
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
d2 = data.frame((as.factor(trump_tweet$text)))
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
#classifier = randomForest(x = training_set[-numCol],
#                          y = training_set$class,
#                          ntree = 10)
classifier = randomForest(x = training,
                          y = response,
                          ntree = 1)
print(difftime(Sys.time(), t1, units = 'mins'))
print(classifier)


# Predict trumps tweet 
trump_pred = predict(classifier, newdata = trump.dtm)
recent_pred = predict(classifier, newdata = recent.dtm)
trump_tweet$class = trump_pred
recent$class = recent_pred


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
