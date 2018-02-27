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

# Import other tweets to fold into the matrix 
trump_tweet <- fromJSON("data/condensed_2018.json", flatten=TRUE)
colnames(trump_tweet)
rows_trump = nrow(trump_tweet)

# Joined dataset 
d1 = data.frame(dataset_original$tweet , stringsAsFactors = FALSE)
colnames(d1) = "tweet"
d2 = data.frame(trump_tweet$text, stringsAsFactors = FALSE)
colnames(d2) = "tweet"
all_tweets = rbind(d1,d2)
#d3


# Create a corpus
corpus = VCorpus(VectorSource(all_tweets))
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
original = dataset[1:rows_original,]

trump = dataset[rows_original:nrow(dataset),]

dataset$class = dataset_original$class
# Encoding the target feature as a factor
dataset$class = factor(dataset$class, levels = c(0, 1, 2))
# Target variable / true classification of the tweet 
response = dataset$class
# Remove the target variable from the training data
dataset = dataset[, !names(dataset) %in% "class"]

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

classifier = randomForest(x = dataset,
                          y = response,
                          ntree = 2)

print(difftime(Sys.time(), t1, units = 'mins'))

print(classifier)


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
