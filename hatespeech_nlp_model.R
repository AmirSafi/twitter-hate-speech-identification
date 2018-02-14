# Natural Language Processing

# Set working directory 
setwd("/Users/amir/Dropbox/MSIM/INFX573/Course_Project/twitter-hate-speech-identification")

# Importing the dataset
dataset_original = read.csv('data/labeled_data.csv', sep = ",")

# Install needed packages if necessary
pkgs <- c('tm','SnowballC', 'caTools', 'randomForest', 'ROCR')
for (pkg in pkgs) {
        if(!(pkg %in% rownames(installed.packages()))) {
                install.packages(pkg)
        }
}
# Load the libraries 
library(tm)
library(SnowballC)
library(caTools)
library(randomForest)
library (ROCR)
corpus = VCorpus(VectorSource(dataset_original$tweet))
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
dataset$hate_speech = dataset_original$hate_speech
# Encoding the target feature as factor
dataset$hate_speech = factor(dataset$hate_speech, levels = c(0, 1, 2, 3))

# Splitting the dataset into the Training set and Test set
set.seed(1)
split = sample.split(dataset$hate_speech, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
numCol = ncol(training_set)
classifier = randomForest(x = training_set[-numCol],
                          y = training_set$hate_speech,
                          ntree = 10)
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
