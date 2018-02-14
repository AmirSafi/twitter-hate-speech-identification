# Natural Language Processing

# Set working directory 
setwd("/Users/amir/Dropbox/MSIM/INFX573/Course_Project/twitter-hate-speech-identification")

# Importing the dataset
dataset_original = read.csv('data/labeled_data.csv', sep = ",")

# Install needed packages if necessary
pkgs <- c('tm','SnowballC')
for (pkg in pkgs) {
        if(!(pkg %in% rownames(installed.packages()))) {
                install.packages(pkg)
        }
}
# Load the libraries 
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(dataset_original$tweet))
corpus = tm_map(corpus, content_transformer(tolower))

