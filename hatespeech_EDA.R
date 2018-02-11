"
INFX 573 Course Project 
Hate speech exploratory analysis 
Data : Tweets classified as hate speech, offensive language, or neither
Source : https://data.world/crowdflower/hate-speech-identification
"


# Set working directory
#setwd("C:\\Users\\Richard\\Desktop\\MSIM Coursework\\Winter 2018\\INFX 573\\Research\\crowdflower-hate-speech-identification\\crowdflower-hate-speech-identification\\data")
setwd("~/Dropbox/MSIM/INFX573/INFX573CourseProject")

# Load the dataset 
data <- read.csv('data/twitter_hate_speech_classifier_dfe_a845520.csv')
dataL <- read.csv('data/labeled_data.csv')
# Explore the variables 
colnames(data)
nrow(data)



list.files()
tt <- read.csv(list.files(), header=T)

text <- tt$tweet_text
text[grep('trump', tolower(text))][1:5]



