# this step sets the working directory for the script
setwd("~/Coursera Courses/Data Science/Getting and Cleaning Data/getting-and-cleaning-data-project")

# this step loads the necessary libraries
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(data.table)

# download and unzip the data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "./projectdata.zip", method = "curl")
unzip("projectdata.zip")

# this step creates activity labels and features dataframes
labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
labels[, 2] <- as.character(labels[, 2])
colnames(labels) <- c("Activity", "ActivityDescription")

features <- read.table("./UCI HAR Dataset/features.txt")
features[, 2] <- as.character(features[, 2])

##### Project Standard 2 #####

# this step filters the features dataframe to the target features:
# Mean and Standard Deviation
TgtFeatures <- grep(".*mean.*|.*std.*", features[, 2])
TgtFeatures.names <- features[TgtFeatures, 2]

##### Project Standard 4 #####

# this step cleans up the variable names 
TgtFeatures.names <- gsub("-mean", "Mean", TgtFeatures.names)
TgtFeatures.names <- gsub("-std", "StdDev", TgtFeatures.names)
TgtFeatures.names <- gsub("^t", "Time", TgtFeatures.names)
TgtFeatures.names <- gsub("^f", "Frequency", TgtFeatures.names)
TgtFeatures.names <- gsub("[-()]", "", TgtFeatures.names)

# this step loads the train and test datasets
train <- read.table("./UCI HAR Dataset/train/X_train.txt")[TgtFeatures]
trainAct <- read.table("./UCI HAR Dataset/train/Y_train.txt")
trainSubj <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubj, trainAct, train)

test <- read.table("./UCI HAR Dataset/test/X_test.txt")[TgtFeatures]
testAct <- read.table("./UCI HAR Dataset/test/Y_test.txt")
testSubj <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubj, testAct, test)

##### Project Standard 1 #####

# this step merges the datasets and adds column names to the variables
MergeData <- rbind(train, test)
colnames(MergeData) <- c("Subject", "Activity", TgtFeatures.names)

##### Project Standard 3 #####

# this step adds the descriptive activity name to the final data set
FinalData <- merge(MergeData, labels, by="Activity")
FinalData <- subset(FinalData, select = -c(1))
FinalData <- FinalData[, c(1, 81, 2:80)]
FinalData <- arrange(FinalData, Subject, ActivityDescription)

##### Project Standard 5 #####

# this step creates a tidy dataset that averages each variable
# for each activity and each subject

TidyDataFrame <- FinalData %>% group_by(ActivityDescription, Subject) %>%
        summarise_all(funs(mean))

##### Write Final DataFrame into .txt File #####

write.table(TidyDataFrame, "./TidyData.txt", row.names = FALSE, quote = FALSE)
