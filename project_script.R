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

##### Project Standard 1 #####

# This section merges the training and the test sets to create one data set

# this step creates activity labels and features dataframes
labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
labels[, 2] <- as.character(labels[, 2])
colnames(labels) <- c("activity", "activityDescription")

features <- read.table("./UCI HAR Dataset/features.txt")
features[, 2] <- as.character(features[, 2])

# this step loads the train and test datasets
train <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainAct <- read.table("./UCI HAR Dataset/train/Y_train.txt")
trainSubj <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train <- cbind(trainSubj, trainAct, train)

test <- read.table("./UCI HAR Dataset/test/X_test.txt")
testAct <- read.table("./UCI HAR Dataset/test/Y_test.txt")
testSubj <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test <- cbind(testSubj, testAct, test)

# this step merges the datasets and adds column names to the variables
mergeData <- rbind(train, test)

##### Project Standard 2 #####

# This section extracts only the measurements on the mean and standard deviation
# for each measurement, then subsets the merged dataset on those variables

varNames <- c("subject", "activity", as.character(features[, 2]))
TgtVars <- grep("subject|activity|.*mean.*|.*std.*", varNames, value = FALSE, ignore.case = TRUE)
TgtData <- mergeData[, TgtVars]

##### Project Standard 3 #####

# This section applies descriptive activity names to the activities in the data set
TgtData[, 2] <- labels[, 2][TgtData$V1.1]

##### Project Standard 4 #####

# This section appropriately labels the data set with descriptive variable names

# this step cleans up the variable names and applies them to the Target Data set
TgtVarNames <- varNames[Tgt]
TgtVarNames <- gsub("-mean", "Mean", TgtVarNames)
TgtVarNames <- gsub("-std", "StdDev", TgtVarNames)
TgtVarNames <- gsub("^t", "Time", TgtVarNames)
TgtVarNames <- gsub("^f", "Frequency", TgtVarNames)
TgtVarNames <- gsub("^anglet", "AngleTime", TgtVarNames)
TgtVarNames <- gsub("[-()]", "", TgtVarNames)
names(TgtData) <- TgtVarNames

##### Project Standard 5 #####

# This section creates a second, independent data set with the average of each
# variable for each activity and each subject

TidyDataFrame <- TgtData %>% group_by(activity, subject) %>%
        summarise_all(funs(mean))

##### Write Final DataFrame into .txt File #####

write.table(TidyDataFrame, "./Tidy.Data.txt", row.names = FALSE, quote = FALSE)
