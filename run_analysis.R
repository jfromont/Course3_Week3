setwd("/Users/jfromont/Dropbox/R/CourserA/GitHub/Course3_Week3/")
getwd()
list.files()
library("dplyr")


# data extraction

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")

table(subject_test)
table(subject_train)


# naming variables

names(subject_test) <- "subject"
names(subject_train) <- "subject"
names(Y_test) <- "activity"
names(Y_train) <- "activity"

var_names <- read.table("UCI HAR Dataset/features.txt")
var_names <- as.character(var_names[,2])

names(X_test) <- var_names
names(X_train) <- var_names


# filter on measurements on the mean and standard deviation

filter_1 <- sort(c(grep("mean", var_names), grep("std", var_names)))

X_test_2 <- X_test[,filter_1]
X_train_2 <- X_train[,filter_1]

# renaming variables for X_test_2 and X_train_2

var_names_2 <- c("tBodyAccMeanX", "tBodyAccMeanY", "tBodyAccMeanZ"
                 ,"tBodyAccStdX", "tBodyAccStdY", "tBodyAccStdZ", "tGravityAccMeanX"
                 ,"tGravityAccMeanY", "tGravityAccMeanZ", "tGravityAccStdX"            
                 , "tGravityAccStdY", "tGravityAccStdZ", "tBodyAccJerkMeanX"
                 , "tBodyAccJerkMeanY", "tBodyAccJerkMeanZ", "tBodyAccJerkStdX"           
                 , "tBodyAccJerkStdY", "tBodyAccJerkStdZ", "tBodyGyroMeanX" 
                 , "tBodyGyroMeanY", "tBodyGyroMeanZ", "tBodyGyroStdX"              
                 , "tBodyGyroStdY", "tBodyGyroStdZ", "tBodyGyroJerkMeanX"
                 , "tBodyGyroJerkMeanY", "tBodyGyroJerkMeanZ", "tBodyGyroJerkStdX"          
                 , "tBodyGyroJerkStdY", "tBodyGyroJerkStdZ", "tBodyAccMagMean"
                 , "tBodyAccMagStd", "tGravityAccMagMean", "tGravityAccMagStd"           
                 , "tBodyAccJerkMagMean", "tBodyAccJerkMagStd", "tBodyGyroMagMean"
                 , "tBodyGyroMagStd", "tBodyGyroJerkMagMean", "tBodyGyroJerkMagStd"         
                 , "fBodyAccMeanX", "fBodyAccMeanY", "fBodyAccMeanZ"
                 , "fBodyAccStdX", "fBodyAccStdY", "fBodyAccStdZ"               
                 , "fBodyAccmeanFreqX", "fBodyAccmeanFreqY", "fBodyAccmeanFreqZ"
                 , "fBodyAccJerkMeanX", "fBodyAccJerkMeanY", "fBodyAccJerkMeanZ"          
                 , "fBodyAccJerkStdX", "fBodyAccJerkStdY", "fBodyAccJerkStdZ"
                 , "fBodyAccJerkmeanFreqX", "fBodyAccJerkmeanFreqY", "fBodyAccJerkmeanFreqZ"      
                 , "fBodyGyroMeanX", "fBodyGyroMeanY", "fBodyGyroMeanZ" 
                 , "fBodyGyroStdX", "fBodyGyroStdY", "fBodyGyroStdZ"              
                 , "fBodyGyromeanFreqX", "fBodyGyromeanFreqY", "fBodyGyromeanFreqZ"
                 , "fBodyAccMagMean", "fBodyAccMagStd", "fBodyAccMagmeanFreq"         
                 , "fBodyBodyAccJerkMagMean", "fBodyBodyAccJerkMagStd", "fBodyBodyAccJerkMagmeanFreq"
                 , "fBodyBodyGyroMagMean", "fBodyBodyGyroMagStd", "fBodyBodyGyroMagmeanFreq"    
                 , "fBodyBodyGyroJerkMagMean", "fBodyBodyGyroJerkMagStd", "fBodyBodyGyroJerkMagmeanFreq"
)

names(X_test_2) <- var_names_2
names(X_train_2) <- var_names_2


# combining dataset

test <- cbind(subject_test, Y_test, X_test_2)
train <- cbind(subject_train, Y_train, X_train_2)

DT1 <- rbind(test, train)


# add variable with name of activities

DT1 <- mutate(DT1, activity_name = ifelse(activity == 1, "WALKING",
                                    ifelse(activity == 2, "WALKING_UPSTAIRS",
                                    ifelse(activity == 3, "WALKING_DOWNSTAIRS",
                                    ifelse(activity == 4, "SITTING",
                                    ifelse(activity == 5, "STANDING",
                                    ifelse(activity == 6, "LAYING", "MISSING"
                                    ))))))
)

table(DT1$activity, DT1$activity_name)


# independent tidy data set with the average of each variable 
# for each activity and each subject

DT2 <- group_by(DT1, subject, activity_name) 
DT3 <- summarize(DT2, 
                 tBodyAccMeanX = mean(tBodyAccMeanX, na.rm=TRUE),
                 tBodyAccMeanY = mean(tBodyAccMeanY, na.rm=TRUE),
                 tBodyAccMeanZ = mean(tBodyAccMeanZ, na.rm=TRUE),
                 tBodyAccStdX = mean(tBodyAccStdX, na.rm=TRUE),
                 tBodyAccStdY = mean(tBodyAccStdY, na.rm=TRUE),
                 tBodyAccStdZ = mean(tBodyAccStdZ, na.rm=TRUE),
                 tGravityAccMeanX = mean(tGravityAccMeanX, na.rm=TRUE),
                 tGravityAccMeanY = mean(tGravityAccMeanY, na.rm=TRUE),
                 tGravityAccMeanZ = mean(tGravityAccMeanZ, na.rm=TRUE),
                 tGravityAccStdX = mean(tGravityAccStdX, na.rm=TRUE),
                 tGravityAccStdY = mean(tGravityAccStdY, na.rm=TRUE),
                 tGravityAccStdZ = mean(tGravityAccStdZ, na.rm=TRUE),
                 tBodyAccJerkMeanX = mean(tBodyAccJerkMeanX, na.rm=TRUE),
                 tBodyAccJerkMeanY = mean(tBodyAccJerkMeanY, na.rm=TRUE),
                 tBodyAccJerkMeanZ = mean(tBodyAccJerkMeanZ, na.rm=TRUE),
                 tBodyAccJerkStdX = mean(tBodyAccJerkStdX, na.rm=TRUE),
                 tBodyAccJerkStdY = mean(tBodyAccJerkStdY, na.rm=TRUE),
                 tBodyAccJerkStdZ = mean(tBodyAccJerkStdZ, na.rm=TRUE),
                 tBodyGyroMeanX = mean(tBodyGyroMeanX, na.rm=TRUE),
                 tBodyGyroMeanY = mean(tBodyGyroMeanY, na.rm=TRUE),
                 tBodyGyroMeanZ = mean(tBodyGyroMeanZ, na.rm=TRUE),
                 tBodyGyroStdX = mean(tBodyGyroStdX, na.rm=TRUE),
                 tBodyGyroStdY = mean(tBodyGyroStdY, na.rm=TRUE),
                 tBodyGyroStdZ = mean(tBodyGyroStdZ, na.rm=TRUE),
                 tBodyGyroJerkMeanX = mean(tBodyGyroJerkMeanX, na.rm=TRUE),
                 tBodyGyroJerkMeanY = mean(tBodyGyroJerkMeanY, na.rm=TRUE),
                 tBodyGyroJerkMeanZ = mean(tBodyGyroJerkMeanZ, na.rm=TRUE),
                 tBodyGyroJerkStdX = mean(tBodyGyroJerkStdX, na.rm=TRUE),
                 tBodyGyroJerkStdY = mean(tBodyGyroJerkStdY, na.rm=TRUE),
                 tBodyGyroJerkStdZ = mean(tBodyGyroJerkStdZ, na.rm=TRUE),
                 tBodyAccMagMean = mean(tBodyAccMagMean, na.rm=TRUE),
                 tBodyAccMagStd = mean(tBodyAccMagStd, na.rm=TRUE),
                 tGravityAccMagMean = mean(tGravityAccMagMean, na.rm=TRUE),
                 tGravityAccMagStd = mean(tGravityAccMagStd, na.rm=TRUE),
                 tBodyAccJerkMagMean = mean(tBodyAccJerkMagMean, na.rm=TRUE),
                 tBodyAccJerkMagStd = mean(tBodyAccJerkMagStd, na.rm=TRUE),
                 tBodyGyroMagMean = mean(tBodyGyroMagMean, na.rm=TRUE),
                 tBodyGyroMagStd = mean(tBodyGyroMagStd, na.rm=TRUE),
                 tBodyGyroJerkMagMean = mean(tBodyGyroJerkMagMean, na.rm=TRUE),
                 tBodyGyroJerkMagStd = mean(tBodyGyroJerkMagStd, na.rm=TRUE),
                 fBodyAccMeanX = mean(fBodyAccMeanX, na.rm=TRUE),
                 fBodyAccMeanY = mean(fBodyAccMeanY, na.rm=TRUE),
                 fBodyAccMeanZ = mean(fBodyAccMeanZ, na.rm=TRUE),
                 fBodyAccStdX = mean(fBodyAccStdX, na.rm=TRUE),
                 fBodyAccStdY = mean(fBodyAccStdY, na.rm=TRUE),
                 fBodyAccStdZ = mean(fBodyAccStdZ, na.rm=TRUE),
                 fBodyAccmeanFreqX = mean(fBodyAccmeanFreqX, na.rm=TRUE),
                 fBodyAccmeanFreqY = mean(fBodyAccmeanFreqY, na.rm=TRUE),
                 fBodyAccmeanFreqZ = mean(fBodyAccmeanFreqZ, na.rm=TRUE),
                 fBodyAccJerkMeanX = mean(fBodyAccJerkMeanX, na.rm=TRUE),
                 fBodyAccJerkMeanY = mean(fBodyAccJerkMeanY, na.rm=TRUE),
                 fBodyAccJerkMeanZ = mean(fBodyAccJerkMeanZ, na.rm=TRUE),
                 fBodyAccJerkStdX = mean(fBodyAccJerkStdX, na.rm=TRUE),
                 fBodyAccJerkStdY = mean(fBodyAccJerkStdY, na.rm=TRUE),
                 fBodyAccJerkStdZ = mean(fBodyAccJerkStdZ, na.rm=TRUE),
                 fBodyAccJerkmeanFreqX = mean(fBodyAccJerkmeanFreqX, na.rm=TRUE),
                 fBodyAccJerkmeanFreqY = mean(fBodyAccJerkmeanFreqY, na.rm=TRUE),
                 fBodyAccJerkmeanFreqZ = mean(fBodyAccJerkmeanFreqZ, na.rm=TRUE),
                 fBodyGyroMeanX = mean(fBodyGyroMeanX, na.rm=TRUE),
                 fBodyGyroMeanY = mean(fBodyGyroMeanY, na.rm=TRUE),
                 fBodyGyroMeanZ = mean(fBodyGyroMeanZ, na.rm=TRUE),
                 fBodyGyroStdX = mean(fBodyGyroStdX, na.rm=TRUE),
                 fBodyGyroStdY = mean(fBodyGyroStdY, na.rm=TRUE),
                 fBodyGyroStdZ = mean(fBodyGyroStdZ, na.rm=TRUE),
                 fBodyGyromeanFreqX = mean(fBodyGyromeanFreqX, na.rm=TRUE),
                 fBodyGyromeanFreqY = mean(fBodyGyromeanFreqY, na.rm=TRUE),
                 fBodyGyromeanFreqZ = mean(fBodyGyromeanFreqZ, na.rm=TRUE),
                 fBodyAccMagMean = mean(fBodyAccMagMean, na.rm=TRUE),
                 fBodyAccMagStd = mean(fBodyAccMagStd, na.rm=TRUE),
                 fBodyAccMagmeanFreq = mean(fBodyAccMagmeanFreq, na.rm=TRUE),
                 fBodyBodyAccJerkMagMean = mean(fBodyBodyAccJerkMagMean, na.rm=TRUE),
                 fBodyBodyAccJerkMagStd = mean(fBodyBodyAccJerkMagStd, na.rm=TRUE),
                 fBodyBodyAccJerkMagmeanFreq = mean(fBodyBodyAccJerkMagmeanFreq, na.rm=TRUE),
                 fBodyBodyGyroMagMean = mean(fBodyBodyGyroMagMean, na.rm=TRUE),
                 fBodyBodyGyroMagStd = mean(fBodyBodyGyroMagStd, na.rm=TRUE),
                 fBodyBodyGyroMagmeanFreq = mean(fBodyBodyGyroMagmeanFreq, na.rm=TRUE),
                 fBodyBodyGyroJerkMagMean = mean(fBodyBodyGyroJerkMagMean, na.rm=TRUE),
                 fBodyBodyGyroJerkMagStd = mean(fBodyBodyGyroJerkMagStd, na.rm=TRUE),
                 fBodyBodyGyroJerkMagmeanFreq = mean(fBodyBodyGyroJerkMagmeanFreq, na.rm=TRUE)
)

getwd()
write.table(DT3, "tidy_data_2.txt", row.name=FALSE)