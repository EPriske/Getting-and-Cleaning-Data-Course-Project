#Loading required libraries for cleaning and managing the data
library(dplyr)

#Downloading the data. First we check if we have the file -
#-already in our working directory. If not we download it.

if(!file.exists("UCI HAR Dataset")){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, "UCI HAR Dataset")
    unzip("UCI HAR Dataset") 
    features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
    activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
    subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
    x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
    y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
    subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
    x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
    y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
    }
  
if(file.exists("UCI HAR Dataset")){
    features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
    activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
    subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
    x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
    y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
    subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
    x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
    y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
    }

#1) Merges the training and the test sets to create one data set.
#Merged from subject, y, x. In that respective order the M stands for Merged.
MSubject <- rbind(subject_train, subject_test)
MY <- rbind(y_train, y_test)
MX <- rbind(x_train, x_test)
MData <- cbind(MSubject, MY, MX)

#2) Extracts only the measurements on the mean and standard deviation for each measurement.
# We use the select() function together with regular expressions.
MData2 <- select(MData, subject, code, contains("mean"), contains("std"))

#3) Uses descriptive activity names to name the activities in the data set
# Here we merge the data from activities to change the numbers to their respective activitie
MData3 <- MData2
MData3$code <- activities[MData2$code, 2]


#4) Appropriately labels the data set with descriptive variable names. 
# Here we use the function gsub() together with Regular expressions
# To find out the labels in the columns and rename them with Descriptive 
# Variable names
MData4 <- MData3
names(MData4)[2] = "activity"
names(MData4)<-gsub("Acc", "Accelerometer", names(MData4))
names(MData4)<-gsub("Gyro", "Gyroscope", names(MData4))
names(MData4)<-gsub("BodyBody", "Body", names(MData4))
names(MData4)<-gsub("Mag", "Magnitude", names(MData4))
names(MData4)<-gsub("^t", "Time", names(MData4))
names(MData4)<-gsub("^f", "Frequency", names(MData4))
names(MData4)<-gsub("tBody", "TimeBody", names(MData4))
names(MData4)<-gsub("-mean()", "Mean", names(MData4), ignore.case = TRUE)
names(MData4)<-gsub("-std()", "STD", names(MData4), ignore.case = TRUE)
names(MData4)<-gsub("-freq()", "Frequency", names(MData4), ignore.case = TRUE)
names(MData4)<-gsub("angle", "Angle", names(MData4))
names(MData4)<-gsub("gravity", "Gravity", names(MData4))

#5) From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.
#   Here we use the functions group_by() and summarize_all().
MData5 <- MData4
MData5 <- group_by(MData5, subject, activity)
MData5 <- summarize_all(MData5, funs(mean))
write.table(MData5,"TidyData.txt", row.names=F)
str(MData5)






