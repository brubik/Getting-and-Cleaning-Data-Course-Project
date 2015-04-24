
# My system: Windows 8
# My R version: 3.1.2 (2014-10-31) -- "Pumpkin Helmet"


# To be able to use this code, you should put the folder "UCI HAR Dataset" with 
# Samsung Data into your working directory
# Folder "UCI HAR Dataset" is available here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 


### reading training data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", quote="\"")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", quote="\"")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", quote="\"")


### reading test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", quote="\"")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", quote="\"")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", quote="\"")


### merging data to one dataset
all_data <- cbind(rbind(subject_train, subject_test), 
              rbind(y_train, y_test),
              rbind(X_train, X_test))

# removing redundant datasets
rm(subject_train, y_train, X_train, subject_test, y_test, X_test)


### labeling the data set with descriptive variable names
colnames(all_data)[1:2] <- c("subject", "activity")
features <- read.table("UCI HAR Dataset/features.txt", quote="\"")
colnames(all_data)[3:dim(all_data)[2]] <- as.character(features[,2])
rm(features)


### extracting the measurements on the mean and standard deviation only:

# indices with appropriate measurements
indices <- grepl("mean()",colnames(all_data), fixed = TRUE) | 
      grepl("std()",colnames(all_data), fixed = TRUE)
# dataset should contain subject and activity variables
indices[1:2] <- TRUE
# new dataset called data with variables: subject, activity, means and
# standard deviations:
data <- all_data[,indices]
# removing redundant datasets
rm(all_data, indices)


### using descriptive activity names to name the activities in the data set
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", quote="\"")
act <- rep("", nrow(data))
for (i in 1:nrow(data)) act[i] <- as.character(activity_labels[data[i,2],2])
data[,2] <- act
rm(activity_labels,act,i)


### creating a second, independent tidy data set with the average of each variable
### for each activity and each subject

# initializing rows and columns of dataset with averages
data_ave <- data.frame(subject = rep(levels(factor(data[,1])), 
                                     times=length(levels(factor(data[,2])))),
                       activity = rep(levels(factor(data[,2])),
                                      each=length(levels(factor(data[,1])))))
data_ave <- cbind(data_ave, matrix(0, nrow(data_ave), ncol(data)-2))
colnames(data_ave)[3:ncol(data_ave)] <- colnames(data)[3:ncol(data)]

# counting averages of variables for activities and subjects
for(i in 1:nrow(data_ave)){
      for(j in 3:ncol(data_ave)){
            data_ave[i,j] <- mean(data[data[,1]==data_ave[i,1] & data[,2]==data_ave[i,2],j])
      }
}
rm(i,j)


### exporting final dataset with averages of measurements on the mean and 
### standard deviation for each activity and each subject to .txt
write.table(data_ave, "data_ave.txt", row.names=FALSE)

# Your current workspace contains also the dataset with all observations of 
# measurements on the mean and standard deviation. It can be exported too:
# write.table(data, "data.txt", row.names=FALSE)

