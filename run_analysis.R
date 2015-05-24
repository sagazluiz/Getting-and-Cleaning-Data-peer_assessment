# Reading the files

Xtrain <- read.table("./Dataset/UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("./Dataset/UCI HAR Dataset/train/y_train.txt")

trainSubject <- read.table("./Dataset/UCI HAR Dataset/train/subject_train.txt")

Xtest <- read.table("./Dataset/UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./Dataset/UCI HAR Dataset/test/y_test.txt") 

testSubject <- read.table("./Dataset/UCI HAR Dataset/test/subject_test.txt")

# Step1. Merges the training and the test sets to create one data set.

XData <- rbind(Xtrain, Xtest)
yData <- rbind(ytrain, ytest)
bindSubject <- rbind(trainSubject, testSubject)


# Step2. Extracts only the measurements on the mean and standard deviation for each measurement.  
features <- read.table("./Dataset/UCI HAR Dataset/features.txt")
Mean_Std <- grep("mean\\(\\)|std\\(\\)", features[, 2])
XData <- XData[, Mean_Std]

names(XData) <- gsub("\\(\\)", "", features[Mean_Std, 2]) # remove "()"
names(XData) <- gsub("mean", "Mean", names(XData)) # capitalize M
names(XData) <- gsub("std", "Std", names(XData)) # capitalize S
names(XData) <- gsub("-", "", names(XData)) # remove "-" in column names 

# Step3. Uses descriptive activity names to name the activities in the data set.

activity <- read.table("./Dataset/UCI HAR Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[yData[, 1], 2]
yData[, 1] <- activityLabel
names(yData) <- "activity"

# Step4. Appropriately labels the data set with descriptive variable names. 
names(bindSubject) <- "subject"
cleanedData <- cbind(bindSubject, yData, XData)
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset

# Step5. From the data set in step 4, creates a second, independent tidy data
## set with the average of each variable for each activity and each subject.

subjectBind <- length(table(bindSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectBind*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectBind) {
    for(j in 1:activityLen) {
        result[row, 1] <- sort(unique(bindSubject)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        bool1 <- i == cleanedData$subject
        bool2 <- activity[j, 2] == cleanedData$activity
        result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
        row <- row + 1
    }
}
write.table(result, "data_means_and_std.txt") # write out the 2nd dataset

# data <- read.table("./data_means_and_std.txt") 
