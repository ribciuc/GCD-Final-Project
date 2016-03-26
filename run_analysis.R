library(tidyr)
library(dplyr)

# Download and unzip the data
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "data.zip", method = "curl")
unzip(zipfile="data.zip")

# Read into tables the Subject, Activity, and Features data for test and train subsets
ActivityTest  <- read.table("./UCI HAR Dataset/test/Y_test.txt" ,header = FALSE)
ActivityTrain  <- read.table("./UCI HAR Dataset/train/Y_train.txt" ,header = FALSE)

SubjectTest  <- read.table("./UCI HAR Dataset/test/subject_test.txt" ,header = FALSE)
SubjectTrain  <- read.table("./UCI HAR Dataset/train/subject_train.txt" ,header = FALSE)

FeaturesTest  <- read.table("./UCI HAR Dataset/test/X_test.txt" ,header = FALSE)
FeaturesTrain  <- read.table("./UCI HAR Dataset/train/X_train.txt" ,header = FALSE)

# Append the "train" and the "test" rows for corresponding tables
Subject <- rbind(SubjectTest, SubjectTrain)
Activity <- rbind(ActivityTest, ActivityTrain)
Features <- rbind(FeaturesTest, FeaturesTrain)

# Read the names of the "features" from features.txt and assign them to corresponding columns 
FeatureNames <- read.table("./UCI HAR Dataset/features.txt", header=FALSE)
names(Features) <- FeatureNames$V2

# Give appropriate names to the columns designating Activities and Subjects
names(Activity) <- c("Activities")
names(Subject) <- c("Subjects")

# Join the data columns regarding Subject, Activity, and Features
fulldata <- cbind(cbind(Subject,Activity),Features)

# Use full activity names from activity_labels.txt to replace number codes
ActivityNames <- read.table("./UCI HAR Dataset/activity_labels.txt", header=FALSE)
names(ActivityNames) <- c("Activities","Activity_Label")
fulldata <- left_join(fulldata,ActivityNames)
# Reorder columns, dropping the column w/ numeric activity codes
fulldata <- fulldata[,c(564,2,3:563)]

# Select only the subset of column related to mean() or std(), plus Activity and Subjects columns
meansdata <- fulldata[,grep("mean()|std()|Activity|Subjects", names(fulldata))]
# Drop columns containing Freq from the remainder
meansdata <- meansdata[,-grep("Freq", names(meansdata))]

# Tidy the data and calculate the means of each type of observation by grouping on Subject & Activity_Label
finaldata <- aggregate(. ~ Subjects + Activity_Label,meansdata,mean)

# Make the names of variables in finaldata a bit more explicit / readable
names(finaldata) <-gsub("^t", "time", names(finaldata))
names(finaldata) <-gsub("^f", "frequency", names(finaldata))
names(finaldata) <-gsub("Acc", "Accelerometer", names(finaldata))
names(finaldata) <-gsub("Gyro", "Gyroscope", names(finaldata))
names(finaldata) <-gsub("Mag", "Magnitude", names(finaldata))
names(finaldata) <-gsub("BodyBody", "Body", names(finaldata))

# Write out the file of tidydata in .txt format
write.table(finaldata, "tidydata.txt", row.names = FALSE)

### ==================================
# THIS OPTION WOULD MAKE THE DATA EVEN "TIDIER" BY GROUPING ALL MEASUREMENTS IN ONE COLUMN AND
# USING A FOURTH COLUMN TO SPECIFY THE THE TYPE OF MEASUREMENT INSTEAD OF 
# KEEPING 66 COLUMNS FOR 66 TYPES OF MEASUREMENTS
# # Make the data tidy by using gather, to recast 180x68 table into 11880x4 table with each of the 
# # 11880 = 30x6x66 combinations of Subject / Activity / type of Measurement showing one value for
# # that measurement
# tidydata <- gather (finaldata, Measurement, Value, -Subjects, - Activity_Label)
# 
# # Make the names of the type of Measurements a bit more explicit / readable
# tidydata$Measurement<-gsub("^t", "time", tidydata$Measurement)
# tidydata$Measurement<-gsub("^f", "frequency", tidydata$Measurement)
# tidydata$Measurement<-gsub("Acc", "Accelerometer", tidydata$Measurement)
# tidydata$Measurement<-gsub("Gyro", "Gyroscope", tidydata$Measurement)
# tidydata$Measurement<-gsub("Mag", "Magnitude", tidydata$Measurement)
# tidydata$Measurement<-gsub("BodyBody", "Body", tidydata$Measurement)
# 
# # Write out the file of tidydata in .txt format
# write.table(tidydata, "tidydata.txt", row.names = FALSE)