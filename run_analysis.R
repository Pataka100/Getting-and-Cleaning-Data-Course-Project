library(dplyr)
library(plyr)
library(stringr)
path <- "C:/Users/VPataka/Desktop/Coursera/Assignment 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset" #Specify Path where data sits
setwd(path)


#Training Data
X_train=read.table("./train/X_train.txt",colClasses="numeric") # Read in X train data
y_train=read.table("./train/y_train.txt") # Read in y train data
subject_train=read.table("./train/subject_train.txt") # Read in y subject train data

#Testing Data
X_test=read.table("./test/X_test.txt",colClasses="numeric") # Read in X test data
y_test=read.table("./test/y_test.txt") # Read in y test data
subject_test=read.table("./test/subject_test.txt") # Read in y test data

#Read in features or descriptive variables
features=read.table("./features.txt") 
Activity_type=read.table("./activity_labels.txt") 


#Rename Variables
y_train <- dplyr::rename(y_train,Activity=V1) # Rename Activity Names, explicitly specified the package name to avoid conflict
y_test <- dplyr::rename(y_test,Activity=V1) # Rename Activity Names, explicitly specified the package name to avoid conflict
subject_test <- dplyr::rename(subject_test,Subject=V1) # Rename variable V1 to Subject for test data
subject_train <- dplyr::rename(subject_train,Subject=V1) # Rename variable V1 to Subject for train data

# Appropriately labels the data set with descriptive variable names using give features info

#Assign appropriate names to the test and train dataset
colnames(X_test)<-c(features[,2])
colnames(X_train)<-c(features[,2])

#Merge Training Data and Testing Data
train_data <- cbind(y_train,subject_train, X_train) 
test_data <- cbind(y_test,subject_test,X_test) 

# 1 Merge Training and Testing to Create One full dataset
All_Data <- rbind(train_data,test_data)


# 2 Extracts only the measurements on the mean and standard deviation for each measurement.

Extract_mean_sd_names_index<-grep("mean\\(\\)|std\\(\\)",features$V2) #returns location of where it appears

Extract_mean_sd_names <- features[Extract_mean_sd_names_index,2]

Sub_Data <- subset(All_Data,select=c("Subject","Activity_Type",Extract_mean_sd))

# 3 Uses descriptive activity names to name the activities in the data set
Sub_Data$Activity_Type  <-Activity_type[Sub_Data$Activity_Type,2]


#4 Appropriately labels the data set with descriptive variable names. 
names(Sub_Data)<-gsub("^t", "time", names(Sub_Data))
names(Sub_Data)<-gsub("^f", "frequency", names(Sub_Data))
names(Sub_Data)<-gsub("BodyBody", "Body", names(Sub_Data))
names(Sub_Data)<-gsub("Mag", "Magnitude", names(Sub_Data))
names(Sub_Data)<-gsub("Gyro", "Gyroscope", names(Sub_Data))
names(Sub_Data)<-gsub("Acc", "Accelerometer", names(Sub_Data))


#5 creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_dataset <- aggregate(.~Subject + Activity_Type, Sub_Data, mean)

tidy_dataset3 <- aggregate(Sub_Data, list(Sub_Data$Subject, Sub_Data$Activity_Type), mean) #Extract mean

Extract_mean <- aggregate(test_data, list(test_data$Activity_Type), mean) #Extract mean
Extract_mean <- subset(Extract_mean,select=-(Group.1)) #Exclude the Group by variable which was automatically created

#
Extract_sd <- aggregate(test_data, list(test_data$Activity_Type), sd)
Extract_sd$Activity_Type=Extract_sd$Group.1 # Assign group values to Activity Type
Extract_sd <- subset(Extract_sd,select=-(Group.1)) #Exclude the Group by variable which was automatically created

write.table(tidy_dataset, file = "tidy_dataset.csv", row.names = FALSE)

