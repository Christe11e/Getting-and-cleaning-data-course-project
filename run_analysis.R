#Step 1
# Merge the training and the test sets to create one data set.
# set wd to coursera
setwd("~/Desktop/Coursera")
# load and inspect the data
subject_test<- read.table("./UCI HAR Dataset/test/subject_test.txt")
str(subject_test)
X_test<- read.table("./UCI HAR Dataset/test/X_test.txt")
str(X_test)
Y_test<- read.table("./UCI HAR Dataset/test/Y_test.txt")
str(Y_test)
subject_train<- read.table("./UCI HAR Dataset/train/subject_train.txt")
str(subject_train)
X_train<- read.table("./UCI HAR Dataset/train/X_train.txt")
str(X_train)
Y_train<- read.table("./UCI HAR Dataset/train/Y_train.txt")
str(Y_train)

# Create a single df from the test data sets and check it
testall <- cbind(X_test, subject_test, Y_test)
str(testall)

# Create a single df from the train data sets and check it
trainall <- cbind(X_train, subject_train, Y_train)
str(trainall)

# Join the two df
all <- rbind(testall, trainall)
str(all)


# Step 2
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# load the features table that gas variable names
features <- read.table("./UCI HAR Dataset/features.txt")
str(features)

# Extract variables that have mean and std in their name
mean <- grep("mean",features$V2)
length(mean)
std <- grep("std",features$V2)
length(std)
# Creat a new df containing the measurements on the mean and standard deviation for each measurement
# and the subject and activity columns.
# Store them in a new df subset. 
subset <- cbind(all[, c(mean, std)], all[, 562:563])
str(subset)


#Step3
# Uses descriptive activity names to name the activities in the data set
# Replace numbers by descriptive activity names in the activity column (the last one) 
subset$V1.1[subset$V1.1 == 1] <- "walking"
subset$V1.1[subset$V1.1 == 2] <- "walkingUpstairs"
subset$V1.1[subset$V1.1 == 3] <- "walkingDownstairs"
subset$V1.1[subset$V1.1 == 4] <- "sitting"
subset$V1.1[subset$V1.1 == 5] <- "standing"
subset$V1.1[subset$V1.1 == 6] <- "laying"

# Step 4.
# Appropriately labels the data set with descriptive variable names. 
# From features, create vectors containing the names containing mean or std
meannames <- features$V2[grep("mean",features$V2)]
head(meannames)

stdnames <- features$V2[grep("std",features$V2)]
head(stdnames, 10)

# Modify the names of meannames to make them more descriptive
meannames <- sub("tBody","timeBody",meannames)
meannames
meannames <- sub("tGravity","timeGravity",meannames)
meannames <- sub("f","frequency",meannames)
meannames <- sub("Acc", "Acceleration", meannames)
meannames <- sub("BodyBody","Body",meannames)
meannames <- sub("()","",meannames, fixed = TRUE)
meannames <- sub("Mag","Magnitude",meannames, fixed = TRUE)
meannames <- sub("Gyro","Gyroscope",meannames)
meannames <- sub("mean","Mean",meannames)
meannames <- gsub("-","",meannames)
tail(meannames)

# Do the same for the stdnames vector
stdnames <- sub("tBody","timeBody",stdnames)
stdnames <- sub("tGravity","timeGravity",stdnames)
stdnames <- sub("f","frequency",stdnames)
stdnames <- sub("Acc", "Acceleration", stdnames)
stdnames <- sub("BodyBody","Body",stdnames)
stdnames <- gsub("()","",stdnames, fixed = TRUE) 
stdnames <- sub("Mag","Magnitude",stdnames, fixed = TRUE)
stdnames <- sub("Gyro","Gyroscope",stdnames)
stdnames <- sub("std","Std",stdnames)
stdnames <- gsub("-","",stdnames)

# Rename the all columns of the subset df using the meannames, stdnames vectors 
# for column 1:79 and subject and activity for columns 80:81
colnames(subset) <- c(meannames, stdnames, "subject", "activity")


# Step 5.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Let's use dplyr
library(dplyr)
# Load subset df into a "datafram tbl"
subset2 <- tbl_df(subset)
# group the data by subject and activity
by_subject_activity <- group_by(subset2, subject, activity)
# Calulate average of each variable by subject and activity. Use summarize_each() from dplyr
tidy_data <- summarise_each(by_subject_activity, funs(mean))
# Check the tidy_data df
tidy_data 
# Save the tidy_data as text file
write.table(tidy_data, file = "/Users/christellelenain/Desktop/Coursera/UCI HAR Dataset/tidy_data.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)
# read the tidy_data txt file
tidy_data <-read.table("/Users/christellelenain/Desktop/Coursera/UCI HAR Dataset/tidy_data.txt", as.is = TRUE, sep = "\t", header = TRUE, quote = "")
