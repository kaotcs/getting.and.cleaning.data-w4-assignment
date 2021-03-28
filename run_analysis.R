library(dplyr)
library(data.table)
#readdata
dataPath <- "UCI HAR Dataset"

# read training data
t_Subjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
t_x <- read.table(file.path(dataPath, "train", "X_train.txt"))
t_y <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data
ts_Subjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
ts_x <- read.table(file.path(dataPath, "test", "X_test.txt"))
ts_y <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# read activity labels
act <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(act) <- c("activityId", "activityLabel")

#merging the test data and traing data

# concatenate individual data tables to make single data table
humanActivity <- rbind(
  cbind(t_Subjects, t_x, t_y),
  cbind(ts_Subjects, ts_x, ts_y)
)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

# Extracting only the measurements on the mean and standard deviation

# searching columns to keep based on column name...
columnstokeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# rearranging data to columstokeep
humanActivity <- humanActivity[, columnstokeep]

# adjusting activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity,levels = act[, 1], labels = act[, 2])


# getting humanActivity Colums (HAC)
HAC <- colnames(humanActivity)

# remove special characters
HAC <- gsub("[\\(\\)-]", "", HAC)

# tidy the data
HAC <- gsub("^f", "frequencyDom", HAC)
HAC <- gsub("^t", "timeDom", HAC)
HAC <- gsub("Acc", "Accelerometer", HAC)
HAC <- gsub("Gyro", "Gyroscope", HAC)
HAC <- gsub("Mag", "Magnitude", HAC)
HAC <- gsub("Freq", "Frequency", HAC)
HAC <- gsub("mean", "Mean", HAC)
HAC <- gsub("std", "StandardDeviation", HAC)
HAC <- gsub("BodyBody", "Body", HAC)

#renaming with tidy data
colnames(humanActivity) <- HAC

# group by subject and activity and summarise using mean
HAM <- humanActivity %>% group_by(subject, activity) %>% summarize_all(funs(mean))

# output to file "tidy_data.txt"
write.table(HAM, "tidy_data.txt", row.names = FALSE, quote = FALSE)
