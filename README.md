# getting.and.cleaning.data-w4-assignment

This repository contain my submission for the assignment of week 4 Getting and Cleaning Data Coursera course.

# Data description

The variables in the data X are sensor signals measured with waist-mounted smartphone from 30 subjects. The variable in the data Y indicates activity type the subjects performed during recording.

# Code explaination

The code have combined training dataset and test dataset, and extracted partial variables to create a new dataset with the averages of each variable for each activity.

# New dataset

The new dataset contain variables calculated on the mean and standard deviation. Each row of the dataset is an average of each activity type for all subjects.

The code was written based on the instruction of the assignment

The R script called run_analysis.R does the following, as requested by the assignment:

1.Merges the training and the test sets to create one data set.

2.Extracts only the measurements on the mean and standard deviation for each measurement.

3.Uses descriptive activity names to name the activities in the data set

4.Appropriately labels the data set with descriptive variable names.

5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
