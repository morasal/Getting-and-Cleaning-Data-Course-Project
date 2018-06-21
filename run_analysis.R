### 0. Read data from a new created directory called "Project" including
# all of the assignment files.
setwd("./Project")
list.files()

activity_labels <- read.table("activity_labels.txt")
colnames(activity_labels) <- c("activity_id", "activity_label")
View(activity_labels)

features <- read.table("features.txt")
colnames(features) <- c("feature_id", "feature")
View(features)

X_train <- read.table("./train/X_train.txt")
dim(X_train) # dimensions = 7352 * 561

X_test <- read.table("./test/X_test.txt")
dim(X_test) # dimensions = 2947 * 561

y_train <- read.table("./train/y_train.txt")
dim(y_train) # dimensions = 7352 * 1

y_test <- read.table("./test/y_test.txt")
dim(y_test) # dimensions = 2947 * 1

subject_train <- read.table("./train/subject_train.txt")
dim(subject_train) # dimensions = 7352 * 1

subject_test <- read.table("./test/subject_test.txt")
dim(subject_test) # dimensions = 2947 * 1

### 1. Merge data sets 

## 1.1. Merging test and train data sets
subject <- rbind(subject_train,subject_test)
activity <- rbind(y_train, y_test)
records <- rbind(X_train, X_test)

## 1.2. Merging (column binding) subject, activity and records dataframes
# to form a merged dataframe called "merged_df" 
merged_df <- cbind(subject, activity, records)
dim(merged_df) # dimensions = 10299 * 563

## 1.3. Label the merged_df (using the "features" dataframe read in Step 0) 
features_vec <- as.vector(features[,2])
labels <- c("subject_id", "activity", features_vec)
colnames(merged_df) <- labels
View(merged_df)

### 2. Extract measurements on the mean and standard deviation of each measurement

## 2.1. Using the column names of the marged dataframe (created in Step 1.3)
# to find the records that include "mean" and "std" in their names while
# keeping the first two columns (i.e., "subject id" and "activity").

# NOTE: As I have assumed that only the columns with mean() and std()
# have been requested and not the ones including meanFreq(), first, I used 
# a temporary dataframe that contained either "subject" or
# "activity" or "std" or "mean" in its column names, and then excluded the 
# columns that contained "meanFreq".

temp_df <-  merged_df[, grepl("subject|activity|std|mean", names(merged_df))]
final_df <- temp_df[, !grepl("meanFreq", names(temp_df))]
dim(final_df) # dimensions = 10299 * 68 

### 3. Use descriptive activity names to activities in the data set

## 3.1. Store "final_df" in a new dataframe
new[] <- final_df

## 3.2. Create a lookup vector using the "activity_labels" dataframe where
# activity labels have been sorted from 1-6.
lookup_vector <- as.vector(activity_labels$activity_label)

## 3.2. Create a activity numbers vector using the "new" df's activity column
# that includes the number of activities.
activity_numbers <- as.vector(new$activity)

## 3.3. Index the lookup vector with the activity numbers vector and Store it 
# in the "activity" column of "new" dataframe.
new$activity <- lookup_vector[activity_numbers] 
View(new)

### 4. Label the data set with descriptive variable names.

## NOTE: This step has been done in Step 1. 3. and the current 
# dataframe version called "new" includes the variable names. Below, for sake
# of better readability, I have only substituted "t", "f", and "_" or "-" with
#"time", "freq", and ".", respectively. And also, removed "()". Check below:
colnames(new) <- lapply(colnames(new), function(x) {
  t1 <- sub("^f", "freq", x)
  t2 <- sub("^t", "time", t1)
  t3 <- gsub("_",".", t2)
  t4 <- gsub("-",".", t3)
  t5 <- gsub("\\(","", t4)
  t6 <- gsub("\\)","", t5)
})
View(new)

### 5. Create a second, independent tidy data set with mean of each variable 
#for each activity and each subject.

## 5.1. Created a list of 30 data frames (i.e., 1 for each subject), 
# where the data frame is formed by:
# 1. selecting the rows containing the records of one subject (called "temp_df1")
# 2. group "temp_df1" by activity (called "temp_df2")
# 3. provide the mean of all of the columns in temp_df2. 
library(dplyr)
final_list <- lapply(1:30, function (x) {
  temp_df1 <- filter(new, subject.id == x)  
  temp_df2 <- group_by(temp_df1, activity)
  summarise_all(temp_df2, mean)})

## 5.2. Below, I have converted the list created above to the requested data frame
# called "tidy_df". And, finally, I have witten the data frame as a txt file.
tidy_df <- do.call(rbind.data.frame, final_list)
dim(tidy_df) # dimensions = 180 * 68 (Note : 30 subjects * 6 activities = 180 rows)
View(tidy_df)

write.table(tidy_df, file = "./tidy.txt", row.names = FALSE, quote = FALSE)







