## Coursera's Getting and Cleaning Data Programming Assessment

## Code based in a RPubs (www.rpubs.com) solution 

## Creates data tables from the .txt files
features <- read.table("./features.txt", col.names = c("n","functions"))
activitY_labels <- read.table("./activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("./test/subject_test.txt", col.names = "subject")
x_test <- read.table("./test/X_test.txt", col.names = features$functions)
y_test <- read.table("./test/y_test.txt", col.names = "code")
subject_train <- read.table("./train/subject_train.txt", col.names = "subject")
x_train <- read.table("./train/X_train.txt", col.names = features$functions)
y_train <- read.table("./train/y_train.txt", col.names = "code")


## Merges each pair (train and test) of datasets by rows
x_data <- rbind(x_train, x_test)
y_data <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)


## Merges the 3 pairs of datasets by column
merged_data <- cbind(subject, y_data, x_data)

## Extracts only the mean and standard deviation measurements
TidyData <- merged_data %>% select(subject, code, contains("mean"), contains("std"))

## Replaces each activity code by its name
TidyData$code <- activity_labels[TidyData$code, "activity"]


## Creates a function to rename the dataset columns in a more descriptive way
## Uses gsub 
renameTidyData <- function(previous_text, new_text, ignore_cases = FALSE)
{
  names(TidyData) <- gsub(previous_text, new_text, names(TidyData), ignore.case = ignore_cases)
}

names(TidyData)[2] = "activity"

## Calls the function to rename each abbreviation
renameTidyData("Acc", "Accelerometer")
renameTidyData("Gyro", "Gyroscope")
renameTidyData("BodyBody", "Body")
renameTidyData("Mag", "Magnitude")
renameTidyData("^t", "Time")
renameTidyData("^f", "Frequency")
renameTidyData("tBody", "TimeBody")
renameTidyData("-mean()", "Mean", ignore_cases = TRUE)
renameTidyData("-std()", "StandardDeviation", ignore_cases = TRUE)
renameTidyData("-freq()", "Frequency", ignore_cases = TRUE)
renameTidyData("angle", "Angle")
renameTidyData("gravity", "Gravity")
  

step5_data <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(step5_data, "FinalData.txt", row.names = FALSE)





