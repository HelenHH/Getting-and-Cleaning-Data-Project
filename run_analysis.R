library(plyr)
library(stringr)
library(reshape2)

## 1. Merges the training and the test sets to create one data set.

#load features.txt file
dataFeat <-read.table ("/Desktop/DC_project/UCI\ HAR\ Dataset/features.txt")
measures <- as.vector(as.matrix(dataFeat[,2]))

#load training data sets that contain 3 componets: subjects, activity and measures
dataX_train <-read.table ("/Desktop/DC_project/UCI\ HAR\ Dataset/train/X_train.txt")
names(dataX_train) <- measures
dataY_train <-read.table ("/Desktop/DC_project/UCI\ HAR\ Dataset/train/Y_train.txt")
names(dataY_train) <- c("activity")
subject_train <-read.table ("/Desktop/DC_project/UCI\ HAR\ Dataset/train/subject_train.txt")
names(subject_train) <- c("subjects")

#load test data sets that contain 3 componets: subjects, activity and measures
dataX_test <-read.table ("/Desktop/DC_project/UCI\ HAR\ Dataset/test/X_test.txt")
names(dataX_test) <- measures
dataY_test <-read.table ("/Desktop/DC_project/UCI\ HAR\ Dataset/test/Y_test.txt")
names(dataY_test) <- c("activity")
subject_test <-read.table ("/Desktop/DC_project/UCI\ HAR\ Dataset/test/subject_test.txt")
names(subject_test) <- c("subjects")

#merge training and test sets
a<-rbind(dataX_train, dataX_test)
b<-rbind(dataY_train, dataY_test)
c<-rbind(subject_train, subject_test)

df <-cbind(c, a, b)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement
measure_indx <- regexpr("*mean\\(\\)*", measures)>0 | regexpr("*std\\(\\)*", measures)>0
df_Means_Stds <- cbind(c, a[,measure_indx], b)

## 3. Uses descriptive activity names to name the activities in the data set

#load activity label file and create a factor variable 
activity_labels <- read.table("/Desktop/DC_project/UCI\ HAR\ Dataset/activity_labels.txt")
activity_Levels <- factor(as.vector(as.matrix(b[,1])),labels=activity_labels[,2])

## replace y-variable with activity_Levels in df and df_Means-Stds 
df <- cbind(c, a, activity_Levels)
names(df)[length(names(df))] <- "activity"

df_Means_Stds <- cbind(c, a[,measure_indx], activity_Levels)   
names(df_Means_Stds)[length(names(df_Means_Stds))] <- "activity"

## 4. Appropriately labels the data set with descriptive variable names
colnames(df_Means_Stds) <- tolower(str_replace_all(colnames(df_Means_Stds), "([A-Z]{1})", ".\\1"))
colnames(df_Means_Stds) <- str_replace_all(colnames(df_Means_Stds), "\\-mean\\(\\)\\-", ".mean")
colnames(df_Means_Stds) <- str_replace_all(colnames(df_Means_Stds), "\\-std\\(\\)\\-", ".std") 
colnames(df_Means_Stds) <- str_replace_all(colnames(df_Means_Stds), "\\-mean\\(\\)", ".mean")
colnames(df_Means_Stds) <- str_replace_all(colnames(df_Means_Stds), "\\-std\\(\\)", ".std") 
colnames(df_Means_Stds) <- str_replace_all(colnames(df_Means_Stds), "^t.","timedomain.")
colnames(df_Means_Stds) <- str_replace_all(colnames(df_Means_Stds), "^f.","frequencydomain.") 

## 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
tidy <- ddply(df_Means_Stds, id.vars=c("subjects", "activity"),
                   .(subjects, activity), numcolwise(mean))

#export tidy data as txt file for submission
write.csv(tidy, file = "tidy.txt", row.names = FALSE)

#reload tidy.tex
data1 <-read.csv("/Desktop/DC_project/tidy.txt")
dim(data1)
names(data1)
head(data1)
tail(data1)
str(data1)




