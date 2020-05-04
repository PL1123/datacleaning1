## Data Cleaning 

#### Downloading files 
if (!file.exists("data")) {
        dir.create("data")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/acceletometers.zip", mode = "wb", method ="curl")

#### getting download date
dateDownloaded <- date()
dateDownloaded

#### unzip file 
unzip(zipfile = "./data/acceletometers.zip", exdir="./data")

#### check the directory 
list.files("./data")

#### setting working directory 
getwd()
setwd("D:/R/data_cleaning_coursera/data/UCI HAR Dataset")

## loading data according to README.txt
#### Load testing data
Xtest <- read.table("./test/X_test.txt", header = FALSE)
ytest <- read.table("./test/y_test.txt", header = FALSE)
subj_test <- read.table("./test/subject_test.txt", header = FALSE)

#### Load training data
Xtrain <- read.table("./train/X_train.txt", header = FALSE)
ytrain <- read.table("./train/y_train.txt", header = FALSE)
subj_train <- read.table("./train/subject_train.txt", header = FALSE)

#### Load activity labels
actlabel <- read.table("./activity_labels.txt", header = FALSE)

#### Load Feature data
feat <- read.table("./features.txt", header = FALSE)

## Checking data loaded and its general format
head(Xtrain); head(ytrain); head(Xtest); head(ytest); head(subj_train); 
head(subj_test); head(feat); head(actlabel)

## Naming columns to be clearer
colnames(Xtest) <- feat[,2]; colnames(Xtrain) <- feat[,2]
colnames(ytest) <- "act"; colnames(ytrain) <- "act"
colnames(subj_test) <- "subj"; colnames(subj_train) <- "subj"
colnames(actlabel) <- c("act", "acttype")

## 1. Merging test and training sets
#### Merging testing data
test <- cbind(Xtest, ytest, subj_test)
#### Merging training data
train <- cbind(Xtrain, ytrain, subj_train)
#### Full Merge
Merged <- rbind(train, test)


## 2. Extracts only the measurements on the mean 
## and standard deviation for each measurement.

#grepl returns a logical vector (match or not
#for each element of x).So we can use grepl to 
#to identify mean & std and store the logical vector.
#The logical vector can be used to select within 
#Merged. 

allcols = colnames(Merged)
logical <- grepl("act", allcols)|grepl("subj", allcols)|grepl("std..", allcols)|grepl("mean..", allcols)
Mergednew <- Merged[logical == TRUE]
str(Mergednew) # checking


## 3. Uses descriptive activity names to name 
## the activities in the data set

#Merge activity label: actlabel created earlier
Mergednew <- merge(Mergednew, actlabel, by='act', all.x=TRUE)
#Naming by making act a factor 
Mergednew$act <- factor(Mergednew$act, levels = 1:nrow(actlabel),
                        labels = actlabel[1:nrow(actlabel),2])


## 4. Appropriately labels the data set with descriptive 
## variable names.

names(Mergednew) <- gsub("Acc", "Acceleromter", names(Mergednew))
names(Mergednew) <- gsub("BodyBody", "Body", names(Mergednew))
names(Mergednew) <- gsub("^t", "Time", names(Mergednew))
names(Mergednew) <- gsub("^f", "Frequency", names(Mergednew))
names(Mergednew) <- gsub("Mag", "Magnitude", names(Mergednew))
names(Mergednew) <- gsub("Mean()", "Mean", names(Mergednew))
names(Mergednew) <- gsub("Std()", "Std", names(Mergednew))
View(Mergednew) #checking

## 5. Create a second, independent tidy data set with the average of 
## each variable for each activity and each subject.

library(dplyr)
Tidydataset <- aggregate(. ~subj + act, Mergednew, mean)
Tidydataset <- Tidydataset[order(Tidydataset$subj, Tidydataset$act),]
write.table(Tidydataset, file = "Tidydataset.txt", row.name = FALSE)
