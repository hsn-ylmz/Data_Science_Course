library(dplyr) ## dplyr provides lot of tools for tidying data sets (ex: summarise)
library(tibble) ## tibble is a tool to creates tidy data sets



setwd('D:/R projects/R programming/') ##Setting working directory wrt u choice

## Set url to download data
url <-
'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'

## Download zip file
download.file(url, destfile = 'harus.zip')

## Unzip your file
unzip('harus.zip')

## Look your working directory to select folder and files that you will work
list.files()

## Getting feature list of data
features <- read.table('UCI HAR Dataset/features.txt')
features <- features[,2]


## Get  activity list
activities <- read.table('UCI HAR Dataset/activity_labels.txt')
activities <- activities[,2]

## Get subject list
sub1 <- read.table('UCI HAR Dataset/train/subject_train.txt')
sub2 <- read.table('UCI HAR Dataset/test/subject_test.txt')


## Getting tain and test data sets
xtrain <- read.table('UCI HAR Dataset/train/X_train.txt', col.names = features)
ytrain <- read.table('UCI HAR Dataset/train/Y_train.txt')
xtest <- read.table('UCI HAR Dataset/test/X_test.txt', col.names = features)
ytest <- read.table('UCI HAR Dataset/test/Y_test.txt')


## Merge data sets
x <- bind_rows(xtrain, xtest)
y <- bind_rows(ytrain, ytest)
sub <- bind_rows(sub1, sub2)

## Free up some memory
rm(xtest, xtrain, ytrain, ytest, sub1, sub2)

## Extract onlu mean and std measurements
data <- x[,grep('mean|std', colnames(x))]


## Creates a col for naming as Activity_name
data <- rownames_to_column(data) ##!!!!row_to_column() is a tibble function!!!
colnames(data)[1] <- 'Activity_name'

## Factorize result(y) data set to naming activities correctly
y$V1 <- factor(y$V1, levels = c(1,2,3,4,5,6), labels = activities)
data$Activity_name <- y


## Creates a col for naming as Sunject
data <- rownames_to_column(data)
colnames(data)[1] <- 'Subject'
data$Subject <- sub$V1

## Take data group by Subject and Activity name and get means of all features
## !!! piping(%>%) and group_by-summarise functions are dplyr functions
tidydata <- data %>% group_by(Subject, Activity_name) %>% 
        summarise_all(list(mean))


