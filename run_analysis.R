##Tidy dateset with analysis


##Read features and activities
Features <- function(directory){
  
  ##setwd("C:/Users/gaborm/Desktop/Own/R/GettingData/Week3/UCI HAR Dataset")
  
  features <- read.table(paste(directory,"features.txt",sep="/"))
    
  features  
  
}

Activities <- function(directory){
  
  activities <- read.table(paste(directory,"activity_labels.txt",sep="/"))
  
  colnames(activities) <- c("labels", "activities")
  
  activities  
  
}

##Read training data
TrainingData <- function(directory, maindir){
  
  data <- read.table(paste(directory,"X_train.txt",sep="/"))
  
  labels <- read.table(paste(directory,"y_train.txt",sep="/"))
  
  subject <- read.table(paste(directory,"subject_train.txt",sep="/"))
    
  #Merge data into one training data.frame
  
  colnames(labels) <- c("labels")
  
  ##Activities <- merge(Activities(maindir), labels, by.x="labels", by.y="labels")
  
  Cols <- t(c("activities", "labels", "subject", 
            as.character(Features(maindir)[,2])))
  
  DataMat <- cbind(labels[], subject[], data[])
  
  TrainingData <- merge(Activities(maindir), as.data.frame(DataMat), by.x="labels", by.y="labels")
  
  colnames(TrainingData) <- Cols
  
  TrainingData[1:5,1:5]
  
}

TrainingData("C:/Users/gaborm/Desktop/Own/R/GettingData/Week3/UCI HAR Dataset/train", 
        "C:/Users/gaborm/Desktop/Own/R/GettingData/Week3/UCI HAR Dataset")

##Read test data
TestData <- function(directory, maindir){
  
  data <- read.table(paste(directory,"X_test.txt",sep="/"))
  
  labels <- read.table(paste(directory,"y_test.txt",sep="/"))
  
  subject <- read.table(paste(directory,"subject_test.txt",sep="/"))
  
  #Merge data into one training data.frame
  
  colnames(labels) <- c("labels")
  
  Cols <- t(c("activities", "labels", "subject", 
              as.character(Features(maindir)[,2])))
  
  ##Activities <- merge(Activities(maindir), labels, by.x="labels", by.y="labels")
  
  DataMat <- cbind(labels[], subject[], data[])
  
  TestData <- merge(Activities(maindir), as.data.frame(DataMat), by.x="labels", by.y="labels")
  
  colnames(TestData) <- Cols
  
  TestData
  
}

##get descritpive statistics for each measurment

getdata <- function(maindir, trainingdir, testdir){
  
  alldata <- rbind(TrainingData(trainingdir, maindir),TestData(testdir, maindir))
  
  dims <- as.character(c("mean", "stdev"))
  
  stat <- as.table(rbind(apply(alldata[,4:length(alldata)], 2, mean), 
                apply(alldata[,3:length(alldata)], 2, sd)), 
                rownames=t(dims))
  
  stat
  
}

##get statistics broken down by activities and subjects

getBDstat <- function(maindir, trainingdir, testdir){
  
  alldata <- rbind(TrainingData(trainingdir, maindir), TestData(testdir, maindir))
  
  statBD <- rbind(with(alldata,tapply(alldata[,20], list(alldata$activities, alldata$subject), mean)),
                  with(alldata,tapply(alldata[,20], list(alldata$activities, alldata$subject), sd)))
  
  statBD
}

##Run functions needed with the directories in which data is: (maindir, trainingdir, testdir) in this order

getdata("C:/Users/gaborm/Desktop/Own/R/GettingData/Week3/UCI HAR Dataset", 
        "C:/Users/gaborm/Desktop/Own/R/GettingData/Week3/UCI HAR Dataset/train",
        "C:/Users/gaborm/Desktop/Own/R/GettingData/Week3/UCI HAR Dataset/test")

getBDstat("C:/Users/gaborm/Desktop/Own/R/GettingData/Week3/UCI HAR Dataset", 
        "C:/Users/gaborm/Desktop/Own/R/GettingData/Week3/UCI HAR Dataset/train",
        "C:/Users/gaborm/Desktop/Own/R/GettingData/Week3/UCI HAR Dataset/test")