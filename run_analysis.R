run_analysis <- function(){
  
  ## imports training and test sets, merge them
  
  training <- read.table("UCI HAR Dataset/train/x_train.txt")
  test <- read.table("UCI HAR Dataset/test/x_test.txt")
  
  mergedData <- rbind(training,test)
  
  ## imports activities coded from 1 to 6, paired to data sets
  ## also, imports activities actual names, with character data in column 2
  
  activity <- read.table("UCI HAR Dataset/activity_labels.txt")
  
  trainingActivity <- read.table("UCI HAR Dataset/train/y_train.txt")
  trainingActivity <- merge(trainingActivity, activity, by.x="V1", by.y="V1",sort=FALSE)
  
  testActivity <- read.table("UCI HAR Dataset/test/y_test.txt")
  testActivity <- merge(testActivity, activity, by.x="V1", by.y="V1",sort=FALSE)
  
  mergedActivity <- rbind(trainingActivity,testActivity)
  
  ## imports the subjects coded from 1 to 30, paired to data sets
  
  trainingSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
  testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  mergedSubjects <- rbind(trainingSubjects,testSubjects)
  
  ##imports labels for columns, with character data in column 2
  
  features <- read.table("UCI HAR Dataset/features.txt")
  
  ## name columns on data set
  colnames(mergedData) <- as.vector(features[,2])
  
  ## Add columns of Activity and Subjects to both data sets
  colnames(mergedActivity) <- c("activityNumber","activityName")
  colnames(mergedSubjects) <- c("subjects")
  
  mergedData <- cbind(mergedData,mergedActivity)
  
  mergedData <- cbind(mergedData,mergedSubjects)
  
  mergedData$typeOfData <- c(rep("training",nrow(training)),rep("test",nrow(test)))
  
  ## Determine which variables are about mean and std. dev.
  
  colHeaders <- as.vector(features[,2])
  isMean <- grepl("mean()",colHeaders,fixed=TRUE)
  isStdev <- grepl("std()",colHeaders,fixed=TRUE)
  
  ## Extracts only the measurements on the mean and standard deviation for each measurement
  
  data <- mergedData[,c(colHeaders[isMean],colHeaders[isStdev],"activityName","subjects","typeOfData")]
  
  ## Adds an ID column
  
  data$id <- c(1:nrow(data))
  
  ## Orders columns
  
  data <- data[c(70,68,67,69,1:66)]
  
  ##The new tidy data table with averages
  
  ##the loop that will generate the new data frame

  df <- data.frame()
  
  for(i in 1:nrow(activity)){
      actname <- activity[i,2]
      for(j in 1:30){
          v <- c(j,actname)
          for(k in 5:70){
                number <- data[data$subjects==j & data$activity == actname, k]
                mn <- mean(number,na.rm=TRUE)
                v <- c(v,mn)
          }  
          df <- rbind(df,v)
      }
  }
  
  ## clean the invalid rows where there is no intersecction of data
  validRows <- !is.nan(df[,3])
  df <- df[validRows,]
  
  ## Give labels, give names to activities, reorder columns
  df <- merge(df,activity,by.x="X4",by.y="V1")
  df <- df[c(69,2:68)]
  
  namesOfData <- names(data)
  namesOfData <- namesOfData[5:70]
  
  names(df) <- c("activity","subject",namesOfData)
  
  print(df)
}
