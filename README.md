runAnalysis<-function() {
    dataPath<-file.path("./data" , "UCI HAR Dataset")
    ## reading data
    activityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
    activityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)
    subjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)
    subjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)
    featuresTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
    featuresTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)
    ## combining test and train data sets
    subjectDataCombined <- rbind(subjectTrain, subjectTest)
    activityDataCombined<- rbind(activityTrain, activityTest)
    featuresDataCombined<- rbind(featuresTrain, featuresTest)
    ## naming subject and activity columns 
    names(subjectDataCombined)<-c("subject")
    names(activityDataCombined)<- c("activity")
    ## reading variables names for features as table
    featuresDataNames <- read.table(file.path(path_rf, "features.txt"),head=FALSE)
    ## naming combined data set with read variable names
    names(featuresDataCombined)<- featuresDataNames$V2
    ## combining all data in one data set
    combineSubjectActivity<- cbind(subjectDataCombined, activityDataCombined)
    compliteData<-cbind(featuresDataCombined, combineSubjectActivity)
    ## reduce data set to mean and stardart deviation columns
    meanStdFeaturesNames<-featuresDataNames$V2[grep("mean\\(\\)|std\\(\\)", featuresDataNames$V2)]
    selectedNames<-c(as.character(meanStdFeaturesNames), "subject", "activity" )
    compliteData<-subset(compliteData,select=selectedNames)
    ## appling factoring to activity column 
    activityNames <- read.table(file.path(path_rf, "activity_labels.txt"),header = FALSE)
    compliteData$activity<-factor(compliteData$activity, labels = activityNames$V2)
    ## chnaging names to more descritive 
    names(compliteData)<-gsub("^t", "time", names(compliteData))
    names(compliteData)<-gsub("^f", "frequency", names(compliteData))
    names(compliteData)<-gsub("Acc", "Accelerometer", names(compliteData))
    names(compliteData)<-gsub("Gyro", "Gyroscope", names(compliteData))
    names(compliteData)<-gsub("Mag", "Magnitude", names(compliteData))
    names(compliteData)<-gsub("BodyBody", "Body", names(compliteData))
    ## return tidy data
    compliteData
}
getTidyData<-function() {
    ## get tidy data
    Data1<-runAnalysis()
    ## find mean for subject and activity 
    Data2<-aggregate(. ~subject + activity, Data1, mean)
    ## sorting by subject and activity
    Data2<-Data2[order(Data2$subject,Data2$activity),]
    ## writing data to output file
    write.table(Data2, file = "tidydata.txt",row.name=FALSE)
    ## return tidier data
    Data2
}
