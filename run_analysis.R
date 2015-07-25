#set the working directory
setwd("/Users/sakinah/Documents/CleaningData")

## PART 1:Merges the training and the test sets to create one data set.
# Reading the Features Name and Activity Label
featureName<-read.table("features.txt")
activityLabel<-read.table("activity_labels.txt")

# Reading training data from file
subjectTrainData<-read.table("train/Subject_train.txt")
actTrainData<-read.table("train/Y_train.txt")
feaTrainData<-read.table("train/X_train.txt")

# Reading testing data from file
subjectTestData<-read.table("test/Subject_test.txt")
actTestData<-read.table("test/Y_test.txt")
feaTestData<-read.table("test/X_test.txt")


# Merges the training  and test data to create one dataset
subjectData<-rbind(subjectTrainData,subjectTestData)
activityData<-rbind(actTrainData,actTestData)
featureData<-rbind(feaTrainData,feaTestData)

#Naming the variables/features in the given columns
colnames(featureData)<-t(featureName[2]) # t()Transpose the matrix
colnames(activityData)<-"Activity"
colnames(subjectData)<-"Subject"

#complete data
compData<-cbind(featureData,activityData,subjectData)

##PART2:Extracts only the measurements on the mean and standard deviation for each measurement. 
colWithMeanStd<-grep(".*Mean.*|.*Std.*",names(compData),ignore.case = TRUE)
newRequiredCol<-c(colWithMeanStd,562,563)
dim(compData)# dimension of complete data
# new data with required column
newData<-compData[ , newRequiredCol]
dim(newData)# dimension of complete data

##PART3: Uses descriptive activity names to name the activities in the data set
# change the type of data to character so that it can accept the activity names from the metadata
newData$Activity<-as.character(newData$Activity)
for (k in 1:6){
  newData$Activity[newData$Activity==k]<-as.character(activityLabel[k,2])
}
# factor the variable name -activity
newData$Activity<-as.factor(newData$Activity)


##PART4:Appropriately labels the data set with descriptive variable names.
# display the name in the original data
names(newData)
# using gsub perform replacement of the first and all matches respectively

names(newData)<-gsub("Acc","Accelerometer",names(newData))
names(newData)<-gsub("Gyro","Gyroscope",names(newData))
names(newData)<-gsub("Mag","Magnitude",names(newData))
names(newData)<-gsub("-mean()","Mean",names(newData))
names(newData)<-gsub("-std()","Std",names(newData))
names(newData)<-gsub("Freq()","Frequency",names(newData))
names(newData)<-gsub("^f","Frequency",names(newData))
names(newData)<-gsub("angle","Angle",names(newData))
names(newData)<-gsub("gravity","Gravity",names(newData))
names(newData)<-gsub("BodyBody","Body",names(newData))
names(newData)<-gsub("^t","Time",names(newData))
names(newData)<-gsub("tBody","TimeBody",names(newData))

##PART5:From the data set in step 4, creates a second, 
## independent tidy data set with the average of each 
## variable for each activity and each subject.

# factor the subject 
newData$Subject<-as.factor(newData$Subject)
#newData<-data.frame(newData)
# aggregate = Splits the data into subsets, compute mean for each, 
#           and returns the result in a convenient form.
tidyData<-aggregate(.~Subject+Activity,newData,mean)
# order the data
tidyData<-tidyData[order(tidyData$Subject,tidyData$Activity),]
# write the new clean data in txt file
write.table(tidyData,file="Tidys.txt", row.names = FALSE)


