run_analysis <- function() { 
  library(dplyr)
  #read in all used data
  TestTE <- read.table(file="C:/Users/Sabine/Downloads/UCI_HAR_Dataset/test/X_test.txt")
  TestTR <- read.table(file="C:/Users/Sabine/Downloads/UCI_HAR_Dataset/train/X_train.txt")
  TestYTrain<- read.table(file="C:/Users/Sabine/Downloads/UCI_HAR_Dataset/train/y_train.txt")
  TestYTest<- read.table(file="C:/Users/Sabine/Downloads/UCI_HAR_Dataset/test/y_test.txt")
  TestSubjTrain<- read.table(file="C:/Users/Sabine/Downloads/UCI_HAR_Dataset/train/subject_train.txt")
  TestSubjTest<- read.table(file="C:/Users/Sabine/Downloads/UCI_HAR_Dataset/test/subject_test.txt")
  Activities<- read.table(file="C:/Users/Sabine/Downloads/UCI_HAR_Dataset/activity_labels.txt")
  
  #Merge the training and the test sets
  TestALL_X<-rbind(TestTE,TestTR)
  TestALL_Y<-rbind(TestYTrain,TestYTest)
  TestALL_Sub<-rbind(TestSubjTrain,TestSubjTest)
  
  # Appropriately labels the data set with descriptive variable names. 
  Headrs<-read.table(file="C:/Users/Sabine/Downloads/UCI_HAR_Dataset/features.txt")
  Headrs[,2]<-gsub("-", "_", Headrs[,2])
  Headrs[,2]<-gsub("\\(|\\)", "", Headrs[,2])
  colnames(TestALL_X) <- Headrs$V2
  names(TestALL_Y)<-"activity"
  names(TestALL_Sub)<-"subjects"
  
  #Merge all data
  TestALL<-cbind(TestALL_Sub,TestALL_Y,TestALL_X)
  
  #Extracts only the measurements on the mean and standard deviation for each measurement. 
  TestSOME_mean <- TestALL[ , grepl(( "mean" ) , names( TestALL ) ) ]
  TestSOME_std <- TestALL[ , grepl(( "std" ) , names( TestALL ) ) ]
  ind <- apply( TestSOME_std , 1 , function(x) any( x > 0 ) )
  TestSOME_std[ ind , ]
  Test_ALL<-cbind(TestALL_Sub, TestALL_Y,TestSOME_mean,TestSOME_std)
  Test_ALL <- Test_ALL[ , !grepl(( "-X"  ) , names( Test_ALL ) ) ]
  Test_ALL <- Test_ALL[ , !grepl(( "-Y"  ) , names( Test_ALL ) ) ]
  Test_ALL <- Test_ALL[ , !grepl(( "-Z"  ) , names( Test_ALL ) ) ]
  Test_ALL <- Test_ALL[ , !grepl(( "Freq()"  ) , names( Test_ALL ) ) ]
  ind <- apply( Test_ALL , 1 , function(x) any( x > 0 ) )
  Test_ALL[ ind , ] 

  #Uses descriptive activity names to name the activities in the data set
  Test_ALL$activity <- factor(Test_ALL$activity, levels = Activities[, 1], labels = Activities[, 2]) 
 
  #From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  Test_ALLGroup<-Test_ALL %>% group_by(subjects, activity) 
  Test_Data <- aggregate(Test_ALLGroup[, 3:ncol(Test_ALLGroup)],by=list( activity = Test_ALLGroup$activity,subject = Test_ALLGroup$subjects),mean)
  
  print(dim(Test_Data))
  View(Test_Data)
  print(Test_Data[1,])
  
  # write the tidy_data to a file
  write.table(Test_Data, row.name=FALSE, file = "tidy_data.txt")
}
