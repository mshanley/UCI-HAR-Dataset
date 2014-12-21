##
## Happy Holidays ##

#Get in the livingroom where the festivities will take place
setwd('/Desktop/datacleansing/UCI HAR Dataset/');

# Hang the stockings (get the data)
features     = read.table('./features.txt',header=FALSE); 
activityType = read.table('./activity_labels.txt',header=FALSE);
subjectTrain = read.table('./train/subject_train.txt',header=FALSE);
xTrain       = read.table('./train/x_train.txt',header=FALSE); 
yTrain       = read.table('./train/y_train.txt',header=FALSE);

# Name the stockings - don't be naughty with the capitalization or else you will drive yourself up the chimney
colnames(activityType)  = c('activityID','activityType');
colnames(subjectTrain)  = "subjectID";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityID";

# OK, all you training data get overhere in one spot called trainingData
trainingData = cbind(yTrain,subjectTrain,xTrain);

# get the ornaments - I'm sticking with this theme
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Name the ornaments - 
colnames(subjectTest) = "subjectID";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityID";


# ok test data, let get together over here  
testData = cbind(yTest,subjectTest,xTest);


# Lets bring the ornaments and stockings together (the analagy is bunk now, but I'm committed.)
finalData = rbind(trainingData,testData);

# need columns for the assignment - use appropriate lables, need mean and standard deviation
colNames  = colnames(finalData); 
 logicalVector = (grepl("activity..",colnames) | grepl("subject..",colnames) | grepl("-mean..",colnames) & !grepl("-meanFreq..",colnames) & !grepl("mean..-",colnames) | grepl("-std..",colnames) & !grepl("-std()..-",colnames));
 finalData = finalData[logicalVector==TRUE];
 finalData = merge(finalData,activityType,by='activityID',all.x=TRUE);
 colNames = colnames(finalData);
 for (i in 1:length(colnames)){
       colnames[i] = gsub("\\()","",colnames[i])
       colnames[i] = gsub("-std$","StdDev",colnames[i])
       colnames[i] = gsub("^(t)","time",colnames[i])
       colnames[i] = gsub("^(f)","freq",colnames[i])
       colnames[i] = gsub("([Gg]ravity)","Gravity",colnames[i])
       colnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colnames[i])
       colnames[i] = gsub("[Gg]yro","Gyro",colnames[i])
       colnames[i] = gsub("AccMag","AccMagnitude",colnames[i])
       colnames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colnames[i])
       colnames[i] = gsub("JerkMag","JerkMagnitude",colnames[i])
       colnames[i] = gsub("GyroMag","GyroMagnitude",colnames[i])
};

colnames(finalData) = colNames;

finalDataNoActivityType = finalData[,names(finalData) != 'activitytype'];

tidayData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityID','subjectID')],by=list(activityID=finalDataNoActivityType$activityID,subjectID = finalDataNoActivityType$subjectID),mean);

#I have misspelled the word 'tiday' but I am going with it! 
tidyData    = merge(tidayData,activityType,by='activityID',all.x=TRUE);

write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t');