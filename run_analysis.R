

## 1: Merge training/test sets to create one data set
require(plyr)
features = read.table("features.txt")
activitylabels = read.table("activity_labels.txt")

colnames(activitylabels) <- c("A_ID", "Activity")

x_train <- read.table("train/X_train.txt") 
x_test <- read.table("test/X_test.txt") 
x <- rbind(x_train, x_test)

colnames(x)= features[,2] 

sub_train <- read.table("train/subject_train.txt") 
sub_test <- read.table("test/subject_test.txt") 
subject <- rbind(sub_train, sub_test)
colnames(subject)<- "S_ID"


y_train <- read.table("train/y_train.txt") 
y_test <- read.table("test/y_test.txt")
y <- rbind(y_train, y_test) 
colnames(y) = "A_ID"


merged_DS <- cbind(y, subject, x)

##rm(x_test, x_train, sub_test, sub_train, y_test, y_train, y, subject, x)

## 2: Extracts only the measurements on the mean and standard deviation for each measurement

selectColList <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- merged_DS[, c(1,2)]
Y <- merged_DS[, -c(1,2)]
Y <- Y[, selectColList]
names(Y) <- features[selectColList, 2]
names(Y) <- gsub("\\(|\\)", "", names(Y))
names(Y) <- tolower(names(Y))
merged_DS <- cbind(X, Y)

##rm(X, Y, selectColList)

## 3: Use descriptive activity names to name the activities in the data set

merged_DS = join(merged_DS, activitylabels, by= "A_ID")
colnames2 <- names(merged_DS)
## 4: Appropriately label the data set with descriptive activity names

####- >completed in the steps 1,2,3

for (i in 1:21)   
{ 
  colnames2[i] <- gsub("Gyro","AngularSpeed",colnames2[i])      
  colnames2[i] <- gsub("JerkMag","JerkMagnitude",colnames2[i])  
  colnames2[i] <- gsub("([Gg]ravity)","Gravity",colnames2[i])  
  colnames2[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colnames2[i])  
  colnames2[i] <- gsub("GyroJerk","AngularAcc",colnames2[i])  
  colnames2[i] <- gsub("Acc","Acceleration",colnames2[i])  
  colnames2[i] <- gsub("\\()","",colnames2[i])  
  colnames2[i] <- gsub("\\.std","StdDev",colnames2[i])  
  colnames2[i] <- gsub("\\.mean","Mean",colnames2[i])  
  colnames2[i] <- gsub("^t","time_",colnames2[i])  
  colnames2[i] <- gsub("^f","freq_",colnames2[i])  
}  
colnames(merged_DS)<-colnames2 


## 5: Create a second, independent tidy data set with the average of each variable for each activity and each

merged_DS1<-merged_DS[,names(merged_DS) != "activitylabels"]
merged_DS3<-merged_DS1[,names(merged_DS1) != c("A_ID","S_ID")]
tidy_DS=ddply(merged_DS3,.(A_ID=merged_DS1$A_ID,S_ID = merged_DS1$S_ID),numcolwise(mean))
tidy_DS=join(tidy_DS,activitylabels,by="A_ID")
tidy_DS<-tidy_DS[order(tidy_DS$A_ID,rank(tidy_DS$S_ID),decreasing=FALSE),] 

write.table(tidy_DS, './tidy.txt',row.names=FALSE) 




