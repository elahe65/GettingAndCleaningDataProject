## reading training data and saving them in R data frames

subject_train <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", dec = ".")
X_train <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", dec = ".")
Y_train <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/Y_train.txt", dec = ".")
## reading test data and saving them in R data frames

subject_test <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", dec = ".")
X_test <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", dec = ".")
Y_test <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt", dec = ".")

# merge train set and test set

X_both <- rbind(X_train, X_test)
Y_both <- rbind(Y_train, Y_test)
subject_both <- rbind(subject_train, subject_test)

#features list
features <- read.table("./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")
features$V2 <- as.character(features$V2)

# extracting name of features with "mean" or "std"
FeaturesWithMean <- features[grep("-mean()", features$V2),1]
FeaturesWithSTD <- features[grep("-std()", features$V2),1]
InxFeaturesWanted <- c(FeaturesWithMean,FeaturesWithSTD)
NamesFeaturesWanted <- features[InxFeaturesWanted,2]

#subseting data set with wanted features
X <- X_both[,InxFeaturesWanted]
NamesFeaturesWanted <- as.character(NamesFeaturesWanted)
names(X) <- NamesFeaturesWanted

# finding the index of rows for each activity
for(i in 1:30){
        act1 = which (Y_both$V1==1&subject_both$V1==i)
        act2 = which (Y_both$V1==2&subject_both$V1==i)
        act3 = which (Y_both$V1==3&subject_both$V1==i)
        act4 = which (Y_both$V1==4&subject_both$V1==i)
        act5 = which (Y_both$V1==5&subject_both$V1==i)
        act6 = which (Y_both$V1==6&subject_both$V1==i)
        #finding the mean value of each variable of X for each activity
        X_mean1 = colMeans(X[act1,])
        X_mean2 = colMeans(X[act2,])
        X_mean3 = colMeans(X[act3,])
        X_mean4 = colMeans(X[act4,])
        X_mean5 = colMeans(X[act5,])
        X_mean6 = colMeans(X[act6,])
        
        # write the final tidy data in a file
        IndependentTidyData = rbind(X_mean1,X_mean2, X_mean3, X_mean4, X_mean5, X_mean6)
        write.table(IndependentTidyData, paste("IndependentTidyData",i,".txt", sep = ""))
}






