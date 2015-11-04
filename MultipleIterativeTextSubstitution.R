# example of iterative text substitutions

library(dplyr)
library(stringr)

old_pattern <- c("tB", "tG", "fB", 
                 "BodyBody", 
                 "BodyAcc-|BodyAccM","GravityAcc-|GravityAccM",
                 "BodyAccJerk-|BodyAccJerkM","BodyGyro-|BodyGyroM",
                 "BodyGyroJerk-|BodyGyroJerkM",
                 "ag-mean\\(\\)","ag-std\\(\\)","mean\\(\\)-X",
                 "mean\\(\\)-Y","mean\\(\\)-Z","std\\(\\)-X",
                 "std\\(\\)-Y","std\\(\\)-Z")

new_pattern <- c("time.B","time.G","freq.B",
                 "Body",
                 "body.accelero.nojerk.","gravity.accelero.nojerk.",
                 "body.accelero.jerk.","body.gyro.nojerk.",
                 "body.gyro.jerk.",
                 "norm.mean","norm.std","x.mean","y.mean","z.mean",
                 "x.std","y.std","z.std")

mapping <- data.frame(old_pattern, new_pattern)

features <- (read.table("UCI HAR Dataset/features.txt"
            , stringsAsFactors = FALSE) %>%
  select(V2))[,1]
features <- features[str_detect(features, "mean\\(\\)") | str_detect(features, "std\\(\\)")]

subs <- function(x,y) {
  for (i in 1:length(y[,1])) {
    x <- sub(y[,1][i],y[,2][i], x)
  }
  x
}

f <- features
f <- subs(f, mapping)