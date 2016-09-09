############################################
# 1.   Used Libraries

library(data.table)
library(dplyr)
library(plyr)
library(reshape2)
library(xlsx)
############################################
# 2.   Set directory for train sets
setwd("C:/Users/Francisco/Documents/proyectos/John Hopkins/final/UCI HAR Dataset/train")
############################################
# 3.       Read Train sets                   
X_train <- fread("X_train.txt")           
Y_train <- fread("Y_train.txt")
sub_train <- fread("subject_train.txt")
############################################

############################################
# 4.     Activity Names for train sets
rnames <- factor(Y_train$V1, labels = c("WALKING", "WALKING_UPSTAIRS",
                 "WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
############################################

############################################
# 5.     Bind Train sets
Train_DF <- cbind(rnames, X_train)
Train_DF <- cbind(sub_train, Train_DF)
############################################

#############################################
# 6.     Set Names for train data frame
names_train <- fread("features.txt")
names_train <- subset(names_train, select = V2)
names <- as.character(names_train$V2)
names(Train_DF)[3:563] <- c(names)
names(Train_DF)[1:2] <- c("Subject", "Activity")

##############################################
###############################################
# 7.   Set directory for test sets
setwd("C:/Users/Francisco/Documents/proyectos/John Hopkins/final/UCI HAR Dataset/test")
############################################
# 8.      Read test sets                   
X_test <- fread("X_test.txt")           
Y_test <- fread("Y_test.txt")
sub_test <- fread("subject_test.txt")
#ba_x_t <- fread("body_acc_x_train.txt")
############################################

############################################
#  9.     Activity Names for test sets
rnames_t <- factor(Y_test$V1, labels = c("WALKING", "WALKING_UPSTAIRS",
              "WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
############################################

############################################
# 10.     Bind Test sets
Test_DF <- cbind(rnames_t, X_test)
Test_DF <- cbind(sub_test, Test_DF)
############################################

#############################################
# 11.     Set Names for test data frame
names_test <- fread("features.txt")
names_test <- subset(names_test, select = V2)
names_t <- as.character(names_test$V2)
names(Test_DF)[3:563] <- c(names_t)
names(Test_DF)[1:2] <- c("Subject", "Activity")

##############################################
# 12.   Set directory for tidy data set
setwd("C:/Users/Francisco/Documents/proyectos/John Hopkins/final")
##############################################
##############################################
# 13. Bind Train and test sets
prueba <- rbind(Train_DF, Test_DF)
prueba <- prueba[order(Subject),]
names(prueba)
###############################################
###############################################
# 14. Change variable names for tidy set
feat <- fread("features.txt")
nombres <- feat$V2
m <- 9
patt <- c("tBodyAcc", "tGravityAcc", "tBodyGyro", "fBodyAcc",
          "fBodyGyro", "\\(\\)", "-" , "-" , "," )
changes <- c("TBA","TGA","TBG","FBA","FBG","","","","_")

for(i in 1:m){
  sub(pattern = patt[i], replacement = changes[i], x =nombres)->nombres
}
names(prueba)[3:563] <- nombres
head(nombres,-50)
#################################################
#################################################
# 15.  Subset for mean and std variables

mpatt <- c("mean", "std")
nomsel_mean <- grep(pattern = mpatt[1], nombres, value=TRUE)
nomsel_std <- grep(pattern = mpatt[2], nombres, value=TRUE)
nomsel <-c("Subject","Activity",nomsel_mean, nomsel_std)
nomsel_1 <- c(nomsel_mean, nomsel_std)
ext <- prueba[, nomsel, with = FALSE]
ext$Subject <- as.factor(ext$Subject)
###################################################
# 16.   Create Tidy data and write tidy.txt
setkey(prueba, Subject, Activity)
ext.melted <- melt(ext, id = c("Subject", "Activity"))
tidy <-dcast(ext.melted, Subject + Activity ~ variable, mean)
write.xlsx(tidy, "C:/Users/Francisco/Documents/proyectos/John Hopkins/final/tidy.xlsx")
