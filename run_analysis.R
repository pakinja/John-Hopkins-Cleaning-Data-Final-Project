library(data.table)
setwd("C:/Users/Francisco/Documents/proyectos/John Hopkins/final/UCI HAR Dataset/train")
############################################
#       Read Train sets                   
X_train <- fread("X_train.txt")           
Y_train <- fread("Y_train.txt")
sub_train <- fread("subject_train.txt")
#ba_x_t <- fread("body_acc_x_train.txt")
############################################

############################################
#       Activity Names
rnames <- factor(Y_train$V1, labels = c("WALKING", "WALKING_UPSTAIRS",
                 "WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
############################################

############################################
#       Bind Train sets
Train_DF <- cbind(rnames, X_train)
Train_DF <- cbind(sub_train, Train_DF)
############################################

#############################################
#     Names
names_train <- fread("features.txt")
names_train <- subset(names_train, select = V2)
names <- as.character(names_train$V2)
names(Train_DF)[3:563] <- c(names)
names(Train_DF)[1:2] <- c("Subject", "Activity")

##############################################

library(data.table)
setwd("C:/Users/Francisco/Documents/proyectos/John Hopkins/final/UCI HAR Dataset/test")
############################################
#       Read Train sets                   
X_test <- fread("X_test.txt")           
Y_test <- fread("Y_test.txt")
sub_test <- fread("subject_test.txt")
#ba_x_t <- fread("body_acc_x_train.txt")
############################################

############################################
#       Activity Names
rnames_t <- factor(Y_test$V1, labels = c("WALKING", "WALKING_UPSTAIRS",
              "WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
############################################

############################################
#       Bind Test sets
Test_DF <- cbind(rnames_t, X_test)
Test_DF <- cbind(sub_test, Test_DF)
############################################

#############################################
#     Names
names_test <- fread("features.txt")
names_test <- subset(names_test, select = V2)
names_t <- as.character(names_test$V2)
names(Test_DF)[3:563] <- c(names_t)
names(Test_DF)[1:2] <- c("Subject", "Activity")

##############################################
setwd("C:/Users/Francisco/Documents/proyectos/John Hopkins/final")

library(data.table)
library(dplyr)
library(plyr)
library(reshape2)
library(xlsx)

prueba <- rbind(Train_DF, Test_DF)
prueba <- prueba[order(Subject),]
names(prueba)

######################################
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
#######################################

mpatt <- c("mean", "std")

nomsel_mean <- grep(pattern = mpatt[1], nombres, value=TRUE)
nomsel_std <- grep(pattern = mpatt[2], nombres, value=TRUE)

nomsel <-c("Subject","Activity",nomsel_mean, nomsel_std)
nomsel_1 <- c(nomsel_mean, nomsel_std)
ext <- prueba[, nomsel, with = FALSE]

ext$Subject <- as.factor(ext$Subject)

#ext_1 <- prueba[, nomsel_1, with = FALSE]
########################################

setkey(prueba, Subject, Activity)

ext.melted <- melt(ext, id = c("Subject", "Activity"))
tidy <-dcast(ext.melted, Subject + Activity ~ variable, mean)

########################################

write.xlsx(tidy, "C:/Users/Francisco/Documents/proyectos/John Hopkins/final/tidy.xlsx")
