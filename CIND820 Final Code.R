# load source files

setwd("~/My Stuff/Courses/CIND820/Baseball Cleaned Data")

battingData <- read.csv("batting.csv", stringsAsFactors = FALSE)
playerData <- read.csv("player.csv", stringsAsFactors = FALSE)
teamData <- read.csv("team.csv", stringsAsFactors = FALSE)
homeGameData <- read.csv("home_game.csv", stringsAsFactors = FALSE)
parkData <- read.csv("park.csv", stringsAsFactors = FALSE)
managerData <- read.csv("manager.csv", stringsAsFactors = FALSE)
awardData <- read.csv("manager_award.csv", stringsAsFactors = FALSE)
playerColData <- read.csv("player_college.csv", stringsAsFactors = FALSE)
collegeData <- read.csv("college.csv", stringsAsFactors = FALSE)
salaryData <- read.csv("salary.csv", stringsAsFactors = FALSE)

# merge files and trim to necessary fields

# merge batting and player data
allData <- merge(battingData, playerData)
#trim dataframe
allData <- allData[, c(1,2,4,5,6,25,26,27,28)]
#calculate player age
allData$player_age <- (allData$year - allData$birth_year)
#trim birthyear
allData <- allData[, c(1,2,3,5,6,8,9,10)]

#merge team data
allData <- merge(allData, teamData)

#merge park data
allData <- merge(allData, homeGameData)
allData <- merge(allData, parkData)
#trim team/park names
allData <- allData[, c(1,2,3,4,5,6,7,8,9,10,11)]

#merge college
allData <- merge(x=allData, y=playerColData, by="player_id",all.x = TRUE)
allData <- merge(x=allData, y=collegeData, by="college_id", all.x=TRUE)
allData$college_id[is.na(allData$college_id)] <-"None"

#merge salary
allData <- merge(x=allData, y=salaryData, by= c("player_id", "year"),all.x=TRUE)
#trim datastructure and clean names
allData <- allData[, c(1,2,3,4,5,7,8,9,10,11,12,16)]
names(allData)[5] <- "team_id"

#merge manager and award data
awardData <- merge(x=awardData, y=managerData, by=c("manager_id","year"),all.x=TRUE)
allData <- merge(x=allData, y=awardData, by=c("team_id","year"),all.x=TRUE)
allData$award_id[is.na(allData$award_id)] <-"None"

#final cleaning
allData <- allData[, c(4,5,6,7,10,11,12,14)]
allData <- allData[, c(2,3,4,5,6,7,8)]

## Dataset testing

##Create training dataset and test dataset

totalRows <- nrow(allData) #Find number of player seasons

numTraining <- round(totalRows*0.8) #Set rows of training data

set.seed(516) #set random

dataIndex <- sample(totalRows,numTraining) #Create random index
trainData <- allData[dataIndex,] #Create training set
testData <- allData[-dataIndex,] #Create test set

# Support Vector Machine 
library(e1071)
library(Metrics)
svmtrainData <- svm(ops~., data=trainData, kernal=radial) #Train data svm

predicttrainData <- predict(svmtrainData, testData) #Predict using the svm algorithm, on the test dataset

svmrmse <- rmse(predicttrainData,allData$ops) #RSME for SVM

svmmse <- mse(predicttrainData,allData$ops) #MSE for SVM

svmmae <- mae(predicttrainData,allData$ops) #MAE for SVM


# Multiple linear regression

ops_regressor = lm(ops~., data=trainData) # Multiple linear regression

predictmlr_ops <- predict(ops_regressor, data=testData) # Predict using mlr algorithm on test dataset

mlrrmse <- rmse(predictmlr_ops, allData$ops) # RSME for MLR

mlrmse <- mse(predictmlr_ops, allData$ops) #MSE for MLR

mlrmae <- mae(predictmlr_ops, allData$ops) #MAE for MLR

anovamlr <- anova(ops_regressor) #ANOVA analysis


##Tables in project
anovamlrpr<-anovamlr$`Pr(>F)`

fieldNames <- c("park_id","g","player_age","w","salary","award_id","")
labels <- c("MAE", "MSE","RMSE")

numbers<-matrix(c(mlrmae,mlrmse,mlrrmse,svmmae,svmmse,svmrmse),ncol=3,byrow=TRUE)

colnames(numbers) <- c("MAE", "MSE","RMSE")
rownames(numbers) <- c("MLR","SVM")

numbers <- as.table(numbers)
numbers
library(xtable)
xtable(labels,numbers)

#Variable plots

plot(allData$ops,allData$g)
plot(allData$ops,allData$player_age)
plot(allData$ops,allData$w)
plot(allData$ops,allData$salary)


