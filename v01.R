#this file is purely for exploratory analysis

rm(list=ls(all=T))
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

#load the training and test sets
train<-read.csv('train.csv',header=T)
test<-read.csv('test.csv',header=T)

#check what percentage of passengers survived
prop.table(table(train$Survived))   #only 38.38% survived

#Assume everyone died?
test$Survived<-0
#submit<-data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
#write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
#Score=0.62679

#Survived histogram colored by Sex
qplot(Survived,data=train,fill=Sex) #shows that women were given priority

#Same histogram now faceted by Sex
qplot(Survived,data=train,facets=.~Sex) #same conclusion, much more clear

#Different representation
qplot(Sex,data=train,fill=as.factor(Survived)) #sex vs count filled by Survival

#Let's see the numbers already!
prop.table(table(train$Sex,train$Survived),1)
#74% of females survived but only 19% of males did!

#What if we instead assume that all males died and all females survived?
#Well, we expect the score to improve dramatically
#test$Survived[test$Sex=='female']=1
#Score=0.76555

summary(train$Age)
#this event was famous for giving children priority too, so let's check that
train$Child<-0
train$Child[train$Age<18]<-1

#Try to find if there is a relation between being a child and surviving
aggregate(Survived~Child+Sex,data=train,FUN=function(x){sum(x)/length(x)}) 
#no, still females survive more but no different if child or adult

#now try to look at pclass variable
aggregate(Survived~Pclass+Sex,data=train,FUN=function(x){sum(x)/length(x)})
#seems like in general people from pclass 1 > pclass 2 > pclass3 w.r.t. survivial rate

#equal width binning on fare
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

#maybe some females that paid a certain kind of fare had low a low survival rate?
aggregate(Survived~Fare2+Sex+Pclass,data=train,FUN=function(x){sum(x)/length(x)})
#looks like females of Pclass3 who paid high fares (>20) had a low survival rate

# test$Survived <- 0
# test$Survived[test$Sex == 'female'] <- 1
# test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
# Score=0.7799

#investigate this! One outlier? Maybe (s)he paid last minute high price
hist(train$Fare)

#Try Decision Tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train, method="class")
fancyRpartPlot(fit)

#Predict using DT model
Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)
#Score: 0.77990

#feature engineering
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)

combi$Title <- sapply(combi$Name, FUN=function(x) {
        strsplit(x, split='[,.]')[[1]][2]})

combi$Title <- sub(' ', '', combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                     Fare + Embarked + Title + FamilySize + FamilyID,
                        data=train, method="class")

#Predict using DT model
Prediction <- predict(fit, test, type = "class")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "feature_engg_1.csv", row.names = FALSE)
#Score: 0.79426




Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +
                            Fare + Embarked + Title + FamilySize +
                            FamilyID2, data=train, importance=TRUE, ntree=2000)
