training <- read.csv("train.csv", stringsAsFactors = FALSE)
require(MASS)
require(rpart)

### Very simple model
#sexclass <- glm(Survived~Sex*as.factor(Pclass), data=training, family=binomial)
#sexclass.sim <- glm(Survived~Sex+as.factor(Pclass), data=training, family=binomial)

sexclassrpart <- rpart(Survived~Sex+as.factor(Pclass)+Age, data=training)


#sum((fitted(sexclass) > 0.5) ==  (training$Survived==1)) / nrow(training) ### training set accuracy

sum((predict(sexclassrpart) > 0.5) ==  (training$Survived==1)) / nrow(training) ### training set accuracy



### what actually matter is test set accuracy

testset <- read.csv("test.csv", stringsAsFactors = FALSE)

testPredicted <- predict(sexclassrpart, testset)


submission <- data.frame(PassengerId = testset$PassengerId, Survived = as.numeric((testPredicted > 0.5)))

write.csv(submission, file="submissionrpart.csv", row.names = FALSE)

### submit that csv file to kaggle

