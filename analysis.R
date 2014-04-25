training <- read.csv("train.csv", stringsAsFactors = FALSE)
require(MASS)

### Very simple model
sexclass <- glm(Survived~Sex*as.factor(Pclass), data=training, family=binomial)
sexclass.sim <- glm(Survived~Sex+as.factor(Pclass), data=training, family=binomial)


sum((fitted(sexclass) > 0.5) ==  (training$Survived==1)) / nrow(training) ### training set accuracy

### what actually matter is test set accuracy

testset <- read.csv("test.csv", stringsAsFactors = FALSE)

testPredicted <- predict(sexclass, testset, "response")


submission <- data.frame(PassengerId = testset$PassengerId, Survived = as.numeric((testPredicted > 0.5)))

write.csv(submission, file="submission.csv", row.names = FALSE)

### submit that csv file to kaggle
