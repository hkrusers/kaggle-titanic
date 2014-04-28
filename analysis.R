raw <- read.csv("train.csv", stringsAsFactors = FALSE)

### preprocess

require(stringr)
extractTitle <- function(x) {
    res <- strsplit(x, ",")
    res2 <- strsplit(res[[1]][2], "\\.")
    return(str_trim(tolower(res2[[1]][1])))
}

preprocess <- function(x, test = FALSE) {
    if (!test) {
    x$die <- factor(x$Survived)
}
    x$classF <- factor(x$Pclass)
    x$gender <- factor(x$Sex)
    x$Age[is.na(x$Age)] <- mean(x$Age, na.rm = TRUE)
    x$single <- factor(x$SibSp == 0 & x$Parch == 0)
    x$EmC <- factor(x$Embarked == "C")
    x$title <- sapply(x$Name, extractTitle)
    x$mrs <- factor(x$title == "mrs")
    x$miss <- factor(x$title == "miss")
    x$master <- factor(x$title == "master")
    return(x)
}


train <- preprocess(raw)
require(randomForest)
rf.fit <- randomForest(die~gender+classF+Age+Fare, data=train, ntree=10000, importance = TRUE)
rf.fit2 <- randomForest(die~gender+classF+Age+Fare+mrs+miss+master, data=train, ntree=10000, importance = TRUE)

varImpPlot(rf.fit)
sum(predict(rf.fit) == train$die) / nrow(train)
varImpPlot(rf.fit2)
sum(predict(rf.fit2) == train$die) / nrow(train)

table(sapply(train$Name, extractTitle), train$die)

### what actually matter is test set accuracy

testraw <- read.csv("test.csv", stringsAsFactors = FALSE)
testset <- preprocess(testraw, test = TRUE)

testsetSur <- as.numeric(predict(rf.fit, testset)) - 1
testsetSur[is.na(testsetSur)] <- 0

write.csv(data.frame(PassengerId = testset$PassengerId, Survived = testsetSur), file = "submission2.csv", row.names = FALSE)
