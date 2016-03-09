##model exploration
set.seed(12345)
library(mlbench)
library(Amelia)
library(caret)
library(e1071)
library(ggplot2)
library(klaR)
library(pROC)
library(ROCR)

#explore Train
train <- read.csv("train.csv", header = TRUE)
#checkNA <- complete.cases(train)
#checkNA
#clsTrain <- sapply(train, class)
#clsTrain
#missmap(train, col=c("black", "grey"), legend=FALSE)
train$TARGET <- as.factor(ifelse(train$TARGET==0,0,1))

#explore test\
test <- read.csv("test.csv", header = TRUE)
#checkNA <- complete.cases(test)
#checkNA
#clsTest <- sapply(test, class)
#clsTest
#missmap(test, col=c("black", "grey"), legend=FALSE)



trainControl <- trainControl(method="repeatedcv", number=5, repeats=1)
metric <- "Accuracy"
#GLM
fit.glm <- train(TARGET~., data=train, method="glm", metric=metric, trControl=trainControl)

# LDA
fit.lda <- train(TARGET~., data=train, method="lda", metric=metric, trControl=trainControl)

# GLMNET
fit.glmnet <- train(TARGET~., data=train, method="glmnet", metric=metric,
                    trControl=trainControl)
# KNN
fit.knn <- train(target~., data=trn.full, method="knn", metric=metric, trControl=trainControl)

# CART
fit.cart <- train(target~., data=trn.full, method="rpart", metric=metric,
                  trControl=trainControl)

# Naive Bayes
fit.nb <- train(target~., data=trn.full, method="nb", metric=metric, trControl=trainControl)

# SVM
fit.svm <- train(target~., data=trn.full, method="svmRadial", metric=metric,
                 trControl=trainControl)

results <- resamples(list(LG=fit.glm, LDA=fit.lda, GLMNET=fit.glmnet, KNN=fit.knn,
                          CART=fit.cart, NB=fit.nb, SVM=fit.svm))
summary(results)
dotplot(results)
