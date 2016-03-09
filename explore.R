## 0 - "Satisfied"
## 1 - "Unsatisfied"
roc.curve <- function(p, l, cutoff=quantile(p, seq(0,1,by=0.2)), ...) {
  ## Make a ROC plot.
  ## code copied from http://stat.fsu.edu/~fchen/model-selection.pdf
  ## Wrapper to plot ROC curve by using R package ROCR
  ## "p" is a vector of prediction (continuous);
  ## "l" is a vector of truth (0/"negative" or 1/"positive")
  ## "cutoff" is a list of values to be plotted on ROC curve.
  ## "..." is a passed to "plot".
  ## RETURN: a list of ROC curve coordinates and cut-offs.
  par(pty="s")
  require(ROCR)
  a <- prediction(p, l)
  ## ROC for Sensitivity vs. Specificity.
  plot((pp <- performance(a, "sens", "spec")), colorize=T,
       print.cutoffs.at=cutoff, text.adj=c(1.2, 1.2), text.cex=0.7, lwd=2, las=1,
       ...)
  grid(col="orange")
  ##Draw a "line of no-discrimination".
  ## Sens = P(X=+ | T=+), Spec = P(X=- | T=-),
  ## if X is independent of T, then Sens + Spec = P(X+)+P(X-) = 1, so the pair
  ## (Sens, Spec) lies on a diagonal line.
  abline(c(1, -1), col="gray70", lty=2)
  par(pty="m")
  invisible(pp)
}


library(mlbench)
library(Amelia)
library(caret)
library(e1071)
library(ggplot2)
library(klaR)
library(pROC)
library(ROCR)

set.seed(12345)


#explore Train
train <- read.csv("train.csv", header = TRUE)
checkNA <- complete.cases(train)
checkNA
clsTrain <- sapply(train, class)
clsTrain
missmap(train, col=c("black", "grey"), legend=FALSE)


#explore test\
test <- read.csv("test.csv", header = TRUE)
checkNA <- complete.cases(test)
checkNA
clsTest <- sapply(test, class)
clsTest
missmap(test, col=c("black", "grey"), legend=FALSE)

## missingness map confirms that both train and test datasets dont have any missing values.
id <- train$ID
target <- train$TARGET
exVar <- names(train) %in% c("ID","TARGET")
prtrain <- train[!exVar]
##identify variables that are important for analysis
##step 1 - remove zero variances
preprocessParams <- preProcess(prtrain, method=c("zv"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
xftrain1 <- predict(preprocessParams, prtrain)
# summarize the transformed dataset
summary(xftrain1)

##step 2 - remove zero variances
preprocessParams <- preProcess(xftrain1, method=c("pca"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
xftrain2 <- predict(preprocessParams, xftrain1)
# summarize the transformed dataset
summary(xftrain2)

##PCA transformation has fetched 101 variables
##step 3 check skewness of data
skew <- apply(xftrain2, 2, skewness)
# display skewness, larger/smaller deviations from 0 show more skew
print(skew)
# Data seems to completely skewed

##step 4 - Yeo-Johnson transformation as data is negative
preprocessParams <- preProcess(xftrain2, method=c("YeoJohnson"))
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
xftrain3 <- predict(preprocessParams, xftrain2)
# summarize the transformed dataset
summary(xftrain3)

##step 5 check skewness again
skew <- apply(xftrain3, 2, skewness)
# display skewness, larger/smaller deviations from 0 show more skew
print(skew)
# Data seems to have normalized now


##prepare data for modelling
trn.full <- cbind(id, xftrain3, target)
trn.full$target <- as.factor(ifelse(trn.full$target==0,0,1))
smp_size <- floor(0.70 * nrow(trn.full))

## set the seed to make your partition reproductible

train_ind <- sample(seq_len(nrow(trn.full)), size = smp_size)

trn.learn <- trn.full[train_ind, ]
trn.test <- trn.full[-train_ind, ]



##generalized linear model
trainControl <- trainControl(method="cv", number=5, classProbs = TRUE, summaryFunction=twoClassSummary)
fit <- train(target~., data=trn.learn, method="glm", metric="ROC",
             trControl=trainControl)
# display results
print(fit)


#naive bayes <not working yet>
fit.nb <- naiveBayes(target~., data=trn.learn)
# summarize the fit
print(fit.nb)
# make predictions
pred.nb <- predict(fit.nb, trn.test[,-target])
# summarize accuracy
table(pred.nb, trn.learn$target)
roc.curve(pred.nb, as.integer(trn.full$target), main="ROC Curve for Prediction of target: Training Sample")