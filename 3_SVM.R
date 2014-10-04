##############################################
# Program: 3_SVM.R.R
#
#
#
#
#
#
# Authur: Dan Rogers
# Date:   Sun 07/31/2011 
##############################################

rm(list = ls(all = TRUE))
setwd("C:\\Users\\Clark\\Documents\\Stats202\\Data\\ProjectOut\\Try2")
library(e1071)

procfreq <- function(variable) {
	FREQ <- as.data.frame(table(variable))
	FREQ$PCT <- round(100*(FREQ$Freq/sum(FREQ$Freq)),digits=1)
	FREQ <- FREQ[order(-FREQ$PCT),]
	return(FREQ)
}

train <- read.csv("train.csv",header=T)
test <- read.csv("test.csv",header=T)
final <- read.csv("final.csv",header=T)

vars <- as.list(names(train))
fvars <- as.list(names(final))

# transform certain variables by adding 1 then taking the log
for (i in 3:6) {
    train[,match(paste("sig",as.character(i),sep=""),vars)] <- log(train[,match(paste("sig",as.character(i),sep=""),vars)]+1)
    test[,match(paste("sig",as.character(i),sep=""),vars)] <- log(test[,match(paste("sig",as.character(i),sep=""),vars)]+1)
    final[,match(paste("sig",as.character(i),sep=""),fvars)] <- log(final[,match(paste("sig",as.character(i),sep=""),fvars)]+1)
}

y <- as.factor(train$relevance)
x <- train[,match("query_length",vars):match("query_num",vars)]
x <- x[-match("relevance",as.list(names(x)))]

fit <- svm(x,y,cost=2)

prediction <- predict(fit,x) 
1-sum(y==prediction)/length(y)
train.p <- cbind(train,prediction)
write.csv(train.p,file="train_svm.csv")
# cost=5  [1] 0.3142086
# cost=4  [1] 0.3155913
# cost=3  [1] 0.3173905
# cost=2  [1] 0.3194229
#         [1] 0.3220884
# cost=.5 [1] 0.3250204
# .329 w/o sig8

y_test <- as.factor(test$relevance)
x_test <- test[,match("query_length",vars):match("query_num",vars)]
x_test <- x_test[-match("relevance",as.list(names(x_test)))]
prediction_test <- predict(fit,x_test) 
1-sum(y_test==prediction_test)/length(y_test)
test.p <- cbind(test,prediction_test)
write.csv(test.p,file="test_svm.csv")
# cost=5 [1]  0.3275388
# cost=4 [1]  0.3267396 
# cost=3 [1]  0.3271392 
# cost=2 [1]  0.3261901
# [        1] 0.3268395
# cost=.5 [1] 0.328438

x_final <- final[match("query_length",vars):match("query_num",vars)]
x_final <- x_final[-match("index",as.list(names(x_final)))]
prediction_final <- predict(fit,x_final) 
final.p <- cbind(final,prediction_final)
write.csv(final.p,file="final_svm.csv")

# svmpath seems to take up too much memory
library(kernlab)
t <- as.matrix(x[1:200,])
ty <- 2*as.numeric(y[1:200])-3
filter <- ksvm(t~.,data=ty)

y <- as.factor(train$relevance)
y_test <- as.factor(test$relevance)
# x <- train[,match("query_length",vars):match("query_num",vars)]
# x <- x[-match("relevance",as.list(names(x)))]
# x <- as.matrix(x)

fit <- ksvm(y~.,data=x,C=2,kernel="splinedot")

prediction <- predict(fit,x) 
1-sum(y==prediction)/length(y)
# basic [1] 0.3242374

prediction_test <- predict(fit,x_test) 
1-sum(y_test==prediction_test)/length(y_test)
# basic [1] 0.3272891

