##############################################
# Program: 3_KNN.R.R
#
#
#
#
#
#
# Authur: Dan Rogers
# Date:   Mon 08/01/2011 
##############################################

rm(list = ls(all = TRUE))
setwd("C:\\Users\\Clark\\Documents\\Stats202\\Data\\ProjectOut\\Try2")
library(class)

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

for (i in 3:6) {
    train[,match(paste("sig",as.character(i),sep=""),vars)] <- log(train[,match(paste("sig",as.character(i),sep=""),vars)]+1)
    test[,match(paste("sig",as.character(i),sep=""),vars)] <- log(test[,match(paste("sig",as.character(i),sep=""),vars)]+1)
    final[,match(paste("sig",as.character(i),sep=""),fvars)] <- log(final[,match(paste("sig",as.character(i),sep=""),fvars)]+1)
}

y <- as.factor(train$relevance)
x <- train[,match("query_length",vars):match("query_num",vars)]
x <- x[,-match("relevance",as.list(names(x)))]

y_test <- as.factor(test$relevance)
x_test <- test[,match("query_length",vars):match("query_num",vars)]
x_test <- x_test[,-match("relevance",as.list(names(x_test)))]

y_final <- as.factor(final$relevance)
x_final <- final[,match("query_length",vars):match("query_num",vars)]
x_final <- x_final[,-match("index",as.list(names(x_final)))]

# make nknn odd
nknn <- 149
error.rate <- array(0,c((nknn+1)/2,3))
setwd("C:\\Users\\Clark\\Documents\\Stats202\\Data\\ProjectOut\\Try2\\KNN")
k <- 105
fit <- knn(x,x,y,k=k)
(error.ratet <- 1-sum(y==fit)/length(y))
train.p <- cbind(train,fit)
# write.csv(train.p,file=paste("train_knn_",k,".csv",sep=""))

fit_test <- knn(x,x_test,y,k=k)
(error.ratetest <- 1-sum(y_test==fit_test)/length(y_test))
test.p <- cbind(test,fit_test)
# write.csv(test.p,file=paste("test_knn_",k,".csv",sep=""))

fit_final <- knn(x,x_final,y,k=k)
final.p <- cbind(final,fit_final)
# write.csv(final.p,file=paste("final_knn_",k,".csv",sep=""))
# write.csv(error.rate,file="knn_error_rate.csv")


# no normalization
# [1] "Training error with knn=1"
# [1] 0
# [1] "Testing error with knn=1"
# [1] 0.4330386
