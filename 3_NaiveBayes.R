##############################################
# Program: 3_NaiveBayes.R
#
#
#
#
#
#
# Authur: Dan Rogers
# Date:   Wed 08/03/2011 
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

y_test <- as.factor(test$relevance)
x_test <- test[,match("query_length",vars):match("query_num",vars)]
x_test <- x_test[-match("relevance",as.list(names(x_test)))]

x_final <- final[match("query_length",vars):match("query_num",vars)]
x_final <- x_final[-match("index",as.list(names(x_final)))]


means_1 <- rep(0,1,dim(x)[2])
stds_1 <- rep(0,1,dim(x)[2])
means_0 <- rep(0,1,dim(x)[2])
stds_0 <- rep(0,1,dim(x)[2])
for (i in 1:dim(x)[2]) {
    if (names(x)[i]!="is_homepage") {
        means_1[i] <- mean(x[y==1,i])
        stds_1[i] <- sd(x[y==1,i])
        means_0[i] <- mean(x[y==0,i])
        stds_0[i] <- sd(x[y==0,i])
    }
    else {
        p_11 <- sum(train$is_homepage==1 & train$relevance==1) / sum(train$relevance==1)  
        p_01 <- sum(train$is_homepage==0 & train$relevance==1) / sum(train$relevance==1) 
        p_10 <- sum(train$is_homepage==1 & train$relevance==0) / sum(train$relevance==0) 
        p_00 <- sum(train$is_homepage==0 & train$relevance==0) / sum(train$relevance==0) 
    }
}


train.v <- array(0,c(2,dim(x)[1],dim(x)[2]))
test.v <- array(0,c(2,dim(x_test)[1],dim(x_test)[2]))
final.v <- array(0,c(2,dim(x_final)[1],dim(x_final)[2]))
for (i in 1:dim(x)[2]) {
    if (names(x)[i]!="is_homepage") {
        train.v[1,,i] <- dnorm(x[,i],means_1[i],stds_1[i])
        train.v[2,,i] <- dnorm(x[,i],means_0[i],stds_0[i])
        test.v[1,,i] <- dnorm(x_test[,i],means_1[i],stds_1[i])
        test.v[2,,i] <- dnorm(x_test[,i],means_0[i],stds_0[i])
        final.v[1,,i] <- dnorm(x_final[,i],means_1[i],stds_1[i])
        final.v[2,,i] <- dnorm(x_final[,i],means_0[i],stds_0[i])
    }
    else {
        train.v[1,,i] <- p_11*as.numeric(x$is_homepage==1) + p_01*as.numeric(x$is_homepage==0)
        train.v[2,,i] <- p_10*as.numeric(x$is_homepage==1) + p_00*as.numeric(x$is_homepage==0)
        test.v[1,,i] <- p_11*as.numeric(x_test$is_homepage==1) + p_01*as.numeric(x_test$is_homepage==0)
        test.v[2,,i] <- p_10*as.numeric(x_test$is_homepage==1) + p_00*as.numeric(x_test$is_homepage==0)
        final.v[1,,i] <- p_11*as.numeric(x_final$is_homepage==1) + p_01*as.numeric(x_final$is_homepage==0)
        final.v[2,,i] <- p_10*as.numeric(x_final$is_homepage==1) + p_00*as.numeric(x_final$is_homepage==0)
    }
}


prediction <- as.numeric((sum(y==1)/dim(x)[1])*train.v[1,,1]*train.v[1,,2]*train.v[1,,3]*train.v[1,,4]*train.v[1,,5]*train.v[1,,6]*train.v[1,,7]*
    train.v[1,,8]*train.v[1,,9]*train.v[1,,10]*train.v[1,,11]>=(sum(y==0)/dim(x)[1])*train.v[2,,1]*train.v[2,,2]*train.v[2,,3]*train.v[2,,4]*train.v[2,,5]*
    train.v[2,,6]*train.v[2,,7]*train.v[2,,8]*train.v[2,,9]*train.v[2,,10]*train.v[2,,11]) 
1-sum(y==prediction)/length(y)
train.p <- cbind(train,prediction)
write.csv(train.p,file="train_naivebayes.csv")
# [1] 0.3686008

prediction_test <- as.numeric((sum(y==1)/dim(x)[1])*test.v[1,,1]*test.v[1,,2]*test.v[1,,3]*test.v[1,,4]*test.v[1,,5]*test.v[1,,6]*test.v[1,,7]*
    test.v[1,,8]*test.v[1,,9]*test.v[1,,10]*test.v[1,,11]>=(sum(y==0)/dim(x)[1])*test.v[2,,1]*test.v[2,,2]*test.v[2,,3]*test.v[2,,4]*test.v[2,,5]*
    test.v[2,,6]*test.v[2,,7]*test.v[2,,8]*test.v[2,,9]*test.v[2,,10]*test.v[2,,11]) 
1-sum(y_test==prediction_test)/length(y_test)
test.p <- cbind(test,prediction_test)
write.csv(test.p,file="test_naivebayes.csv")
# [1] 0.3646036

prediction_final <- as.numeric((sum(y==1)/dim(x)[1])*final.v[1,,1]*final.v[1,,2]*final.v[1,,3]*final.v[1,,4]*final.v[1,,5]*final.v[1,,6]*final.v[1,,7]*
    final.v[1,,8]*final.v[1,,9]*final.v[1,,10]*final.v[1,,11]>=(sum(y==0)/dim(x)[1])*final.v[2,,1]*final.v[2,,2]*final.v[2,,3]*final.v[2,,4]*final.v[2,,5]*
    final.v[2,,6]*final.v[2,,7]*final.v[2,,8]*final.v[2,,9]*final.v[2,,10]*final.v[2,,11]) 
final.p <- cbind(final,prediction_final)
write.csv(final.p,file="final_naivebayes.csv")

fit <- naiveBayes(x,y)
prediction <- predict(fit,x)
1-sum(y==prediction)/length(y)
# [1] 0.3696003

prediction_test <- predict(fit,x_test)
1-sum(y_test==prediction_test)/length(y_test)
# [1] 0.3661022 
