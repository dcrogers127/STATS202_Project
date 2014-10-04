##############################################
# Program: 3_AdaBoost.R
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
library(rpart)

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

y <- train$relevance*2-1
x <- train[,match("query_length",vars):match("query_num",vars)]
x <- x[-match("relevance",as.list(names(x)))]

y_test <- test$relevance*2-1
x_test <- test[,match("query_length",vars):match("query_num",vars)]
x_test <- x_test[-match("relevance",as.list(names(x_test)))]

x_final <- final[match("query_length",vars):match("query_num",vars)]
x_final <- x_final[-match("index",as.list(names(x_final)))]

iter <- 50
train_error <- rep(0,iter)
test_error <- rep(0,iter)

f <- rep(0,dim(x)[1])
f_test <- rep(0,dim(x_test)[1])
f_final <- rep(0,dim(x_final)[1])

exp_train <- array(0,c(iter,2))
exp_test <- array(0,c(iter,2))

i <- 1
while (i<=iter) {

    w <- exp(-y*f) # This is a shortcut to compute w
    w <- w/sum(w)
    fit <- rpart(y~.,x,w,method="class") # 
    g <- -1+2*(predict(fit,x)[,2]>.5) # make -1 or 1
    g_test <- -1+2*(predict(fit,x_test)[,2]>.5)
    g_final <- -1+2*(predict(fit,x_final)[,2]>.5)
    e <- sum(w*(y*g<0))
    # if (e>=.5) {
        # f <- rep(0,dim(x)[1])
    # }
    # else {
        alpha <- .5*log( (1-e) / e )
        f <- f+alpha*g
        f_test <- f_test+alpha*g_test
        f_final <- f_final+alpha*g_final

        exp_train[i,1] <- i
        exp_train[i,2] <- log(sum(exp(-y*f)))

        exp_test[i,1] <- i
        exp_test[i,2] <- log(sum(exp(-y_test*f_test)))

        train_error[i] <- sum(1*f*y<0)/dim(x)[1]
        test_error[i] <- sum(1*f_test*y_test<0)/dim(x_test)[1]
        print(paste("train error for iter ",i," is: ",train_error[i]))
        print(e)
        i <- i+1
    # }
}
plot(exp_train[,1],exp_train[,2],type="l",
   main="Daniel Rogers' Natural Log of Exponential Loss",
   ylim=c(-25,25),xlim=c(0,iter),
   ylab="Log of Exponential Loss",xlab="Iterations",lwd=2)
points(exp_test[,1],exp_test[,2],type="l",
    col="red",lwd=2)
legend(20,iter-5,c("Train","Test"),   
    col=c("black","red"),lwd=2)

prediction <- as.numeric(f<0)
prediction_test <- as.numeric(f_test<0)
prediction_final <- as.numeric(f_final<0)

plot(seq(1,iter),test_error,type="l",
   main="Chart 6: AdaBoost Error Rate",ylim=c(0,.5),
   ylab="Error Rate",xlab="Iterations",lwd=2)
lines(train_error,lwd=2,col="red")
legend(4,.5,c("Training Error","Test Error"),   
    col=c("red","black"),lwd=2)


train.p <- cbind(train,prediction)
write.csv(train.p,file="train_adaboost.csv")

test.p <- cbind(test,prediction_test)
write.csv(test.p,file="test_adaboost.csv")

final.p <- cbind(final,prediction_final)
write.csv(final.p,file="final_adaboost.csv")

library(ada)
