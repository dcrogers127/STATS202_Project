
rm(list = ls(all = TRUE))
setwd("C:\\Users\\Clark\\Documents\\Stats202\\Data\\ProjectOut\\Try2")

procfreq <- function(variable) {
	FREQ <- as.data.frame(table(variable))
	FREQ$PCT <- round(100*(FREQ$Freq/sum(FREQ$Freq)),digits=1)
	FREQ <- FREQ[order(-FREQ$PCT),]
	return(FREQ)
}

train <- read.csv(file="train_svm.csv",header=T)
test <- read.csv(file="test_svm.csv",header=T)
final <- read.csv(file="final_svm.csv",header=T)

final <- final[order(final$index),]

1-sum(train$relevance==train$prediction)/length(train$relevance)
1-sum(test$relevance==test$prediction)/length(test$relevance)

procfreq(train$prediction)
procfreq(test$prediction)
procfreq(final$prediction_final)


f <- final$prediction_final
t <- final$index
write.table(f,file="final_project_prediction.txt",row.names=FALSE,col.names=FALSE);
write.table(t,file="test.txt",row.names=FALSE,col.names=FALSE);
