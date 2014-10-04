##############################################
# Program: 1_Split_Data.R
#
#
#
#
#
#
# Authur: Dan Rogers
# Date:   Wed 07/27/2011 
##############################################

rm(list = ls(all = TRUE))
set.seed(8283)
setwd("C:\\Users\\Clark\\Documents\\Stats202\\Data\\ProjectOut\\Try2")
split <- .75

procfreq <- function(variable) {
	FREQ <- as.data.frame(table(variable))
	FREQ$PCT <- round(100*(FREQ$Freq/sum(FREQ$Freq)),digits=1)
	FREQ <- FREQ[order(-FREQ$PCT),]
	return(FREQ)
}

fulltrain <- read.csv("C:\\Users\\Clark\\Documents\\Stats202\\Data\\training.csv",header=T)
fulltrain <- fulltrain[order(fulltrain$query_id,fulltrain$url_id),]

final <- read.csv("C:\\Users\\Clark\\Documents\\Stats202\\Data\\test.csv",header=T)
final$index <- seq(1:dim(final)[1])
final <- final[order(final$query_id,final$url_id),]

fulltrain$query_num <- 0
fulltrain$query_num[1] <- 1
for (obs in 2:dim(fulltrain)[1]) {
    if (fulltrain$query_id[obs]==fulltrain$query_id[obs-1] && fulltrain$url_id[obs]==(fulltrain$url_id[obs-1]+1)) {
        fulltrain$query_num[obs] <- fulltrain$query_num[obs-1] + 1
    }
    else fulltrain$query_num[obs] <- 1
}

final$query_num <- 0
final$query_num[1] <- 1
for (obs in 2:dim(final)[1]) {
    if (final$query_id[obs]==final$query_id[obs-1] && final$url_id[obs]==(final$url_id[obs-1]+1)) {
        final$query_num[obs] <- final$query_num[obs-1] + 1
    }
    else final$query_num[obs] <- 1
}

# train is unique by query_id url_id
dupvalues <- fulltrain[duplicated(fulltrain[,1:2]),]
(nodupes <- dim(dupvalues)[1]==0)
query_ids <- data.frame(query_id=fulltrain[duplicated(fulltrain[,1])==FALSE,1])
query_ids$randy <- runif(dim(query_ids)[1])
query_ids$bin <- "train"
query_ids$bin[query_ids$randy>split] <- "test"

fulltrain <- merge(fulltrain,query_ids)
fulltrain.s <- split(fulltrain,fulltrain$bin)
train <- as.data.frame(fulltrain.s$train)
test <- as.data.frame(fulltrain.s$test)

procfreq(fulltrain$bin)
procfreq(train$bin)
procfreq(test$bin)

procfreq(train$relevance)
procfreq(test$relevance)
procfreq(final$relevance)

head(train)
head(test)
head(final)

vars <- as.list(names(train))

train <- train[-match("bin",vars)]
train <- train[-match("randy",vars)]
test <- test[-match("bin",vars)]
test <- test[-match("randy",vars)]

write.csv(train,file="train.csv")
write.csv(test,file="test.csv")
write.csv(final,file="final.csv")
