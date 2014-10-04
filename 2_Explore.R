##############################################
# Program: 2_Visualize.R
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

procfreq <- function(variable) {
	FREQ <- as.data.frame(table(variable))
	FREQ$PCT <- round(100*(FREQ$Freq/sum(FREQ$Freq)),digits=1)
	FREQ <- FREQ[order(-FREQ$PCT),]
	return(FREQ)
}

training <- read.csv("C:\\Users\\Clark\\Documents\\Stats202\\Data\\training.csv",header=T)
testing <- read.csv("C:\\Users\\Clark\\Documents\\Stats202\\Data\\test.csv",header=T)

procfreq(testing$query_length)
hist(testing$query_length,breaks=seq(1,16,by=1),
    col="blue",
    xlab="query_length", ylab="Frequency",
    main="Chart 1: Query_length Histogram")

plot(testing$sig1,testing$sig2)

