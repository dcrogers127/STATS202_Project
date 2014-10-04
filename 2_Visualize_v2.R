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
setwd("C:\\Users\\Clark\\Documents\\Stats202\\Data\\ProjectOut")
library(ggplot2)

procfreq <- function(variable) {
	FREQ <- as.data.frame(table(variable))
	FREQ$PCT <- round(100*(FREQ$Freq/sum(FREQ$Freq)),digits=1)
	FREQ <- FREQ[order(-FREQ$PCT),]
	return(FREQ)
}

train <- read.csv("train.csv",header=T)
test <- read.csv("test.csv",header=T)
all <- rbind(train,test)


all$Relevance <- "b) Not Relevant"
all$Relevance[all$relevance==1] <- "a) Relevant"
all$is_homepage <- as.factor(all$is_homepage)

library(gridExtra)
plot1 <- ggplot(all)+geom_histogram(aes(x=query_length,fill=Relevance),binwidth=1)+opts(title="Chart 1a: Stacked Histogram for query_length")+ylab("Frequency")
plot2 <- ggplot(all)+geom_histogram(aes(x=is_homepage,fill=Relevance),binwidth=.5)+opts(title="Chart 1b: Stacked Histogram for is_homebage")+ylab("Frequency")
plot3 <- ggplot(all)+geom_histogram(aes(x=sig1,fill=Relevance),binwidth=.05)+opts(title="Chart 1c: Stacked Histogram for sig1")+ylab("Frequency")
plot4 <- ggplot(all)+geom_histogram(aes(x=sig2,fill=Relevance),binwidth=.05)+opts(title="Chart 1c: Stacked Histogram for sig2")+ylab("Frequency")
plot5 <- ggplot(all)+geom_histogram(aes(x=sig7,fill=Relevance),binwidth=.05)+opts(title="Chart 1d: Stacked Histogram for sig7")+ylab("Frequency")
all$sig8 <- all$sig8+.01
plot6 <- ggplot(all)+geom_histogram(aes(x=sig8,fill=Relevance),binwidth=.03)+opts(title="Chart 1e: Stacked Histogram for sig8")+ylab("Frequency")
all$sig8 <- all$sig8-.01
sidebysideplot <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2, nrow=3)


# qplot(sig1, data = all, geom = "density", colour = Relevance,adjust=1)
# qplot(sig2, data = all, geom = "density", colour = Relevance,adjust=1)
# qplot(sig7, data = all, geom = "density", colour = Relevance,adjust=1)
# qplot(sig8, data = all, geom = "density", colour = Relevance,adjust=1)

# 3a Plot histogram for CA house prices
# breaks=seq(0,3500000,by=500000),
vars <- as.list(names(all))
logs <- array(0,c(dim(all)[1],4))
for (i in 3:6) {
    logs[,i-2] <- log(all[,match(paste("sig",as.character(i),sep=""),vars)]+1)
}
par(mfrow=c(4,2))
letter <- c('a','b','c','d')
for (i in 3:6) {
    hist(all[,match(paste("sig",as.character(i),sep=""),vars)],col="blue",
        xlab=paste("sig",i,sep=""), ylab="Frequency",
        main=paste("Chart 2",letter[i-2],": Histogram for sig",i,sep=""))
}
for (i in 3:6) {
    hist(logs[,i-2],col="blue",
        xlab=paste("ln(sig",i,"+1)",sep=""), ylab="Frequency",
        main=paste("Chart 3",letter[i-2],": Histogram for ln(sig",i,"+1)",sep=""))
}

train2 <- read.csv("C:\\Users\\Clark\\Documents\\Stats202\\Data\\ProjectOut\\Try2\\train.csv",header=T)
test2 <- read.csv("C:\\Users\\Clark\\Documents\\Stats202\\Data\\ProjectOut\\Try2\\test.csv",header=T)
all2 <- rbind(train2,test2)
all2$Relevance <- "b) Not Relevant"
all2$Relevance[all$relevance==1] <- "a) Relevant"

plot9 <- ggplot(all)+geom_histogram(aes(x=query_num,fill=Relevance),binwidth=1)+opts(title="Chart 4a: query_num Method 1")+ylab("Frequency")
plot10 <- ggplot(all2)+geom_histogram(aes(x=query_num,fill=Relevance),binwidth=1)+opts(title="Chart 4b: query_num Method 2")+ylab("Frequency")
plot11 <- qplot(query_num, data = all, geom = "density", colour = Relevance,adjust=3)+opts(title="Chart 4a: Stacked Histogram for query_num v1")
plot12 <- qplot(query_num, data = all2, geom = "density", colour = Relevance,adjust=3)+opts(title="Chart 4a: Stacked Histogram for query_num v1")
sidebysideplot <- grid.arrange(plot9, plot10, ncol=2, nrow=1)


# isrelevant <- all[all$relevance==1,]
# notrelevant <- all[all$relevance==0,]
# 
# vars <- as.list(names(isrelevant))
# for (i in 3:6) {
#     isrelevant[,match(paste("sig",as.character(i),sep=""),vars)] <- log(isrelevant[,match(paste("sig",as.character(i),sep=""),vars)]+1)
#     notrelevant[,match(paste("sig",as.character(i),sep=""),vars)] <- log(notrelevant[,match(paste("sig",as.character(i),sep=""),vars)]+1)
# }

# i <- 6
# plot(ecdf(isrelevant[,match(paste("sig",as.character(i),sep=""),vars)]),
#   verticals= TRUE,
#   do.p = FALSE, 
#   main =paste("sig",i," ECDF",sep=""),
#   xlab=paste("sig",i,sep=""),
#   ylab="Cumulative Percent")
# lines(ecdf(notrelevant[,match(paste("sig",as.character(i),sep=""),vars)]),
#   verticals= TRUE,do.p = FALSE,
#   col.h="red",col.v="red",lwd=1)
# legend(.3,.3,c("relevant","not relevant"),
#   col=c("black","red"),lwd=c(1,1))


# qplot(sig1, data = all, geom = "density", colour = Relevance,adjust=1)
