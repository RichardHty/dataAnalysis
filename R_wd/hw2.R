#setwd("~/Desktop/cs_spring18/cs555/R_wd")
raw_data<-read.csv("datasets/cs555_a2.csv", header = FALSE)/Users/richard/Desktop/cs_spring18/cs555/R_wd/cs555_TianyouHuang_hw2.R
#get corresponding data, get rid of NA value
participate<-raw_data[[1]]
nonParticipate<-raw_data[[2]]
nonParticipate<-nonParticipate[!is.na(nonParticipate)]

#create a summary vector for "participate" and add standard deviation into it.
summary_participate <- summary(participate)
standard_deviation <- sd(participate)
summary_data_participate <- c(summary_participate,standard_deviation)
print(summary_data_participate)

#create a summary vector for "non-participate" and add standard deviation into it.
summary_nonParticipate <- summary(nonParticipate)
standard_deviation <- sd(nonParticipate)
summary_data_nonParticipate <- c(summary_nonParticipate,standard_deviation)
print(summary_data_nonParticipate)

#use a data frame to structure the result
summary_result <- data.frame(summary_data_participate,summary_data_nonParticipate,
                             row.names = c("minimum","first quartiles","median","mean","third quartiles","maximum","standard deviation")
                            )
#save into a .csv file
write.csv(summary_result,"summaryForA2.csv")

#generate a histgram
hist(participate,breaks = 15,labels = TRUE,
     main = "Participate vs. Non-participate", 
     xlab = "Intake calories",col = rgb(0,1,0,0.5))
hist(nonParticipate,breaks = 15,labels = TRUE,
     col = rgb(1,0,0,0.5) ,add = T)
legend(550,8, c("participate", "non-participate"),
       fill=c(col = rgb(0,1,0,0.5), rgb(1,0,0,0.5)), horiz =FALSE, cex=0.5)

#question 2 p-value
pt(0.302721,24)
#question 4 p-value
pt(2.8248,21)
#question 5 boxplot
boxplot(participate,col = rgb(0,1,0,0.5))
boxplot(nonParticipate,col = rgb(1,0,0,0.5), add = T)
