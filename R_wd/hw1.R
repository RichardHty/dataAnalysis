#setwd("~/Desktop/cs_spring18/cs555/R_wd")
duration<-read.csv("datasets/cs555_a1.csv", header = FALSE)
#change data type into a vector
days<-unlist(duration, use.names=FALSE)

#make sure the range is just right
range_days <- max(days) - min(days) + 1

#generate a histgram
hist(days,range_days,labels = TRUE,
     main = "Histogram of Days", 
     xlab = "duration")
grid(nx=NA,ny=NULL,lty=1,lwd=1)

#generate a density histgram
hist(days,range_days,labels = TRUE,prob=TRUE,
     main = "Distribution of Duration of Days", 
     xlab = "duration")
lines(density(days),col="red")

#generate a boxplot 
boxplot(days)

#create a summary vector and add standard deviation into it.
summary_data <- summary(days)
standard_deviation <- sd(days)
summary_data <- c(summary_data,standard_deviation)
print(summary_data)
#save into a .csv file
names(summary_data) <- c("minimum","first quartiles","median","mean","third quartiles","maximum","standard deviation")
write.csv(summary_data,"summaryForA1.csv")

population_mean = 5
population_sd = 3
number_of_sample = 10

percentage_less_than_a_week <- pnorm(7,mean=population_mean,sd=population_sd)
cat(percentage_less_than_a_week*100 , "%")

sampleMean_greater_than_7 <- 1-pnorm(
  (7-population_mean)/(population_sd/sqrt(number_of_sample)))
print(sampleMean_greater_than_7)


