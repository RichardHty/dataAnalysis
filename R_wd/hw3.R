#setwd("~/Desktop/cs_spring18/cs555/R_wd")
raw_data<-read.csv("datasets/cs555_a3.csv", header = TRUE)
#get corresponding data, get rid of NA value
mealsWithFish<-raw_data[[1]]
mealsWithFish<-mealsWithFish[!is.na(mealsWithFish)]
mercuryInMg<-raw_data[[2]]
mercuryInMg<-mercuryInMg[!is.na(mercuryInMg)]
#create a scatter plot.
attach(raw_data)
min_x = min(mealsWithFish) 
max_x = max(mealsWithFish) 
min_y = min(mercuryInMg) 
max_y = max(mercuryInMg) 
plot(mealsWithFish, mercuryInMg,
     main="Number Of Meals Containing Fish vs. Mercury Level In Hair", 
     xlab="Number of meals containing fish", ylab="Mercury level in hair",
     xlim=c(min_x,max_x), ylim=c(min_y,max_y),
     pch=18, col="red", cex.lab=1.3)

#calculate correlation coefficient
r <- cor(mealsWithFish,mercuryInMg)
#calculate slope and intercept
sd_y <- sd(mercuryInMg)
sd_x <- sd(mealsWithFish)
yBar <- mean(mercuryInMg)
xBar <- mean(mealsWithFish)
beta_1 <- r*(sd_y/sd_x)
beta_0 <- yBar - beta_1 * xBar
m <- lm(mercuryInMg~mealsWithFish)
abline(m, lty=3, col="blue")
#F-statistic test
qf(0.9,df1=1,df2=98)
anova(m)
confint(m,level=0.9)
