#setwd("~/Desktop/cs_spring18/cs555/R_wd")
raw_data<-read.csv("datasets/cs555_a4.csv", header = TRUE)
#get corresponding data, get rid of NA value
prestigeScore<-raw_data[[5]]
prestigeScore<-prestigeScore[!is.na(prestigeScore)]
educationYears<-raw_data[[2]]
educationYears<-educationYears[!is.na(educationYears)]
#create a scatter plot.
attach(raw_data)
min_x = min(educationYears) 
max_x = max(educationYears) 
min_y = min(prestigeScore) 
max_y = max(prestigeScore)
plot(educationYears, prestigeScore,
     main="Education Level vs. Prestige Score", 
     xlab="Education Level", ylab="Prestige Score",
     xlim=c(min_x,max_x), ylim=c(min_y,max_y),
     pch=18, col="red", cex.lab=1.3)
#calculate correlation coefficient
r <- cor(educationYears,prestigeScore)
#simple linear regression
par(mfrow=c(1,2))
m <- lm(prestigeScore~educationYears)
abline(m, col="blue")
resid(m)
plot(educationYears, resid(m), axes=TRUE, 
     frame.plot=TRUE,pch=18, col="red", cex.lab=1.3,
     xlab='Education Level', ylab='Residue') 
abline(h=0,col = 'blue')
plot(fitted(m), resid(m), axes=TRUE,
     frame.plot=TRUE, pch=18, col="red", cex.lab=1.3,
     xlab='fitted values', ylab='residue')
abline(h=0,col = 'blue')
hist(resid(m))
boxplot(educationYears,col = rgb(0,1,0,0.5))
boxplot(resid(m),col = rgb(1,0,0,0.5))

#multiple linear regression
income<-raw_data[[3]]
income<-income[!is.na(income)]
percentageOfWomen<-raw_data[[4]]
percentageOfWomen<-percentageOfWomen[!is.na(percentageOfWomen)]
m2<-lm(prestigeScore~educationYears+income+percentageOfWomen)
summary(m2)
anova(m2)
#plot fitted value
plot(fitted(m2), resid(m2), axes=TRUE, 
     frame.plot=TRUE, xlab='fitted values', ylab='residue')
abline(h=0,col = 'blue')
