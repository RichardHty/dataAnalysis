#setwd("~/Desktop/cs_spring18/cs555/R_wd")
raw_data<-read.csv("datasets/cs555_a5.csv", header = TRUE)
#get corresponding data, get rid of NA value
groupFactor<-factor(raw_data[[1]])
summary(groupFactor)

physics<-raw_data[raw_data$group == 'Physics student',]
mathStudent<-raw_data[raw_data$group == 'Math student',]
chemistry<-raw_data[raw_data$group == 'Chemistry student',]
plot(physics$iq, physics$age,
     main="physcis student iq vs age", 
     xlab="iq", ylab="age",
     pch=18, col="red", cex.lab=1.3)
summary(physics)
plot(mathStudent$iq, mathStudent$age,
     main="math student iq vs age", 
     xlab="iq", ylab="age",
     pch=18, col="red", cex.lab=1.3)
summary(mathStudent)
plot(chemistry$iq,chemistry$age,
     main="chemistry student iq vs age", 
     xlab="iq", ylab="age",
     pch=18, col="red", cex.lab=1.3)
summary(chemistry)
par(mfrow=c(1,3))
boxplot(physics$iq,physics$age,xlab="category", ylab="value",main="physics student")
boxplot(mathStudent$iq,mathStudent$age,xlab="category", ylab="value",main="math student")
boxplot(chemistry$iq,chemistry$age,xlab="category", ylab="value",main="chemistry student")

iq<-raw_data[[2]]
age<-raw_data[[3]]

m_iq<-aov(iq~groupFactor,data=raw_data)
summary(m_iq)
plot(groupFactor, iq,
     main="Different Group vs Test Score", 
     xlab="group", ylab="Test Score",
     pch=18, col="red", cex.lab=1.3)

plot(groupFactor, age,
     main="Different Group vs Age", 
     xlab="group", ylab="Age",
     pch=18, col="red", cex.lab=1.3)


qf(0.95,df1 = 2,df2 = 42)
TukeyHSD(m_iq)

dummy_g1 <- ifelse(groupFactor=='Math student', 1, 0) 
dummy_g2 <- ifelse(groupFactor=='Physics student', 1, 0)
m2 <- lm(iq~dummy_g1+dummy_g2, data=raw_data) 
summary(m2)


install.packages('car')
library(car)
Anova(lm(iq~groupFactor+age), type=3)

install.packages('lsmeans')
library(lsmeans)
options(contrasts=c("contr.treatment", "contr.poly"))
lsmeans(lm(iq~groupFactor+age), pairwise~groupFactor, adjust="tukey")

