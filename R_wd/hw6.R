#setwd("~/Desktop/cs_spring18/cs555/R_wd")
raw_data<-read.csv("datasets/cs555_a6.csv", header = TRUE)
#create temp_level variable
temp_level<-ifelse(raw_data$temp >= 98.6, 1, 0)
#group data
male_temp<-raw_data[raw_data$sex == '1',]
female_temp<-raw_data[raw_data$sex == '2',]
male_temp_level <- ifelse(male_temp$temp >= 98.6,1,0)
female_temp_level <- ifelse(female_temp$temp >= 98.6,1,0)

male_sample_size <- length(male_temp_level)
male_high_temp_size<- sum(male_temp_level)
male_high_temp_proportion<- male_high_temp_size/ male_sample_size

female_sample_size <- length(female_temp_level)
female_high_temp_size<- sum(female_temp_level)
female_high_temp_proportion<- female_high_temp_size/ female_sample_size
#risk difference
rd<-male_high_temp_proportion-female_high_temp_proportion
#logistic regression
m<-glm(temp_level ~ raw_data$sex, family=binomial)
summary(m)
or<-(male_high_temp_proportion/(1-male_high_temp_proportion))/(female_high_temp_proportion/(1-female_high_temp_proportion))
#multiple logistic regression
m2<-glm(temp_level ~raw_data$sex+raw_data$Heart.rate,family = binomial)
summary(m2)
#todo change the way to calculate or


or_sex<-exp(1.38919)
or_heart_rate<-exp(0.06337)
#install.packages('pROC')
library(pROC) 
m_prob <- predict(m, type=c("response"))
m_g<-roc(temp_level~m_prob)
m_c_statistic<-auc(m_g)

m2_prob <- predict(m2, type=c("response"))
m2_g<-roc(temp_level~m2_prob)
m2_c_statistic<-auc(m2_g)
