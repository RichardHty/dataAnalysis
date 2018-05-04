#cheat sheet

#quantitative data
quantile(data$variable)
#qualitative data
#class frequencies
table(data$variable)

#histogram

#change data type into a vector
data<-unlist(duration, use.names=FALSE)
#make sure the range is just right
range_days <- max(data) - min(data) + 1
#generate a density histgram
hist(data,range_days,labels = TRUE,prob=TRUE,
     main = "Distribution of Duration of Days", 
     xlab = "duration")
lines(density(data),col="red")

#generate a boxplot 
boxplot(data)

#normal distribution
z=1.2
c = 0.8849303
a = 0
b = 1
#area to the left of z
z<-pnorm(z)
z_score<-pnorm(z,mean=a,sd=b)
#z-score with a as the area to the left
p<-qnorm(c)
probability<-qnorm(c,mean=a,sd=b)
#get the point value of normal distribution
d<-dnorm(x)

#outlier
boxplot()
#from t-statistic calculate prob
p<-pt(t_statistic,df=degreeOfFreedom)
#from prob calculate t-statistic
t<-qt(prob,df=degreeOfFreedom)

#t.test
#alternative can be ‘less’, ‘greater’, or ‘two.sided’
t.test(data$variable, mu=mu0, alternative='two.sided', conf.level=0.9)
shark_len<- c(18.1, 23.4, 23.8, 24.1, 22.5, 19, 25.4, 23.1, 16.5, 26.7)
t <- (mean(shark_len) - 20)/(sd(shark_len)/sqrt(length(shark_len))) 
p <- 1 - pt(t , df=length(shark_len) - 1)
t.test(shark_len, mu=20, alternctive="two sided", conf.level=0.9)

#correlation
r<-cor(data)
#t-test for linear association, nature is t-test so can be calculated through qt(prob,df)
t_association<-r*(sqrt((n-2)/(1-r*r)))
# or [alternative] = ‘two.sided’, ‘less’ (corresponds to negative association), or ‘greater’ (corresponds to positive association)
#[method] = “pearson”, “kendall”, or “spearman”
cor.test(data$explanatoryvariable, data$responsevariable,
         alternative='alternative', method='method', conf.level='confdence level')

#simple linear regression
slr<-lm(data$response~data$explanatory) 
abline(a=intercept, b=slope)


