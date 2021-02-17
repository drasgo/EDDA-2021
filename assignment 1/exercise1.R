
setwd('/Users/simon/Documents/Master AI/Experimental Design/Assignment 1. R')

data=read.table(file='birthweight.txt', header=TRUE) #header=true when there are labeled headers
data = as.numeric(data$birthweight)  #convert from integer to numeric
class((data))                        #checking class
options(digits = 3)

### QUESTION 1a
# first check normality with histogram
par(mfrow=c(1,2))
hist(data, freq=FALSE, col="gray", xlab="Birthweight", main="Birthweight of 88 newborn babies") #histogram of birthweight
curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col="red") #normality line
qqnorm(data)

# 90% confidence interval for µ
stddev= sd(data, na.rm=TRUE)                 #computer sd
n = 188
qt(.95, n-1)
mean(data)

me <- qt(.95, 187)* stddev/sqrt(n-1)

lower_bound = mean(data) - me
lower_bound
upper_bound = mean(data) + me
upper_bound


### QUESTION 1b
t.test(data, y = NULL,
       mu = 2800, alternative = 'g', paired = FALSE,    # H0 : µ ??? µ0 vs H1 : µ > µ0 with alpha of 0.10
       conf.level = 0.90)
t.test(data, y = NULL,
       mu = 2800, alternative = 'g', paired = FALSE,    # H0 : µ ??? µ0 vs H1 : µ > µ0 with alpha 0f 0.05
       conf.level = 0.95)
t.test(data, y = NULL,
       mu = 2800, alternative = 'g', paired = FALSE,    # H0 : µ ??? µ0 vs H1 : µ > µ0 with alpha 0f 0.01
       conf.level = 0.99)












# DELETED

qqnorm(data)




