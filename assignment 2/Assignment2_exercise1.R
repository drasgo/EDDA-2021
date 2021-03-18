setwd('/Users/simon/Documents/Master AI/EDDA/Assignment 2. R')
bread=read.table(file='bread.txt', header=TRUE) #header=true when there are labeled headers


bread$environment = as.factor(bread$environment)
bread$humidity = as.factor(bread$humidity)

### Question 1a Randomization
I = 3 ; J = 2 ; N= 3
rbind(rep(1:I, each = N *J), rep(1:J, N*I), sample(1:(N*I*J)))


### Question 1b Box-plots
par(mfrow=c(1,2))
boxplot(bread$hours~bread$environment)
boxplot(bread$hours~bread$humidity)

par(mfrow=c(1,2))
interaction.plot(bread$environment,bread$humidity,bread$hours)
interaction.plot(bread$humidity,bread$environment, bread$hours)


### Question 1c Two-way ANOVA


# create a linear model
breadmodel <- lm(bread$hours ~ bread$environment * bread$humidity, data = bread)

# r-squared environment
breadmodel1 <- lm(bread$hours ~ bread$environment, data = bread)
summary(breadmodel1)
breadmodel2 <- lm(bread$hours ~ bread$humidity, data = bread)
summary(breadmodel2)

# perform ANOVA test
anova(breadmodel)
anova(lm(bread$hours ~ bread$environment * bread$humidity))


# calculate the estimated means for the different levels
summary(breadmodel)

fitted(breadmodel)

# with function contrast you can change the defailt paramaterization of the 
# treatment paramaritization
# if the interaction is not significant, we have to remove the interaction from the 
# model. anova(lm(y, factor A + factor B, data = data))

### Question 1e Normality
par(mfrow=c(1,2))
qqnorm(residuals(breadmodel))
qqline(residuals(breadmodel))

plot(fitted(breadmodel), residuals(breadmodel))


















