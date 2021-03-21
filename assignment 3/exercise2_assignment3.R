titanic=read.table(file="titanic.txt",header=TRUE)
attach(titanic)
titanic
par(mfrow=c(2, 2))



# **** a. Fatto

titanic$Age = as.numeric(titanic$Age)
titanic$PClass = as.numeric(titanic$PClass)
titanic$Survived = as.numeric(titanic$Survived)
titanic$Sex = as.numeric(titanic$Sex)
hist(titanic[,3],main="Age")
hist(titanic[,2], main="PClass")
hist(titanic[,4], main="Sex")
hist(titanic[,5], main="Survived")


# number of individuals for combination of PClass and Gender
xtabs(~titanic$PClass+titanic$Sex, data=titanic)

# number of individuals for combination of Survived and Gender
xtabs(~titanic$Survived+titanic$Sex, data=titanic)

# number of individuals for combination of Survived and PClass 
xtabs(~titanic$Survived+titanic$PClass, data=titanic)

# number of individuals for combination of Survived and Age 
xtabs(~titanic$Survived+titanic$Age, data=titanic)

# percentage of saved for combination of PClass and Sex
tot = xtabs(titanic$Survived~titanic$PClass+titanic$Sex, data=titanic)
round(tot/xtabs(~titanic$PClass+titanic$Sex), 2)

summary(titanic)




# **** b. (Fatto)

# titanic$Survived
# titanic$Survived = factor(titanic$Survived)
# titanic$PClass = factor(titanic$PClass)
# titanic$Age = factor(titanic$Age)
titanic$Sex = as.numeric(titanic$Sex)
titanic$PClass = as.numeric(titanic$PClass)
titanic$Age = as.numeric(titanic$Age)
call <- glm(Survived~PClass+Age+Sex,family=binomial, data=titanic)
#call1 <- glm(Survived~PClass+Sex,family=binomial, data=titanic)
#anova(call1, call)
summary(call)
#anova(call)
# the estimated odds are calculated as reported by summary(call)


# **** c. Fatto
titanic$Sex = factor(titanic$Sex)
titanic$PClass = factor(titanic$PClass)
call1 <- glm(Survived~Age*(PClass+Sex),family=binomial, data=titanic)
anova(call1_alternativa, test="Chisq")

# No interaction between age pc class and sex, but interaction between age and sex



# guardare interazioni tra factors
titanic$Survived = factor(titanic$Survived)
titanic$PClass = factor(titanic$PClass)
titanic$Sex = factor(titanic$Sex)
titanic$Age = as.numeric(titanic$Age)
model = glm(Survived~PClass+Age*Sex,data=titanic,family=binomial)
summary(model)
fitted(model)


for (pc in levels(titanic$PClass)){
  for (sex in levels(titanic$Sex)) {
    newdata = data.frame(Sex=sex, PClass=pc, Name="test", Age=53)
    print(newdata)
    print(predict(model, newdata, type="response"))
    }
}



# **** d. Ripetere parte di Machine Learning 
# Much higher than p_0 or much lower than p_0, P gives a prediction with more confidence



# **** e. Fatto

#Chisquare for pclass + survived and fisher for sex + survived

chisq.test(titanic$PClass, titanic$Survived)
fisher.test(titanic$Sex, titanic$Survived)


#call <- glm(Survived~PClass+Age+Sex,family=binomial, data=titanic)
#call1 <- glm(Survived~Age+Sex,family=binomial, data=titanic)
#call2 <- glm(Survived~Age+PClass,family=binomial, data=titanic)
#call3 <- glm(Survived~Age,family=binomial, data=titanic)

#anova(call1, call)
#anova(call2, call)
#anova(call3, call)


# **** f. Da fare

# chisquare and fisher only give an idea on the independence of two variables but nothing more on the actual influence + can be inaccurate for too little samples


