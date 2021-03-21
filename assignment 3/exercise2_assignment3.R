titanic=read.table(file="titanic.txt",header=TRUE)
attach(titanic)
titanic

# a (Fatto)
titanic$Age = as.numeric(titanic$Age)
titanic$PClass = as.numeric(titanic$PClass)
hist(titanic[,3],main="Age")
hist(titanic[,2], main="PClass")

# number of individuals for combination of PClass and Gender
xtabs(~titanic$PClass+titanic$Sex, data=titanic)

# number of individuals for combination of Survived and Gender
xtabs(~titanic$Survived+titanic$Sex, data=titanic)

# number of individuals for combination of Survived and PClass 
xtabs(~titanic$Survived+titanic$PClass, data=titanic)

# percentage of saved for combination of PClass and Sex
tot = xtabs(titanic$Survived~titanic$PClass+titanic$Sex, data=titanic)
round(tot/xtabs(~titanic$PClass+titanic$Sex), 2)

summary(titanic)
# summarise_all(titanic, funs(mean))
# titanic %>%
#  summarise(averageRating = mean(Rating),
#            sdRating = sd(Rating))


#b (Fatto)
# titanic
# titanic$Survived
titanic$Survived = factor(titanic$Survived)
# titanic$PClass = factor(titanic$PClass)
# titanic$Age = factor(titanic$Age)
titanic$Sex = as.numeric(titanic$Sex)
titanic$PClass = as.numeric(titanic$PClass)
titanic$Age = as.numeric(titanic$Age)
call <- glm(Survived~PClass+Age+Sex,family=binomial, data=titanic)
call1 <- glm(Survived~PClass+Sex,family=binomial, data=titanic)
anova(call1, call)
summary(call)
anova(call)
# the estimated odds are calculated as reported by summary(ca)


# c Mi manca "scegliere" modello e testarlo  su factors di PClass e Sex con Age == 53
call1 <- glm(Survived~Age*PClass*Sex,family=binomial, data=titanic)
anova(call1, test="Chisq")
summary(call1)
# No interaction between age pc class and sex, but interaction between pc class and sex
titanic$Survived = factor(titanic$Survived)
titanic$PClass = factor(titanic$PClass)
titanic$Sex = factor(titanic$Sex)
titanic$Age = as.numeric(titanic$Age)
head(titanic)

# titanic = titanic[titanic$Age==53.00,]
call2 <- glm(Survived~PClass+Age+Sex,family=binomial, data=titanic)
drop1(call2,test="Chisq")
summary(call2)
shortglm=glm(cbind(ncases,ncontrols)~age+age2+alc+tob,data=titanic,family=binomial)

# d Mi manca da trovare test per la qualità della predizione
newdata = data.frame(PClass="", Age="", Sex="")
predict(model, newdata, type="response")

# e Capire perchè non mi da il p-value (Forse tipo di test..?)

call <- glm(Survived~PClass+Age+Sex,family=binomial, data=titanic)
call1 <- glm(Survived~Age+Sex,family=binomial, data=titanic)
call2 <- glm(Survived~Age+PClass,family=binomial, data=titanic)
call3 <- glm(Survived~Age,family=binomial, data=titanic)

anova(call1, call)
anova(call2, call)
anova(call3, call)


# f Da fare