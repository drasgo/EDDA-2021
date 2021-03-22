df = read.table(file="F:\\Downloads\\fruitflies.txt",header=TRUE)
df$loglongevity = log(df$longevity)
df
df$activity = as.factor(df$activity)
is.factor(df$activity)
#Make an informative plot of the data. Investigate whether sexual activity influences longevity by
#performing a statistical test, without taking the thorax length into account. What are the estimated
#longevities for the three conditions? Comment.
plot(df$longevity~df$thorax,pch=as.character(df$activity))

plot(df$loglongevity~df$thorax,pch=as.character(df$activity))

m1 = lm(loglongevity~activity, data=df)
m1
summary(m1)
print(anova(m1))
#factor activity is significant, however 1-way anova test with a factor is not correct. We must consider the predictor variable


#Investigate whether sexual activity influences longevity by performing a statistical test, now including
#thorax length as an explanatory variable into the analysis. Does sexual activity increase or decrease
#longevity? What are the estimated longevities for the three groups, for a fly with average thorax
#length?
m2 = lm(loglongevity~thorax+activity,data=df)
drop1(m2,test='F')
summary((m2))
#sexual activity does influence longevity
#The estimates from the summary show that high sexual activity results in more longevity
avg_thorax = mean(df$thorax)
avg_thorax
summary(m2)
fitted(m2)


#How does thorax length influence longevity? Investigate graphically and by using an appropriate test
#whether this dependence is similar under all three conditions of sexual activity
plot(df$thorax,df$loglongevity)
plot(loglongevity~thorax,pch=unclass(activity), data=df)
for (i in c('high', 'low', 'isolated')) abline(lm(loglongevity~thorax,data=df[df$activity==i,]))
m3 = lm(loglongevity~activity*thorax,data=df)
anova(m3)


#Which of the two analyses, without or with thorax length, do you prefer? Is one of the analyses wrong?

#