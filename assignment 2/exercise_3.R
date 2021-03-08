library(lme4)


cow=read.table(file="cow.txt",header=TRUE)
attach(cow)


#Point 1: fixed effects
cow$id=factor(cow$id);
cow$milk=factor(cow$milk)
cow$id <- as.numeric(as.character(cow$id)) 
cow$milk <- as.numeric(as.character(cow$milk))

cowlm=lm(milk~treatment+per+id,data=cow)
anova(cowlm)
summary(cowlm)

#Point 2: mixed effects
cowlmer=lmer(milk~treatment+order+per+(1|id),REML=FALSE)
summary(cowlmer)


#Point 3: t-test paired
t.test(milk[treatment=="A"],milk[treatment=="B"],paired=TRUE)
