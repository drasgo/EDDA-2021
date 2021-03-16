library(lme4)


cow=read.table(file="cow.txt",header=TRUE)
attach(cow)
cow

#Point 1: fixed effects
cow$id=factor(cow$id);
cow$milk=factor(cow$milk)
cow$id <- as.numeric(as.character(cow$id)) 
cow$milk <- as.numeric(as.character(cow$milk))

cowlm=lm(milk~per+id+treatment,data=cow)
anova(cowlm)
summary(cowlm)

#Point 2: mixed effects
cowlmer=lmer(milk~treatment+order+per+(1|id),data=cow,REML=FALSE)
summary(cowlmer)

cowlmer1=lmer(milk~order+per+(1|id),data=cow,REML=FALSE)
anova(cowlmer1,cowlmer)


#Point 3: t-test paired
t.test(milk[treatment=="A"],milk[treatment=="B"],paired=TRUE)
