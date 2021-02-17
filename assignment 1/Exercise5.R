chick = chickwts   #make variable of the data

### QUESTION 5a compare weights between meatmeal and sunflower
sunflower <- subset(chick, chick$feed == 'sunflower')
meatmeal <- subset(chick, chick$feed == 'meatmeal')
meatmeal_sunflower = rbind(sunflower, meatmeal)            #create a new dataset with only 'sunflower'and 'meatmeal'

meatmeal_sunflower$feed= as.numeric(meatmeal_sunflower$feed)   #change factor to numeric
meatmeal_sunflower

# T-test 
t.test(meatmeal_sunflower$weight ~ meatmeal_sunflower$feed, paired= FALSE)  #perform t-test

# Mann-Whitney
wilcox.test(meatmeal_sunflower$weight ~ meatmeal_sunflower$feed, paired= FALSE)  #perform Mann-Whitney
wilcox.test(meatmeal_sunflower$weight, meatmeal_sunflower$feed)

# Kolmogorov-Smirnov test
sunflower = as.numeric(chick$feed); meatmeal = as.numeric(chick$feed)
par(mfrow=c(1,2)); hist(sunflower); hist(meatmeal)

ks.test(sunflower, meatmeal)


### QUESTION 5b compare weights between all the groups with an ANOVA test

# first we create a data frame in the right format with 'feed' as factor
chick = chickwts
is.factor(chick$feed) ; is.numeric(chick$weight)               #check whether 'feed' is a factor
chick[1:5,]
# create a linear model
chickenmodel <- lm(chick$weight ~ chick$feed, data = chick)
chickenmodel
# perform ANOVA test
anova(chickenmodel)

# calculate the estimated means for the different levels
summary(chickenmodel)
fitted(chickenmodel)



### QUESTION 5c use diagnostic tools 

# check for normality
par(mfrow=c(1,2));qqnorm(residuals(chickenmodel)); plot(chickenmodel, 1)

# Compute Shapiro-Wilk test of normality
shapiro.test(residuals(chickenmodel))


### QUESTION 5d 

is.factor(chick$feed) ; is.numeric(chick$weight)  #check for weight as number and feed as factor

kruskal.test(chick$weight, chick$feed, data = chick)















DELETED

#chickwtsframe = data.frame(weight= as.vector(as.matrix(chick)), feed= factor(chick))  #(rep(1:6, each=10)))
#chickwtsframe[1:10,]

# = data.frame(weight = as.vector(as.matrix(chick)), feed= factor(chick))
#chick
#chick = as.matrix(chickwts)
#chick = as.data.frame(chickwts)

#is.matrix(chick)
#is.data.frame(chick)




