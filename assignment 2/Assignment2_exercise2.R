setwd('/Users/simon/Documents/Master AI/EDDA/Assignment 2. R')
search=read.table(file='search.txt', header=TRUE) #header=true when there are labeled headers
search$skill = as.factor(search$skill)
search$interface = as.factor(search$interface)

# MAKE PLOTS/GRAPHS of DATA
xtabs(time~interface+skill, search())

attach(search)

par(mfrow=c(1,2))
interaction.plot(interface,skill,time)
#interaction.plot(skill,interface,time)


boxplot(time~interface)
boxplot(time~skill)

##################################

# QUESTION 2A make randomized block design

search <- tibble::rowid_to_column(search, "student_number")
search

I = 3 ; B = 5 ; N= 1
for (i in 1:B) print (sample(1:(N*I)))

###################################

# QUESTION 2B  perform ANOVA

# main effects
main = lm(time ~ interface+skill, data = search);  
anova(main)
summary(aovint)
summary(main)

# interaction effect
interaction = lm(time~interface * skill)
anova(interaction)
summary(interaction)

fitted(main)

#par(mfrow=c(1,4))
#interaction.plot(interface,skill,time)
#interaction.plot(skill,interface,time)


xtabs(time~interface+skill, search())
boxplot(time~interface)
boxplot(time~skill)

##################################

# QUESTION 2C check assumptions

par(mfrow=c(1,2))
qqnorm(residuals(main), main= 'Normal Q-Q plot', xlab= "Sample Quantiles", ylab= 'Theoretical Quantiles')
#plot(fitted(main), residuals(main))

par(mfrow=c(1,2));qqnorm(residuals(main)); plot(main)

shapiro.test(residuals(main))

help("plot")

help("qqnorm")

#######################

# QUESTION 2D FRIEDMAN

friedman.test(time, interface, skill, data = search)

###########################

# QUESTION 2E ONE-WAY ANOVA

searchmodel <- lm(time ~ interface, data = search )
anova(searchmodel)



chickenmodel <- lm(chick$weight ~ chick$feed, data = chick)
chickenmodel
# perform ANOVA test
anova(chickenmodel)