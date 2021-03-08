setwd('/Users/simon/Documents/Master AI/EDDA/Assignment 2. R')
austen=read.table(file='austen.txt', header=TRUE) #header=true when there are labeled headers

##### Question 4b
# make new dataframe
df = subset(austen, select = -c(Sand2) )
df

z= chisq.test(df); z

residuals(z)  # =(z$observed-z$expected)/sqrt(z$expected)



##### Question 4c
#new_df = as.numeric(austen$Sense)
#new_df1 = as.numeric(austen$Emma)
#new_df2 = as.numeric(austen$Sand1)

#newdata = cbind(new_df,new_df1, new_df2)
#newdata

y = chisq.test(austen); y
residuals(y)
