setwd('/Users/simon/Documents/Master AI/EDDA/Assignment 3. R')
africa=read.table(file='africa.txt', header=TRUE); africa

### Question 3a) Poisson regression

# first change the data to the right data type
africa$miltcoup = as.numeric(africa$miltcoup)
africa$oligarchy = as.numeric(africa$oligarchy)
africa$parties = as.numeric(africa$parties)
africa$numelec = as.numeric(africa$numelec)
africa$numregim = as.numeric(africa$numregim)
africa$pollib = as.numeric(africa$pollib)
#africa$pollib = as.factor(africa$pollib)

# perform a poisson analysis
africaglm=glm(miltcoup ~ oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim, 
              family=poisson, data= africa)
summary(africaglm)


### Question 3b) Step-down method

summary(glm(miltcoup ~ oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim, family = poisson, data= africa)) # first model

summary(glm(miltcoup ~ oligarchy+pollib+parties+pctvote+popn+size+ numregim, family = poisson, data= africa)) #numelec out

summary(glm(miltcoup ~ oligarchy+pollib+parties+pctvote+popn+size, family = poisson, data= africa)) #numregim out

summary(glm(miltcoup ~ oligarchy+pollib+parties+pctvote+ popn,family = poisson,  data= africa)) #size out

summary(glm(miltcoup ~ oligarchy+pollib+parties+pctvote, family = poisson,  data= africa)) #popn out

summary(glm(miltcoup ~ oligarchy+pollib+parties, family = poisson,  data= africa)) #pctvote out

final_model= summary(glm(miltcoup ~ oligarchy+pollib + parties, family = poisson,  data= africa)); final_model #final top-down model

final_model2= glm(miltcoup ~ oligarchy+pollib + parties, family = poisson,  data= africa); final_model2 #final top-down model

topdown_model= glm(miltcoup ~ oligarchy+pollib + parties, family = poisson,  data= africa); #top-down model


### Question 3c) Predicting

# change data frame to average 
avg_oligarchy = mean(africa$oligarchy)
avg_parties = mean(africa$parties)

# predict the outcome for the different factor levels
pred_lib_data = data.frame(oligarchy=avg_oligarchy, pollib=0, parties=avg_parties)  #level 0
output = predict(topdown_model, pred_lib_data, type="response"); output

pred_lib_data = data.frame(oligarchy=avg_oligarchy, pollib=1, parties=avg_parties)  #level 1
output = predict(topdown_model, pred_lib_data, type="response"); output

pred_lib_data = data.frame(oligarchy=avg_oligarchy, pollib=2, parties=avg_parties)  #level 2
output = predict(topdown_model, pred_lib_data, type="response"); output







pred_lib_data = data.frame(oligarchy='5.22', pollib=0, parties='17.08')
output = predict(final_model2, pred_lib_data, type="response")





























africa$miltcoup = as.numeric(africa$miltcoup)
africa$oligarchy = as.numeric(africa$oligarchy)
africa$parties = as.numeric(africa$parties)
africa$numelec = as.numeric(africa$numelec)
africa$numregim = as.numeric(africa$numregim)
africa$pollib = as.factor(africa$pollib)

mean(africa$oligarchy)
mean(africa$parties)
mean(africa$pctvote)
mean(africa$popn)
mean(africa$size)
mean(africa$numelec)
mean(africa$numregim)

newdata = data.frame(africa$miltcoup, africa$pollib, oligarchy= '5.22', parties = '17.03', pctvote = '32.11', popn = '11.57' , size = '484.58', numelec= '6.72', numregim = '2.75')

africaglm2= (glm(africa$miltcoup ~ oligarchy+ africa$pollib + parties + pctvote + popn  + size + numelec+ numregim,family=poisson, data= newdata))
                   



glm2=glm(africa$miltcoup ~ oligarchy + africa$pollib + parties+ pctvote + popn + size + numelec + numregim, data= newdata); summary(glm2)
fitted(glm2)
predict(glm2,africa,type="response")




