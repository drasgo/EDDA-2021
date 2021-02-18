# Title     : TODO
# Objective : TODO
# Created by: drasgo
# Created on: 16/02/21

data=read.table(file="run.txt",header=TRUE)
print(data)

"A: paired t test"
print(data[,1])
t.test(data[,1],data[,2],paired=TRUE)
qqnorm(data[,1]-data[,2])

"B: pearson correlation"
lemo <- data[1:12, 1:2]
energy <- data[13:24, 1:2]

qqnorm(lemo[,1])
qqnorm(lemo[,2])
cor.test(lemo[, 1], lemo[, 2])

qqnorm(energy[,1])
qqnorm(energy[,2])
cor.test(energy[,1], energy[,2])

"C: hypothesis is 'there is no difference', so p>0.05 ==> there is a difference"

lemo_diff = lemo[,1] - lemo[,2]
energy_diff = energy[,1] - energy[,2]

mystat=function(x,y) {mean(x-y)}
B=1000
tstar=numeric(B)
for (i in 1:B)
{
  datastar=t(apply(cbind(lemo_diff,energy_diff),1,sample))
  tstar[i]=mystat(datastar[,1],datastar[,2])
}
myt=mystat(lemo_diff,energy_diff)

myt
hist(tstar)
pl=sum(tstar<myt)/B
pr=sum(tstar>myt)/B
p=2*min(pl,pr)
print(p)