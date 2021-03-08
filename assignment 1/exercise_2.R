# Title     : TODO
# Objective : TODO
# Created by: drasgo
# Created on: 16/02/21
p.value=function(n,m,mu,nu,sd,B=1000){
  p=numeric(B) # p will be an array of realized p-values
  for (b in 1:B) {x=rnorm(n,mu,sd); y=rnorm(m,nu,sd)
  p[b]=t.test(x,y,var.equal=TRUE)[[3]]}
  return(p)}


get_power=function(n,m,mu,sd,B, sequence){
  power = numeric(length(sequence))
  
  for (index in 1:length(sequence)){
    nu_val <- sequence[index]
    p <- numeric(B)
    
    for (b in 1:B) {
      x<-rnorm(n,mu,sd);
      y<-rnorm(m,nu_val,sd)
      p[b] <- t.test(x,y,var.equal=TRUE)[[3]]
    }
    
    power[index] <- mean(p.value(n, m, mu, nu_val, sd, B)<0.05)
  }
  return(power)
}

par(mfrow=c(1, 2))

"A"
n <- 30
m <- 30
mu <- 180
sd <- 5
B <- 1000
nu <-seq(175, 185, by=0.25)

power = get_power(n,m,mu,sd,B, nu)
plot(power, nu, main="Graph 2.1")
"hist(power)
qqnorm(power)"

"B"
n <- 100
m <- 100

power = get_power(n,m,mu,sd,B,nu)
plot(power, nu, main = "Graph 2.2")

"C"
par(mfrow=c(1, 1))
n<-30
m<-30
sd<-15

power = get_power(n,m,mu,sd,B,nu)
plot(power, nu, main="Graph 2.3")

