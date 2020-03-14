### Figure 12.2 

library(TSA)

a0<-1
a1<-0.95

u<-0.1
phi<-0.8

N<-60
seed<-2^7-1
set.seed(seed)

whitenoise<-rnorm(N)

##      GARCH -
# Simulate ARCH(1):
at = garch.sim(alpha = c(a0, a1), n=N)

#conditional std dev
st<-rep(1,N)
for(i in 2:N)
{
st[i]<-a0+a1*(at[i-1]*at[i-1])
}

#simulate AR(1)/ARCH(1)
arat<-rep(1,N)
for(i in 2:N)
{
arat[i]<- u+ phi*(arat[i-1]-u)+at[i]
}


par(mfrow=c(2,2))
plot(whitenoise,type='l',xlab=" ",ylab=" ", main="White Noise")
plot(st,type='l',xlab=" ",ylab=" ", main="conditional std dev")
plot(at,xlab=" ",ylab=" ", main="ARCH(1)")
plot(arat,type='l',xlab=" ",ylab=" ", main="AR(1)/ARCH(1)")


#Figure 12.4 

a0<-1
a1<-0.08
b1<-0.9

phi<-0.8

N<-600

seed<-2^11-1
set.seed(seed)

whitenoise<-rnorm(N)

##      GARCH -
# Simulate GARCH(1,1):
gat = garch.sim(alpha = c(a0,a1),beta=b1, n=N)

vart<-rep(1,N)
for(i in 2:N)
{
vart[i]<-a0+a1*(gat[i-1]*gat[i-1])+b1*(vart[i-1])
}
st<-sqrt(vart)

#simulate AR(1)/gARCH(1,1)
garat<-rep(0,N)
garat[1]<-gat[1]
for(i in 2:N)
{
garat[i]<- phi*garat[i-1]+gat[i]
}

par(mfrow=c(3,2))
plot(whitenoise,type='l',xlab=" ",ylab=" ", main="White Noise")
plot(st,type='l',xlab=" ",ylab=" ", main="conditional std dev")
plot(at,xlab=" ",ylab=" ", main="GARCH(1,1)")
plot(arat,type='l',xlab=" ",ylab=" ", main="AR(1)/GARCH(1,1)")
qqnorm(at)
qqline(at)



## Fit a GARCH model
library(fGarch)
library(tseries)

gat<-scan("./garch02.txt")
fit = garchFit(formula = ~arma(1, 0)+garch(1, 1),data = ts(gat))
summary(fit)

