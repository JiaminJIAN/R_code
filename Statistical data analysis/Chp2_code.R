toluca = read.table("toluca.txt", header=TRUE)
n = dim(toluca)[1]

attach(toluca)
### Point Estimation
muy = mean(WorkHours); mux = mean(LotSize)
s2x = sum( (LotSize-mux)^2 )/(n-1)
sxy = sum( (LotSize-mux)*(WorkHours-muy) )/(n-1)
b1 = sxy/s2x; b0 = muy-b1*mux
resid = WorkHours - (b0+b1*LotSize)
sum(resid)
sig = sqrt( sum(resid^2)/(n-2) )
sum(resid*LotSize)

plot(LotSize, WorkHours, pch=20)
abline(b0,b1, col=2)

### CI
s1 = sig/sqrt(s2x*(n-1)); s0 = sig*sqrt( 1/n+mux^2/(s2x*(n-1)) )
alpha = 0.05
talpha = qt(1-alpha/2,n-2)
b0+c(-1,1)*talpha*s0
b1+c(-1,1)*talpha*s1

### P-value
qt(1-alpha/2,n-2)
t1 = abs(b1)/s1
pt(-t1,n-2)*2
t0 = abs(b0)/s0
pt(-t0,n-2)*2

### Power Calculation
beta0 = 62; beta1 = 3; sigma = 40
ncp0 = beta0/(s0/sig)/sigma
p01 = pt(-qt(1-alpha/2,n-2), df=n-2, ncp=ncp0, lower.tail = TRUE, log.p = FALSE)
p02 = 1-pt(qt(1-alpha/2,n-2), df=n-2, ncp=ncp0, lower.tail = TRUE, log.p = FALSE)
p01+p02

ncp1 = beta1/(s1/sig)/sigma
p11 = pt(-qt(1-alpha/2,n-2), df=n-2, ncp=ncp1, lower.tail = TRUE, log.p = FALSE)
p12 = 1-pt(qt(1-alpha/2,n-2), df=n-2, ncp=ncp1, lower.tail = TRUE, log.p = FALSE)
p11+p12