#################################################################
### Some R codes for analyzing Toluca Company data
#################################################################
 

## readin data, R can handle http input
toluca = read.table("toluca.txt",header=TRUE)
attach(toluca); n = length(WorkHours)

#### Residual Diagnostics
toluca.reg = lm(WorkHours ~ LotSize)
resid = toluca.reg$res
## built-in R commands for doing regression diagnostics plots
plot(toluca.reg)
## no special pattern, quite random
plot(LotSize, resid)

alpha = 0.05
### Modified Levene Test
e1 = resid[LotSize<=70]; e2 = resid[LotSize>70]
n1 = length(e1); n2 = length(e2)
d1 = abs(e1-median(e1)); d2 = abs(e2-median(e2))
d1mu = mean(d1); d2mu = mean(d2)
nom = d1mu - d2mu
sig = sqrt( sum((d1-d1mu)^2)/(n-2)+sum((d2-d2mu)^2)/(n-2) )
tML = nom/sig/sqrt(1/n1+1/n2)
tML; qt(1-alpha/2,n-2); 2*pt(-abs(tML),n-2)

### Breusch-Pagan Test
r2 = resid^2; r2.reg = lm(r2 ~ LotSize)
nom = sum((r2-mean(r2))^2)/2 - sum(r2.reg$resid^2)/2
den = (sum(resid^2)/n)^2
xBP = nom/den
xBP; qchisq(1-alpha,1); 1-pchisq(nom/den,1)


#################################################################
### Plutonium Example Study
#################################################################
###### Some useful wrap functions
### Modified Levene test for constant variance


cvML = function(resid, X,x0=median(X), alpha=0.05){
  e1 = resid[X<=x0]; e2 = resid[X>x0]
  n1 = length(e1); n2 = length(e2); n = n1+n2
  d1 = abs(e1-median(e1)); d2 = abs(e2-median(e2))
  d1mu = mean(d1); d2mu = mean(d2)
  nom = d1mu - d2mu
  sig = sqrt( sum((d1-d1mu)^2)/(n-2)+sum((d2-d2mu)^2)/(n-2) )
  tML = nom/sig/sqrt(1/n1+1/n2)
  crtT = qt(1-alpha/2,n-2)
  pval = 2*pt(-abs(tML),n-2)
  return( list(testT=tML,crtT=crtT,Pval=pval) )
}
## x = plut[-24,2]; y = plut[-24,1]
## cvML(lm(y ~ x)$res, x)
## cvML(lm(sqrt(y) ~ x)$res, x)
## cvML(lm(sqrt(y) ~ sqrt(x))$res, sqrt(x))
### Breusch-Pagan test for constant variance
cvBP = function(resid,X,alpha=0.05){
  n = length(resid)
  e2 = resid^2; e2.reg = lm(e2 ~ X)
  nom = sum((e2-mean(e2))^2)/2 - sum(e2.reg$res^2)/2
  den = (sum(e2)/n)^2
  Tstat = nom/den; crtT = qchisq(1-alpha,1)
  pval = 1-pchisq(Tstat,1)
  return( list(testT=Tstat,crtT=crtT,Pval=pval) )
}
## cvBP(lm(y ~ x)$res, x)
## cvBP(lm(sqrt(y) ~ x)$res, x)
## cvBP(lm(sqrt(y) ~ sqrt(x))$res, sqrt(x))
## Correlation test for normality
corTest = function(resid){
  n = length(resid)
  r = cor(sort(resid),qnorm((1:n-0.375)/(n+0.25)))
  return(corRes=r)
}
## corTest(lm(y ~ x)$res)
### F-test for lack of fit
testLF = function(X,Y,alpha=0.05){
  n = length(X); g = length( unique(X) )
  reg = lm(Y ~ X)
  prd = reg$fit; resid = reg$res; sse = sum(resid^2)
  sspe = sum( tapply(Y, X, function(y) sum( (y-mean(y))^2 )) )
  Fstat = (sse-sspe)/(g-2)/(sspe/(n-g))
  crtF = qf(1-alpha,g-2,n-g); pval = 1-pf(Fstat,g-2,n-g)
  return( list(testT=Fstat,crtT=crtF,Pval=pval) )
}
## testLF(x, y); testLF(x, sqrt(y)); testLF(sqrt(x), sqrt(y))

### Uncomment pdf()/dev.off() pair to create hard copies of plots
plut = read.table("direct-to/plutonium.txt",header=TRUE)
attach(plut)

### Univariate study
## dot plot
stripchart(AlphaCount, method="stack", vertical=FALSE, pch=20)
## stem-and-leaf: text version
stem(AlphaCount)
## postscript(file="Ch03-03.ps", width=6, height=4)
## pdf(file="Ch03-03.pdf", width=6, height=4)
par(mar=c(4.5,4.5,2,1), mfrow=c(2,2))
## histogram
hist(AlphaCount, prob=FALSE)
## manually create stem-and-leaf plot
plot(1:7, 1:7, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="none")
title(main="Stem-and-Leaf Plot")
text(1,6, "decimal point 1 digit to the right of |", adj=c(0,1),cex=0.8)
text(1,5, "0 | 0000111113444", adj=c(0,1), cex=1)
text(1,4, "0 | 5556778", adj=c(0,1), cex=1)
text(1,3, "1 | 113", adj=c(0,1), cex=1)
text(1,2, "1 | 5", adj=c(0,1), cex=1)
## boxplot
boxplot(AlphaCount, main="Box Plot", horizontal=TRUE)
## sequence plot
plot(AlphaCount, main="Sequence Plot", pch=20, type="b", lty=2)
## dev.off()

### Scatterplot
## postscript(file="Ch03-04.ps", width=6, height=4)
## pdf(file="Ch03-04.pdf", width=6, height=4)
par(mar=c(4.5,4.5,2,1), mfrow=c(1,1))
plot(AlphaCount ~ PluAct, pch=20)
## lowess line interpolation: 
##  doesn't make much sense, too few data points, better to do mean interpolation
lines(lowess(PluAct, AlphaCount), col=2, lty=2)
## interactively label data points
## identify(PluAct, AlphaCount)
text(PluAct[24], AlphaCount[24], label="24", col=2, pos=3)
## dev.off()

## remove outlier #24
PluAct = PluAct[-24]; AlphaCount = AlphaCount[-24]
n = length(PluAct); alpha=0.05
## Regression 1
reg1 = lm(AlphaCount ~ PluAct)
res1 = reg1$res; prd1 = reg1$fit
## postscript(file="Ch03-05.ps", width=6, height=4)
## pdf(file="Ch03-05.pdf", width=6, height=4)
par(mar=c(4.5,4.5,2,1), mfrow=c(2,2))
plot(AlphaCount ~ PluAct, pch=20)
abline(reg1, col=2, lty=2)
plot(PluAct, res1, pch=20, ylab="Residuals")
abline(h=0, lty=2, col="gray")
plot(prd1, res1, pch=20, xlab=expression(hat(Y)), ylab="Residuals")
abline(h=0, lty=2, col="gray")
qqnorm(res1,pch=20); qqline(res1, col=2, lty=2)
## dev.off()

## Breusch-Pagan test
r2 = res1^2; r2.reg = lm(r2 ~ PluAct)
nom = sum((r2-mean(r2))^2)/2 - sum(r2.reg$res^2)/2
den = (sum(res1^2)/n)^2
xBP = nom/den
xBP; qchisq(1-alpha,1); 1-pchisq(xBP,1)
cvBP(res1,PluAct)
## ML test
cvML(res1,PluAct)
## LF test
testLF(PluAct, AlphaCount)
## Correlation tset
corTest(res1)

## Square root transformation
AlphaCount2 = sqrt(AlphaCount)
## Regression 2
reg2 = lm(AlphaCount2 ~ PluAct)
res2 = reg2$res; prd2 = reg2$fit
## postscript(file="Ch03-06.ps", width=6, height=4)
## pdf(file="Ch03-06.pdf", width=6, height=4)
par(mar=c(4.5,4.5,2,1), mfrow=c(2,2))
plot(AlphaCount2 ~ PluAct, pch=20, ylab=expression(sqrt(AlphaCount)) )
abline(reg2, col=2, lty=2)
plot(PluAct, res2, pch=20, ylab="Residuals")
abline(h=0, lty=3, col="gray")
plot(prd2, res2, pch=20, xlab=expression(hat(Y)), ylab="Residuals")
abline(h=0, lty=3, col="gray")
qqnorm(res2,pch=20); qqline(res2, col=2, lty=2)
## dev.off()

## Breusch-Pagan test
r2 = res2^2; r2.reg = lm(r2 ~ PluAct)
nom = sum((r2-mean(r2))^2)/2 - sum(r2.reg$res^2)/2
den = (sum(res2^2)/n)^2
xBP = nom/den
xBP; qchisq(1-alpha,1); 1-pchisq(xBP,1)
cvBP(res2,PluAct)
cvML(res2,PluAct)
testLF(PluAct,AlphaCount2)
corTest(res2)

## Square root transformation
PluAct2 = sqrt(PluAct)
## Regression 3
reg3 = lm(AlphaCount2 ~ PluAct2)
res3 = reg3$res; prd3 = reg3$fit
## postscript(file="Ch03-07.ps", width=6, height=4)
## pdf(file="Ch03-07.pdf", width=6, height=4)
par(mar=c(4.5,4.5,2,1), mfrow=c(2,2))
plot(AlphaCount2 ~ PluAct2, pch=20, xlab=expression(sqrt(PluAct)), ylab=expression(sqrt(AlphaCount)) )
abline(reg3, col=2, lty=2)
plot(PluAct2, res3, pch=20, xlab=expression(sqrt(PluAct)), ylab="Residuals")
abline(h=0, lty=3, col="gray")
plot(prd3, res3, pch=20, xlab=expression(hat(Y)), ylab="Residuals")
abline(h=0, lty=3, col="gray")
qqnorm(res3,pch=20); qqline(res3, col=2, lty=2)
## dev.off()

## correlation test for normality
cor(sort(res3), qnorm((1:n-0.375)/(n+0.25)))
corTest(res3)
## Shapiro-Wilk & Kolmogorov-Smirnov test
shapiro.test(res3)
ks.test(res3, "pnorm", mean(res3), sd(res3))

## Breusch-Pagan test for constant variance
r2 = res3^2; r2.reg = lm(r2 ~ PluAct2)
nom = sum((r2-mean(r2))^2)/2 - sum(r2.reg$res^2)/2
den = (sum(res3^2)/n)^2
xBP = nom/den
xBP; qchisq(1-alpha,1); 1-pchisq(xBP,1) ## marginally significant
cvBP(res3,PluAct2)
cvML(res3,PluAct2)
testLF(PluAct2,AlphaCount2)

## postscript(file="Ch03-08.ps", width=6, height=4)
## pdf(file="Ch03-08.pdf", width=6, height=4)
par(mar=c(4.5,4.5,2,1), mfrow=c(1,1))
plot(AlphaCount2 ~ PluAct2, pch=20,
     xlab=expression(sqrt(PluAct)), ylab=expression(sqrt(AlphaCount)) )
## abline(reg3, col=2, lty=2)
## Mean Interpolation
lines(sort(unique(PluAct2)), tapply(AlphaCount2, PluAct2, mean), col=2)
points(sort(unique(PluAct2)), tapply(AlphaCount2, PluAct2, mean), col=2, pch=3)
## confidence band
mux = mean(PluAct2); s2x = sum((PluAct2-mux)^2)/(n-1)
xh = seq(min(PluAct2), max(PluAct2), length=1e2)
yh = reg3$coef[1]+reg3$coef[2]*xh
w = sqrt(2*qf(1-alpha,2,n-2))
sig = sqrt(sum(res3^2)/(n-2))
cbx = sqrt(2*qf(1-alpha,2,n-2))*sig*sqrt(1/n+(xh-mux)^2/(s2x*(n-1)))
lines(xh, yh+cbx, col=3); lines(xh, yh-cbx, col=3)
legend(0,0.4, c("Mean Curve", "95% Confidence Band"), col=2:3, lty=1, bty="n")
## dev.off()