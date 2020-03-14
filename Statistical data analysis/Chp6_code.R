##Matrix Approach to Multiple Regression

dsu = read.table("http://www.stat.columbia.edu/~jwang/w4315/data/dwaine.txt", header=F)

## Graphical exploration
pairs(dsu[,c(3,1,2)])
# install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(dsu, pch=20, box=FALSE, highlight=TRUE, angle=120)

x = dsu[2:22,1:2]; y = dsu[2:22,3]
X = as.matrix(cbind(1,x))
n = length(y); p = dim(X)[2]

## Estimation
Xs = t(X)%*%X; Xy = t(X)%*%y
b = qr.solve(X,y)
solve(Xs,Xy)
H = X%*%solve(Xs,t(X))
Hj = matrix(1,n,n)/n

yhat = X%*%b
res = y-yhat; sig2 = sum(res^2)/(n-p)

ssto = t(y)%*%(diag(n)-Hj)%*%y
sse = t(y)%*%(diag(n)-H)%*%y
ssr = t(y)%*%(H-Hj)%*%y

interact=x[,1]*x[,2]
par(mfrow=c(2,2), pch=20)
plot(res ~ yhat)
plot(res ~ x[,1])
plot(res ~ x[,2])
plot(res ~ interact, xlab="x[,1]x[,2]")

par(mfrow=c(1,2))
plot( abs(res) ~ yhat )
qqnorm(res)
qqline(res, col=2)

## Inference
s2b = sig2*solve(Xs)
Xh = rbind(c(1,65.4,17.6),c(1,53.1,17.7))
Yh = Xh%*%b
s2yh = Xh%*%s2b%*%t(Xh)
sqrt( s2yh + diag(2)*sig2 )

## Lack of fit test for multiple linear regression

testLF.m = function(X,Y,alpha=0.05){
  n = nrow(X); c = nrow( unique(X) ); p = ncol(X)        ### here X includes the 1 column
  reg = lm(Y ~ X)
  prd = reg$fit; resid = reg$res; sse = sum(resid^2)
  Xfac = apply(X,1,function(x) order(apply(t(x-t(unique(X)))^2,1,sum))[1])
  sspe = sum( tapply(Y, Xfac, function(y) sum( (y-mean(y))^2 )))
  Fstat = (sse-sspe)/(c-p)/(sspe/(n-c))
  crtF = qf(1-alpha,c-p,n-c); pval = 1-pf(Fstat,c-p,n-c)
  return( list(testT=Fstat,crtT=crtF,Pval=pval) )
}
