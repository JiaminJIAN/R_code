###############  Case Study: Power Cells Data  ###############

power=read.table("PowerCells.txt",header=F)
head(power)
names(power)=c("Y","X1","X2")
n=nrow(power)
X1=as.numeric(factor(power$X1))-2; X2=as.numeric(factor(power$X2))-2
X1SQ=X1^2; X2SQ=X2^2; X1X2=X1*X2
Y = power$Y
power=data.frame(cbind(Y,X1,X2,X1SQ,X2SQ,X1X2))
cor(power)
m1=lm(Y~.,data=power)
summary(m1)
plot(m1$resid~m1$fit); plot(m1$resid~X1); plot(m1$resid~X2)

m2=lm(Y~X1+X2)
summary(m2)

sse.m1=anova(m1)[6,2]
sse.m2=anova(m2)[3,2]

F.stat=(sse.m2-sse.m1)/3/(sse.m1/(n-6))
F.pval=1-pf(F.stat,3,n-6)

###############  Codings in ANOVA  ###############

## the default contrast options for ANOVA
options("contrasts")

## simulation example
n = 1e2; p = 4
y = rnorm(n)
x = sample(letters[1:p], size=n, rep=TRUE)
xf = as.factor(x)

## individual class means
mug = tapply(y,xf, mean)

## class indicator matrix
Z = matrix(0,n,p)
Z[cbind(1:n,as.integer(xf))] = 1
## indicator coding: identity matrix
(bcf = qr.solve(Z,y))

### treatment coding
lm(y~xf, contrasts=list(xf="contr.treatment"))
Tr = cbind(1, contr.treatment(p))
solve(Tr,bcf)
### Helmert coding
lm(y~xf, contrasts=list(xf="contr.helmert"))
Hm = cbind(1, contr.helmert(p))
solve(Hm,bcf)
### Polynomial coding
lm(y~xf, contrasts=list(xf="contr.poly"))
Pn = cbind(1, contr.poly(p))
solve(Pn,bcf)
### Sum-to-zero coding
lm(y~xf, contrasts=list(xf="contr.sum"))
Sz = cbind(1, contr.sum(p))
solve(Sz,bcf)

## contrast matrix in R
contr.helmert(4)
contr.poly(4)
contr.treatment(4)

