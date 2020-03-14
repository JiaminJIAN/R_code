
###### Extra SSR

x = matrix( rnorm(100*5), 100, 5 )
bcf = sample(1:14, size=5)
y = x%*%bcf + rnorm(100)

pairs(cbind(y,x),labels=c("y",paste("x",1:5,sep="")))

### simple regression
reg1 = lm(y ~ x[,1])
res1 = reg1$res

## project x2 into 1 and x1 using regression
e2 = lm(x[,2] ~ x[,1])$res

## extra sum of squares is to project the residuals into the orthogonal part of x2
reg2 = lm(res1 ~ e2)
res2 = reg2$res

### decompose SSR
reg12 = lm(y ~ x[,1:2])
anova(reg2)[1,2] + anova(reg1)[1,2]; anova(reg12)[1,2]

## regression coefficients
reg2$coef[2]; reg12$coef[3]

## the same process as QR decomposition

##########  Multi-collinearity
##########  Uncorrelated predictors: controlled experiment

crew = read.table("http://www.stat.columbia.edu/~jwang/w4315/data/WorkCrew.txt")

x1 = crew[,1]; x2 = crew[,2]; y = crew[,3]
##### x1, x2: uncorrelated
cor(x1,x2)
## or
sum((x1-mean(x1))*(x2-mean(x2)))
### Check the ANOVA
anova(lm(y ~ x1))[1,2]; anova(lm(y ~ x2))[1,2]
anova(lm(y ~ x1+x2))[1:2,2]
### look at the centered design matrix
X = cbind(1, scale(crew[,1:2],center=TRUE,scale=FALSE) )
t(X)%*%X; t(X)%*%y

##########  Big correlation: body fat example

bf = read.table("http://www.stat.columbia.edu/~jwang/w4315/data/BodyFat.txt", header=TRUE)
pairs(bf)
### marginal correlations: all positive
cor(bf)
### regression summary
bf.reg = lm(Fat ~ ., data=bf)
summary(bf.reg)

## using residuals idea to do regression
resY = lm(bf[,4] ~ bf[,2]+bf[,3])$res
## or resY = lm(Fat ~ ., data=bf[,-1])$res
resX = lm(bf[,1] ~ bf[,2]+bf[,3])$res
reg1 = lm(resY ~ resX-1)
summary(reg1)
anova(reg1)[1,2]

## using residuals idea to do regression
resY = lm(bf[,4] ~ bf[,2])$res
## or resY = lm(Fat ~ ., data=bf[,-1])$res
resX = lm(bf[,1] ~ bf[,2])$res
reg2 = lm(resY ~ resX-1)
summary(reg2)
anova(reg2)[1,2]

## using residuals idea to do regression
resY = lm(bf[,4] ~ bf[,3])$res
## or resY = lm(Fat ~ ., data=bf[,-1])$res
resX = lm(bf[,1] ~ bf[,3])$res
reg3 = lm(resY ~ resX-1)
summary(reg3)
anova(reg3)[1,2]

## compared to the marginal regression
reg4 = lm(bf[,4] ~ bf[,1])
anova(reg4)[1,2]


##### mean response/prediction
n = dim(bf)[1]
xh = c(25,50,29)
## one predictor
xh1 = c(1,25)
X1 = cbind(1,bf[,1])
reg1 = lm(bf[,4] ~ bf[,1])
mse1 = sum(reg1$res^2)/(n-2)
yh1 = sum( reg1$coef*xh1 )
syh1 = sqrt(xh1%*%solve(t(X1)%*%X1)%*%xh1*mse1)
yh1;syh1

## two predictors
xh2 = c(1,25,50)
X2 = as.matrix( cbind(1,bf[,1:2]) )
reg2 = lm(bf[,4] ~ bf[,1]+bf[,2])
mse2 = sum(reg2$res^2)/(n-2)
yh2 = sum( reg2$coef*xh2 )
syh2 = sqrt(xh2%*%solve(t(X2)%*%X2)%*%xh2*mse2)
yh2; syh2

### three predictors
xh3 = c(1,xh)
X3 = as.matrix( cbind(1,bf[,1:3]) )
reg3 = lm(Fat ~ ., data=bf)
mse3 = sum(reg3$res^2)/(n-2)
yh3 = sum( reg3$coef*xh3 )
syh3 = sqrt(xh3%*%solve(t(X3)%*%X3)%*%xh3*mse3)
yh3; syh3


### marginal test vs. combined test
summary(bf.reg)
a = anova(bf.reg)
(a[1,2]+a[2,2])/2/a[4,3]

summary(lm(bf[,4] ~ bf[,1]+bf[,2]))
(a[1,2]+a[2,2])/2/((a[4,2]+a[3,2])/(n-3))

### look at the correation matrix inverse
cr3 = cor(bf[,1:3])
solve( cr3 )


############### Polynomial regression

## polynomial regression example
pc = read.table("http://www.stat.columbia.edu/~jwang/w4315/data/PowerCell.txt", header=TRUE)
y = pc[,1]; x = pc[,2:3]

### creat ordinary polynomial matrix
xp = cbind(1, x, x^2, x[,1]*x[,2])
round( cor(xp[,-1]), 5 )

### centered matrix: much reduced correlation
xc = scale(x, center=TRUE, scale=FALSE)
xcp = cbind(1, xc, xc^2, xc[,1]*xc[,2])
round( cor(xcp[,-1]), 5 )

### regression
creg = lm( y ~ xcp[,2]+xcp[,3]+xcp[,4]+xcp[,5]+xcp[,6])
## or lm(y ~ xcp-1 )
res = creg$res

par(mfrow=c(2,2))
plot(res ~ creg$fit)
plot(res ~ xcp[,2])
plot(res ~ xcp[,3])
qqnorm(res); qqline(res, col=2)

sse = sum(res^2)
a = x[,1]*100+x[,2]; tmp = a[duplicated(a)][1]
yrep = y[a==tmp]
sspe = sum( (yrep-mean(yrep))^2 )

den = (sse-sspe)/(dim(xcp)[1]-dim(xcp)[2]-length(yrep)+1)
nom = sspe/(length(yrep)-1)
den/nom
qf(1-0.05, dim(xcp)[1]-dim(xcp)[2]-length(yrep)+1, length(yrep)-1)
pt(den/nom, dim(xcp)[1]-dim(xcp)[2]-length(yrep)+1, length(yrep)-1)

### partial F-test: extra SSR
a = anova(creg)
sum(a[3:5,2])/3/(sse/(dim(xcp)[1]-dim(xcp)[2]))
qf(1-0.05, 3, dim(xcp)[1]-dim(xcp)[2])

############### Interaction modeling

### interaction modeling
x1 = seq(0, 4, length=1e2)
x2 = seq(0, 5, length=1e2)
y = outer(x1, x2, function(x1,x2) 1+2*x1+4*x2-5*x1*x2)
persp(x1,x2, y)

library(scatterplot3d)
x12 = expand.grid(x1,x2)
scatterplot3d(x12[,1],x12[,2], y, type="l", angle=120)

contour(x1,x2, y)

x1 = seq(0, 4, length=1e2)
x2 = seq(1,10, length=1e2)
x12 = expand.grid(x1,x2)
y = 165+2.6*x12[,1]+5.3*log(x12[,2])-0.6*x12[,1]*log(x12[,2])
persp(x1,x2, matrix(y,1e2,1e2))
contour(x1,x2, matrix(y,1e2,1e2))

scatterplot3d(x12[,1],x12[,2], y, type="l", angle=120)

