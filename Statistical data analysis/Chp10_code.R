#######################################################################################################
############################R codes for some of the example in the textbook############################
#######################################################################################################

life = read.table("LifeIns.txt", header=TRUE)
pairs(life)
cor(life)
reg1 = lm(Ins ~ Income + Risk, data=life)
reg1
lm(Ins ~ Risk, data=life)
resY = .Last.value$res
lm(Income ~ Risk, data=life)
resX1 = .Last.value$res
reg2 = lm(Ins ~ Risk + Income + I(Income^2), data=life)
resX1.2 = lm(I(Income^2) ~ Risk + Income, data=life)$res
par(mfrow=c(2,2), pch=20)
plot(life$Income, reg1$res)
abline(h=0, lty=2)
plot(resX1, resY)
abline(h=0, lty=2)
abline(0, reg1$coef[2], col=2, lty=1)
identify(resX1, resY)
plot(resX1, reg1$res)
abline(h=0, lty=2)
plot(resX1.2, reg1$res)
abline(h=0, lty=2)
abline(0, reg2$coef[4], col=2, lty=1)
anova(reg2)
### Figure 10.4
body = read.table("BodyFat.txt", header=TRUE)
pairs(body)
cor(body)
reg1 = lm(Fat ~ Tricep + Thigh, data=body)
summary(reg1)
resY1 = lm(Fat ~ Tricep, data=body)$res
resY2 = lm(Fat ~ Thigh, data=body)$res
resX1 = lm(Thigh ~ Tricep, data=body)$res
resX2 = lm(Tricep ~ Thigh, data=body)$res
par(mfcol=c(2,2), pch=20)
plot(reg1$res ~ body$Tricep)
abline(h=0, lty=2)
plot(reg1$res ~ body$Thigh)
abline(h=0, lty=2)
plot(resX2, resY2)
abline(h=0, lty=2)
abline(0,reg1$coef[2], col=2, lty=2)
plot(resX1, resY1)
abline(h=0, lty=2)
abline(0,reg1$coef[3], col=2, lty=2)
### Table 10.3
e = reg1$res
Xs = as.matrix(cbind(1,body[,1:2]))
H = Xs%*%solve(t(Xs)%*%Xs)%*%t(Xs)
h = diag(H)
n = dim(body)[1]
di = e/(1-h)
ti = e*sqrt((n-4)/(sum(e^2)*(1-h)-e^2))
cbind(1:n, e, h, di, ti)
### Figure 10.6
x1 = c(14,19,12,11)
x2 = c(25,32,22,15)
Xs = as.matrix(cbind(1,x1,x2))
H = Xs%*%solve(t(Xs)%*%Xs)%*%t(Xs)
h = diag(H)
y = c(301,327,246,187)
par(mfrow=c(1,1))
plot(x1,x2, pch=20)
points(mean(x1), mean(x2), pch=22, col=2, cex=3)
text(x1,x2, pos=2)
text(x1,x2, label=round(h,4), col=2, pos=4)
### Figure 10.7
plot(body$Tricep, body$Thigh, pch=" ")
text(body$Tricep, body$Thigh)
#### table 10.4
reg1 = lm(Fat ~ Tricep + Thigh, data=body)
summary(reg1)
e = reg1$res
Xs = as.matrix(cbind(1,body[,1:2]))
H = Xs%*%solve(t(Xs)%*%Xs)%*%t(Xs)
h = diag(H)
n = dim(body)[1]
di = e/(1-h)
ti = e*sqrt((n-4)/(sum(e^2)*(1-h)-e^2))
dfi = ti*sqrt(h/(1-h))
cook = e^2/sum(e^2)*(n-3)/3*h/(1-h)^2
round( cbind(1:n,dfi,cook), 3 )
db = matrix(0,n,3)
cx = diag( solve(t(Xs)%*%Xs) )
for(i in 1:n){
  tmp = lm(Fat ~ Tricep + Thigh, data=body[-i,])
  nom = reg1$coef - tmp$coef
  den = sqrt(sum(tmp$res^2)/(n-1-3)*cx)
  db[i,] = nom/den
}
round( cbind(1:n,dfi,cook, db), 3 )
### Figure 10.8
par(mfrow=c(1,2), pch=20)
plot(reg1$fit, reg1$res, cex=cook/median(cook) )
plot(cook, type="o", cex=2, yaxs="i")
### Influences on Inferences
reg1 = lm(Fat ~ Tricep + Thigh, data=body)
reg3 = lm(Fat ~ Tricep + Thigh, data=body[-3,])
X = as.matrix(cbind(1,body[,1:2]))
mean( abs(as.vector(X%*%reg3$coef)/reg1$fit-1) )*100

#######################################################################################################
##################################### Outlying measure diagnostics ####################################
#######################################################################################################

dsu = read.table("dwaine.txt", head=TRUE)
pairs(dsu)
y = dsu[,3]; x1 = dsu[,1]; x2 = dsu[,2]
summary(lm(y ~ x1+x2))
res1 = lm(y ~ x1)$res
res2 = lm(x2 ~ x1)$res
plot(res1 ~ res2)
abline(0, 9.3655, col=2)
plot(y ~ res2)
abline(lm(y ~ res2), col=2)
lm(y ~ res2)
## Euclidean vs. Mahalanobis distance
plot(x1,x2)
cor(x1,x2)
xm = cbind(1,x1,x2)
H = xm%*%solve(t(xm)%*%xm, t(xm))
diag(H)
points(mean(x1), mean(x2), pch=20, cex=2, col=2)
x1c  = x1 - mean(x1)
x2c  = x2 - mean(x2)
1+x1c^2+x2c^2
identify(x1,x2, label=round(diag(H),3), col=3)
identify(x1,x2, label=round(1+x1c^2+x2c^2,3), col=4)
#######################
fit = lm(y ~ x1+x2)
X = cbind(1, x1,x2)
H = X%*%solve(t(X)%*%X, t(X))
i  = 1
fiti = lm(y[-i] ~ x1[-i]+x2[-i])
di = y[i] - sum(c(1,x1[i],x2[i])*fiti$coef)
fit$res[i]/(1-diag(H)[i])
fit$fitted - c(y[i]-di, fiti$fit)
fit$res[i]/(1-diag(H)[i])*H[i,]
cor(fit$fitted - c(y[i]-di, fiti$fit), fit$res[i]/(1-diag(H)[i])*H[i,])
fiti$coef+di*solve(t(X)%*%X, X[i,])
fit$coef
