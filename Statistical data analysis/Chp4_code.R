## readin data, R can handle http input
toluca = read.table("toluca.txt",header=TRUE)
y = toluca$WorkHour; x = toluca$LotSize
n = length(x); alpha = 0.05

## Joint confidence interval of \beta_0 and \beta_1
mux = mean(x); muy = mean(y)
s2x = sum((x-mux)^2)/(n-1); s2y = sum((y-muy)^2)/(n-1)
sxy = sum((x-mux)*(y-muy))/(n-1)

b1 = sxy/s2x; b0 = muy-mux*b1

resid = y-(b0+b1*x); mse = sum(resid^2)/(n-2)
sig = sqrt(mse)

sb1 = sig/sqrt(s2x*(n-1))
sb0 = sig*sqrt(1/n+mux^2/(s2x*(n-1)))

crt = qt(1-alpha/4,23)
b0+c(-1,1)*crt*sb0; b1+c(-1,1)*crt*sb1

### Simultaneous Mean Estimation
Xh = c(30,65,100); Yh = b0+b1*Xh
syh = sig*sqrt(1/n+(Xh-mux)^2/(s2x*(n-1)))
## Working-Hotelling
w = sqrt(2*qf(1-alpha,2,n-2))
cbind(Yh-w*syh, Yh + w*syh)
## Bonferroni
B = qt(1-alpha/6,n-2)
cbind(Yh-B*syh, Yh + B*syh)

### Simultaneous Prediction
syp = sig*sqrt(1+1/n+(Xh-mux)^2/(s2x*(n-1)))
## Sheffe
sf =  sqrt(3*qf(1-alpha,3,n-2))
cbind(Yh-sf*syp, Yh + sf*syp)
## Bonferroni
cbind(Yh-B*syp, Yh + B*syp)

### Compare Working-Hotelling, Sheffe <-> Bonferroni
## postscript(file="Ch04-01.ps", width=6, height=4, onefile=FALSE, horizontal=FALSE)
## pdf(file="Ch04-01.pdf", width=6, height=4)
par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), cex=0.7, lwd=2)
curve(sqrt(2*qf(1-x,2,23)), 0,0.1, xlab=expression(alpha),
      ylab="Critical Values (m=3)", ylim=c(2,5))
m = 3; curve(qt(1-x/2/m,23), add=TRUE, col=2, lty=2)
curve(sqrt(m*qf(1-x,m,23)), ad=TRUE, col=3, lty=3)
legend(0.034,5, c("Sheffe", "Working-Hotelling", "Bonferroni"),
       col=c(3,1,2), lty=c(3,1,2), bty="n", pt.cex=0.5)
curve(sqrt(2*qf(1-x,2,23)), 0,0.1, xlab=expression(alpha),
      ylab="Critical Values (m=6)", ylim=c(2,5))
m = 6; curve(qt(1-x/2/m,23), add=TRUE, col=2, lty=2)
curve(sqrt(m*qf(1-x,m,23)), ad=TRUE, col=3, lty=3)
legend(0.034,5, c("Sheffe", "Bonferroni", "Working-Hotelling"),
       col=3:1, lty=3:1, bty="n", pt.cex=0.5)
## dev.off()

### regression through the origin
lm(y~x-1); b1 = sum(x*y)/sum(x^2)
res3 = y - b1*x
sum(y^2); sum(b1^2*x^2)+sum(res3^2)

### Inverse prediction
Yh = c(200,300,400)
Xh = (Yh-b0)/b1
SpredX = sig/b1*sqrt(1+1/n+(Xh-mux)^2/(s2x*(n-1)))
B = qt(1-alpha/6,n-2); S = sqrt(3*qf(1-alpha,3,n-2))
rbind(Xh - B*SpredX, Xh + B*SpredX)
rbind(Xh - S*SpredX, Xh + S*SpredX)

qt(1-alpha/2,n-2)^2*sig^2/b1^2/(s2x*(n-2))

### Inverse regression v.s. Inverse prediction
xD = (x-mean(x))/sd(x); yD = (y-mean(y))/sd(y)
r = cor(x,y); lm(yD ~ xD)

prcomp(cbind(xD,yD))$rot[,1]

par(mfrow=c(1,1))
plot(xD, yD, pch=20)
points(0,0, pch="x", col=5)
abline(0,r, col=2, lty=2)
abline(0,1/r, col=3, lty=2)
abline(0,1, col=4)
