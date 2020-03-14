## readin the data using function 'read.table()'
toluca = read.table("toluca.txt", header=T)
## look at the dataset
toluca
## names for the variables
names(toluca)
## names(toluca) = c("LotSize", "WorkHours")
toluca$WorkHours
WorkHours
## attach the dataset in the search path, so that
##  we can access the variables directly using their names
attach(toluca)
WorkHours
## Look at some summary statistics for the variable WorkHours
summary(WorkHours)
boxplot(WorkHours)
fivenum(WorkHours)
## Looking for help on some command using '?'
?fivenum
## QQ-plot to check the normality of WorkHours distribution
qqnorm(WorkHours)
qqline(WorkHours)

## Regression coefficient calculation
mean(LotSize); mean(WorkHours)
sum(LotSize*WorkHours)
sum(LotSize^2); sum(WorkHours^2)

nom = sum( (LotSize-mean(LotSize))*(WorkHours-mean(WorkHours)) )
den = sum( (LotSize-mean(LotSize))^2 )
b1 = nom/den
b0 = mean(WorkHours)-mean(LotSize)*b1

resid = WorkHours - (b0+b1*LotSize)
sum(resid); sum(resid*LotSize)
mse = sum(resid^2)/(length(LotSize)-2)

## Do linear regression in R using command 'lm'
toluca.lm = lm(WorkHours ~ LotSize)
toluca.lm
## summary will produce more information on the linear regression subject
summary(toluca.lm)

## Visual exploration of the data: scatter plot
plot(LotSize, WorkHours, pch=1)
plot(LotSize, WorkHours, pch=20)
abline( toluca.lm, col=2, lty=2, lwd=2)
legend(20, 500, expression(y=beta[0]+beta[1]*x), col=2, lwd=2, lty=2)
legend(20, 400, legend=c("y=62.366+3.57x"), col=2, lwd=2, lty=2)
## do mathematical annotations in R plot
?plotmath
## Do some model diagnostics
names(toluca.lm)
toluca.lm[[5]]
plot(toluca.lm[[5]], toluca.lm[[2]], pch=20)
abline(h=0, lty=3)
a = lowess(toluca.lm[[5]], toluca.lm[[2]])
lines(a, col=4)
