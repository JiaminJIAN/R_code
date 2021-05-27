## DA502/MA543, Homework 1
## Jiamin JIAN

#######################################################################
## Exercise 4: Section 2.4, page 54-55, question 8

## (a) Read the data
college = read.csv('/Users/jianjiamin/Code/R_code/Dataset/College.csv')
head(college[, 1:5])

## (b) Look at the data using the fix function

rownames(college) = college[,1]
#fix(college)
college = college[,-1]
#fix(college)
head(college[, 1:5])

## (c) 

# i
summary(college)
# ii.
pairs(college[,1:10])
# iii.
plot(college$Private, college$Outstate, xlab = "Private University", ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")
# iv.
Elite = rep("No", nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab = "Elite University", ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")
# v.
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$perc.alumni, col=2)
hist(college$S.F.Ratio, col=3, breaks=10)
hist(college$Expend, breaks=100)
# v.
par(mfrow = c(2,2))
hist(college$Books, col = 2, xlab = "Books", ylab = "Count")
hist(college$PhD, col = 3, xlab = "PhD", ylab = "Count")
hist(college$Grad.Rate, col = 4, xlab = "Grad Rate", ylab = "Count")
hist(college$perc.alumni, col = 6, xlab = "% alumni", ylab = "Count")
# vi.
par(mfrow=c(1,1))
plot(college$Outstate, college$Grad.Rate)
plot(college$Accept / college$Apps, college$S.F.Ratio)
plot(college$Top10perc, college$Grad.Rate)


#############################################################################
## Exercise 5: Section 2.4, page 56, question 9

## load the data

Auto = read.csv('/Users/jianjiamin/Code/R_code/Dataset/Auto.csv')
Auto$horsepower = as.numeric(as.character(Auto$horsepower))
Auto = na.omit(Auto)
dim(Auto)
summary(Auto)

head(Auto)

# (a)
# quantitative: mpg, cylinders, displacement, horsepower, weight, acceleration, year
# qualitative: origin, name

# (b) The range of each quantitative predictor
qualitative_columns <- c(8, 9)
sapply(Auto[, -qualitative_columns], range)

# (c) The mean and standard deviation of each quantitative predictor
sapply(Auto[, -qualitative_columns], mean)
sapply(Auto[, -qualitative_columns], sd)

# (d)
sapply( Auto[-seq(10,85),-qualitative_columns], range)
sapply( Auto[-seq(10,85),-qualitative_columns], mean)
sapply( Auto[-seq(10,85),-qualitative_columns], sd)

# (e) 
Auto$cylinders <- as.factor(Auto$cylinders)
Auto$year <- as.factor(Auto$year)
Auto$origin <- as.factor(Auto$origin)
pairs(Auto)

# (f)
Auto$horsepower <- as.numeric(Auto$horsepower)
cor(Auto$weight, Auto$horsepower)
cor(Auto$weight, Auto$displacement)
cor(Auto$displacement, Auto$horsepower)


########################################################################
## Exercise 9: Section 3.7, page 121-122, question 8

# (a)
Auto = read.csv('/Users/jianjiamin/Code/R_code/Dataset/Auto.csv')
Auto$horsepower = as.numeric(as.character(Auto$horsepower))
Auto = na.omit(Auto)
dim(Auto)
summary(Auto)


fit <- lm(mpg ~ horsepower, data = Auto)
summary(fit)

# (a) (iv) 
predict(fit, data.frame(horsepower = 98), interval = "confidence")
predict(fit, data.frame(horsepower = 98), interval = "prediction")

# (b) 
plot(Auto$horsepower, Auto$mpg, main = "Scatterplot of mpg vs. horsepower", xlab = "horsepower", ylab = "mpg", col = "blue")
abline(fit, col = "red")

# (c)
par(mfrow = c(2, 2))
plot(fit)



########################################################################
## Exercise 10: Section 3.7, page 122, question 9

# (a)
pairs(Auto)

# (b)
cor(Auto[1:8])

# (c)
fit2 <- lm(mpg ~.- name, data = Auto)
summary(fit2)

# (d)
par(mfrow = c(2, 2))
plot(fit2)

# (e)
fit3 <- lm(mpg ~ cylinders * displacement+displacement * weight, data = Auto[, 1:8])
summary(fit3)

# (f)
par(mfrow = c(2, 2))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)

fit4 <- lm(mpg~log(horsepower), data = Auto)
summary(fit4)

fit5 <- lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2), data = Auto)
summary(fit5)
par(mfrow=c(2,2))
plot(fit5)
plot(predict(fit5), rstudent(fit5))

fit6<-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(fit6)

par(mfrow=c(2,2)) 
plot(fit6)
plot(predict(fit6),rstudent(fit6))






