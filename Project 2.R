Auto <- read.table("Auto.data", header = T, na.strings = "?",
                   stringsAsFactors = T)
head(Auto)
dim(Auto)
Auto[1:4, ]
Auto <- na.omit(Auto)
dim(Auto)
names(Auto)
str(Auto)

df = Auto
df$mpg01 = NA
median_mpg = median(df$mpg)

for(i in 1:dim(df)[1]){
  if (df$mpg[i] > median_mpg){
    df$mpg01[i] = 1
  }else{
    df$mpg01[i] = 0
  }
}


movetolast = function(data, move) {
  data[c(setdiff(names(data), move), move)]
}

f = movetolast(df, c("name"))
pairs(df[,1:9])



#Linear Regression 
#create_name = lm(Y~X, data=dataset)
simple.fit = lm(mpg~horsepower, data=Auto)
simple.fit
summary(simple.fit)

#question 8 part iv:
predict(simple.fit, data.frame(horsepower = 98), interval = "confidence", level = 0.95)
#prediction interval:
predict(simple.fit, data.frame(horsepower = 98), interval = "prediction", level = 0.95)

#question part b:
#plot(horsepower, mpg) for some reason this didn't work
library(ggplot2)
ggplot(Auto, aes(x = horsepower, y = mpg)) + geom_point() + geom_abline(intercept = coef(simple.fit)[1], slope = coef(simple.fit)[2], col = "red")
#question 8 part c
diagPlts<-diagPlot(simple.fit)
gg_diagnose(simple.fit)
plot(simple.fit)

#Question 11:
set.seed(1)

x = rnorm (100)

y = 2*x + rnorm(100)
#Part a:
lm_1 <- lm(y ~ x + 0)
summary(lm_1)
#Part b:
lm_2 <- lm(x ~ y + 0)
summary(lm_2)
#part d:
set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)

lm_1 <- lm(y ~ x + 0)
summary(lm_1)

T_statistic <- (sqrt(length(x) - 1) * sum(x*y)) / sqrt(sum(x^2) * sum(y^2) - sum(x*y)^2)
T_statistic
round(as.numeric(T_statistic), 10) == round(as.numeric(summary(lm_1)$coefficients[3]), 10)

set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)

lm_3 <- lm(y ~ x)
summary(lm_3)$coefficients

lm_4 <- lm(x ~ y)
summary(lm_4)$coefficients

set.seed(1)
x1=runif (100)
x2=0.5*x1+rnorm (100)/10
y=2+2*x1+0.3*x2+rnorm (100)            

cor(x1, x2)
ggplot(mapping = aes(x = x1, y = x2)) + 
  geom_point()

lm_01 <- lm(y ~ x1 + x2)

summary(lm_01)

lm_02 <- lm(y ~ x1)

summary(lm_02)

lm_03 <- lm(y ~ x2)

summary(lm_03)

x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

data <- data.frame(y, x1, x2, new = 0)
data$new[length(data$new)] <- 1

ggplot(data, aes(x = x1, y = x2, col = factor(new))) + 
  geom_point() + 
  scale_color_manual(values = c("black", "red")) + 
  theme(legend.position = "none")

ggplot(data, aes(x = x1, y = y, col = factor(new))) + 
  geom_point() + 
  scale_color_manual(values = c("black", "red")) + 
  theme(legend.position = "none")


ggplot(data, aes(x = x2, y = y, col = factor(new))) + 
  geom_point() + 
  scale_color_manual(values = c("black", "red")) + 
  theme(legend.position = "none")

ggplot(data,aes(x = .hat, y = .std.resid, col = factor(new))) + 
  geom_point() +
  scale_color_manual(values = c("black", "red"))
