Auto <- read.table("Auto.data", header = T, na.strings = "?",
                   stringsAsFactors = T)
head(Auto)
dim(Auto)
Auto[1:4, ]
Auto <- na.omit(Auto)
dim(Auto)
names(Auto)
str(Auto)
summary("displacement")
# get means for variables in data frame Auto
# excluding missing values
sapply(Auto)
sapply(Auto[,1:7], range)
sapply(Auto[,1:7], sd)
NewAuto <- Auto[-c(10,85),]
sapply(NewAuto[,1:7], sd)

cor(Auto[,1:7])
pairs(Auto[,1:7])
pairs(~mpg+horsepower+weight+displacement, data=Auto,panel=panel.smooth)

#create_name = lm(Y~X, data=dataset)
simple.fit = lm(mpg~horsepower, data=Auto)
simple.fit
summary(simple.fit)
