Auto <- read.table("Auto.data", header = T, na.strings = "?",
                   stringsAsFactors = T)

head(Auto)
dim(Auto)
Auto[1:4, ]
Auto <- na.omit(Auto)
dim(Auto)
names(Auto)

attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)

cor(Auto[, -9])
pairs(Auto) 

train = (year%%2 == 0)  
test = !train
Auto.train = Auto[train, ]
Auto.test = Auto[test, ]
mpg01.test = mpg01[test]


# LDA
library(MASS)
lda.fit = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
lda.pred = predict(lda.fit, Auto.test)
mean(lda.pred$class != mpg01.test)


qda.fit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
qda.pred = predict(qda.fit, Auto.test)
mean(qda.pred$class != mpg01.test)



glm.fit = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              family = binomial, subset = train)
glm.probs = predict(glm.fit, Auto.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != mpg01.test)


library(class)
train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[test, ]
train.mpg01 = mpg01[train]
set.seed(1)

naiveBayes.pred = NB(train.X, test.X, train.mpg01)
mean(naiveBayes.pred != mpg01.test)

#k=10
naiveBayes.pred = NB(train.X, test.X, train.mpg01)
mean(naiveBayes.pred != mpg01.test)
#k=100
knn.pred = knn(train.X, test.X, train.mpg01, k = 100)
mean(knn.pred != mpg01.test)

#Part a
Auto$mpg01 <- factor(as.numeric(Auto$mpg > median(Auto$mpg)))
table(Auto$mpg01)


#Part b
cor(Auto[, -9])
Auto$name <- NULL
Auto$mpg <- NULL

Auto$origin <- factor(Auto$origin, labels = c("American", "European", "Japanese"))
str(Auto)
library("ggplot")
install.packages("ggplot")
g1 <- ggplot(Auto, aes(x = mpg01, y = cylinders, col = mpg01)) + 
  geom_jitter() + 
  theme(legend.position = "none") + 
  ggtitle("Cylinders vs mpg01 - Jitter Plot") 

g2 <- ggplot(Auto, aes(x = mpg01, y = displacement, fill = mpg01)) + 
  geom_boxplot() + 
  theme(legend.position = "none") + 
  ggtitle("Displacement vs mpg01 - Boxplot")

g3 <- ggplot(Auto, aes(x = mpg01, y = horsepower, fill = mpg01)) + 
  geom_boxplot() + 
  theme(legend.position = "none") + 
  ggtitle("Horsepower vs mpg01 - Boxplot")

g4 <- ggplot(Auto, aes(x = mpg01, y = weight, fill = mpg01)) + 
  geom_boxplot() + 
  theme(legend.position = "none") + 
  ggtitle("Weight vs mpg01 - Boxplot")

g5 <- ggplot(Auto, aes(x = mpg01, y = acceleration, fill = mpg01)) + 
  geom_boxplot() + 
  theme(legend.position = "none") + 
  ggtitle("Acceleration vs mpg01 - Boxplot")

g6 <- ggplot(Auto, aes(x = mpg01, y = year, fill = mpg01)) + 
  geom_boxplot() + 
  theme(legend.position = "none") + 
  ggtitle("Year vs mpg01 - Boxplot")

g7 <- ggplot(Auto, aes(x = origin, fill = mpg01)) + 
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.title.y = element_blank()) + 
  ggtitle("Origin vs mpg01 - Bar Plot") 


grid.arrange(g1, g2, g3, g4, g5, g6, g7, 
             ncol = 2, 
             nrow = 4)

attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)



nb.Afit = naiveBayes(mpg01~cylinders + displacement + weight + horsepower, data = Auto, subset = train)
nb.Aclass = predict(nb.Afit, Auto.test)
mean(nb.Aclass!= mpg01.test)


train.auto = cbind(cylinders, displacement, weight, horsepower)[train.A,]
test.auto = cbind(cylinders, displacement, weight, horsepower)[!train.A,]
train.mpg01 = mpg01[train.A]
set.seed(1)
knn.autopred = knn(train.auto, test.auto, train.mpg01, k = 1)
mean(knn.autopred != mpg01.test)