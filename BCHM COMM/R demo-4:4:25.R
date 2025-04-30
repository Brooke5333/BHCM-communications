pca <- prcomp(iris[,-5])
summary(pca)
biplot(pca)

library(ggfortify)
autoplot(pca, data = iris, colour = 'Species') +
  theme_classic()


library(e1071)
n <- nrow(iris) # Number of observations
ntrain <- round(n*0.75) # 75% for training set
set.seed(500) # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create a random index
train_iris <- iris[tindex,] # Create training set
test_iris <- iris[-tindex,] # Create test set
svm1 <- svm(Species~., data=train_iris,
            method="C-classification", kernal="radial",
            gamma=0.1, cost=10)
summary(svm1)
plot(svm1, train_iris, Petal.Width ~ Petal.Length,
     slice=list(Sepal.Width=3, Sepal.Length=4))
prediction <- predict(svm1, test_iris)
xtab <- table(test_iris$Species, prediction)
xtab

precision <- (12+15+9)/(12+15+9+2)
