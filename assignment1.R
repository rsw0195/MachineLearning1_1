rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}

needed  <-  c("class", "FNN", "Metrics")  #class contains the knn() function
installIfAbsentAndLoad(needed)

###############################
########  Question 1  #########
###############################

homedata <- read.table("HomePrices.txt", header=TRUE)

medvmean <- mean(homedata$medv)
homedata$diff <- (homedata$medv - medvmean)^2
meansquareerror <- mean(homedata$diff)
meansquareerror

medvar <- var(homedata$medv)*(length(homedata$medv)-1)/length(homedata$medv)
medvar

homedatascale <- data.frame(scale(homedata[1:12], center = T, scale = T), cbind(medv = homedata$medv))

head(homedatascale, 6)

set.seed(5072)
trainprop <- 0.75
validprop <- 0.15

n <- nrow(homedatascale)

train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validprop * n) 
test <- setdiff(setdiff(1:n, train), validate)

head(train,1)
head(validate, 1)
head(test,1)

trainset <- homedatascale[train,]
validateset <- homedatascale[validate,]
testset <- homedatascale[test,]


train.x <- trainset[-13]
validate.x <- validateset[-13]
test.x <- testset[-13]
train.y <- trainset$medv
validate.y <- validateset$medv
test.y <- testset$medv

numlist <- seq(19, 1, -2)
numreps <- length(numlist)
validate.errors <- rep(length(numreps))
train.errors <- rep(length(numreps))
for(i in 1:numreps) {
  knn.pred <- knn.reg(train.x, validate.x, train.y, k=numlist[i])
  validate.errors[i] <- mean((validate.y - knn.pred$pred)**2)
  
  knn.pred <- knn.reg(train.x, train.x, train.y, k = numlist[i])
  train.errors[i] <- mean((train.y - knn.pred$pred)**2)
}

plot(NULL, NULL, type = 'n', xlim = c(numreps, 1), ylim = range(0,26), xlab = 'Increasing Flexibility (Decreasing k)', ylab = 'Error Rate', main = 'Error Rate versus Flexibility', legend=TRUE)
lines(numreps:1, validate.errors[order(1:length(validate.errors))], type = 'b', pch = 16, col = 2)
lines(numreps:1, train.errors[order(1:length(train.errors))], type = 'b', pch =16, col = 1)
legend("topright", legend = c("Validation MSEs", "Training MSEs"), col=c(2,1), cex = .75, pch=16)

print (paste("Best validate MSE occured at k =", numlist[which.min(validate.errors)],
             "with a validation MSE of", validate.errors[which.min(validate.errors)]))
print (paste("Best train MSE occured at k =", numlist[which.min(train.errors)],
             "with a training MSE of", train.errors[which.min(train.errors)]))

knn.pred <- knn.reg(train.x, test.x, train.y, k = numlist[which.min(validate.errors)])
print(paste("Test MSE:", mean((test.y - knn.pred$pred)**2)))

###############################
#######  Question 2  ##########
###############################


loandata <- read.csv("LoanData.csv", header=TRUE)

loandatascale <- data.frame(scale(loandata[1:7], center = T, scale = T), loan.repaid = loandata$loan.repaid)
head(loandatascale, 6)

set.seed(5072)

trainprop <- 0.75
validprop <- 0.15

n <- nrow(loandatascale)

train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validprop * n) 
test <- setdiff(setdiff(1:n, train), validate)

head(train,1)
head(validate, 1)
head(test,1)

trainset <- loandatascale[train,]
validateset <- loandatascale[validate,]
testset <- loandatascale[test,]

train.x <- trainset[-8]
validate.x <- validateset[-8]
test.x <- testset[-8]
train.y <- trainset$loan.repaid
validate.y <- validateset$loan.repaid
test.y <- testset$loan.repaid


numreps <- 10
validate.errors <- rep(0,numreps)
train.errors <- rep(0, numreps)
for(k in 1:numreps) {
  i <- 2*k - 1
  knn.pred <- knn(train.x, validate.x, train.y, k=i)
  validate.errors[k] <- mean(validate.y != knn.pred)
  
  knn.pred <- knn(train.x, train.x,  train.y, k = i)
  train.errors[k] <- mean(train.y != knn.pred)
  print(i)
}

bestk <- 2*which.min(validate.errors)-1
besttraink <- 2*which.min(train.errors)-1

plot(NULL, NULL, type = 'n', xlim = c(2*numreps-1, 1), ylim = c(0, max(validate.errors, train.errors)), xlab = 'Increasing Flexibility (Decreasing k)', ylab = 'Error Rate', main = 'Error Rate versus Flexibility', legend=TRUE)
lines(2*seq(numreps,1)-1:1, validate.errors[length(validate.errors):1], type = 'b', pch = 16, col = 2)
lines(2*seq(numreps,1)-1:1, train.errors[length(train.errors):1], type = 'b', pch =16, col = 1)
legend("topleft", legend = c("Validation MSEs", "Training MSEs"), col=c(2,1), cex = .75, pch=16)

print (paste("Best validate MSE occured at k =", bestk,
             "with a validation MSE of", validate.errors[which.min(validate.errors)]))
print (paste("Best train MSE occured at k =", besttraink,
             "with a training MSE of", train.errors[which.min(train.errors)]))

knn.pred <- knn(train.x, test.x, train.y, k = bestk)
mytable <- table(test.y, knn.pred)
mytable
print(paste("Test error rate:", (mytable["Yes", "No"] + mytable["No", "Yes"]) / sum(mytable)))

#############################
#####  Question 3  ##########
#############################
set.seed(5072)

trainprop <- 0.75
validprop <- 0.15
n <- nrow(homedatascale)

validcerrors <- rep(0,50)
traincerrors <- rep(0,50)
testcerrors <- rep(0,50)
for(m in 1:50){
  
  train  <-  sample(n, trainprop * n)
  validate  <-  sample(setdiff(1:n, train), validprop * n) 
  test <- setdiff(setdiff(1:n, train), validate)
  
  head(train,1)
  head(validate, 1)
  head(test,1)
  
  trainset <- homedatascale[train,]
  validateset <- homedatascale[validate,]
  testset <- homedatascale[test,]
  
  
  train.x <- trainset[-13]
  validate.x <- validateset[-13]
  test.x <- testset[-13]
  train.y <- trainset$medv
  validate.y <- validateset$medv
  test.y <- testset$medv
  
  numreps <- 10
  validate.errors <- rep(0,numreps)
  train.errors <- rep(0, numreps)
  for(k in 1:numreps) {
    i <- 2*k - 1
    knn.pred <- knn.reg(train.x, validate.x, train.y, k=i)$pred
    validate.errors[k] <- mse(validate.y, knn.pred)
    
    knn.pred <- knn.reg(train.x, train.x,  train.y, k = i)$pred
    train.errors[k] <- mse(train.y, knn.pred)
    #print(i)
  }
  
  bestkvalid <- 2*which.min(validate.errors)-1
  bestktrain <- 2*which.min(train.errors)-1
  
  #print (paste("Best validate MSE occured at k =", bestkvalid,
               #"with a validation MSE of", validate.errors[which.min(validate.errors)]))
  #print (paste("Best train MSE occured at k =", bestktrain,
               #"with a training MSE of", train.errors[which.min(train.errors)]))
  
  knn.pred <- knn.reg(train.x, test.x, train.y, k = bestkvalid)$pred
  testmse <- mse(test.y, knn.pred)
  #print(paste("Test MSE:", testmse))
  
  
  validcerrors[m] <- validate.errors[which.min(validate.errors)]
  traincerrors[m] <- train.errors[which.min(train.errors)]
  testcerrors[m] <- testmse
}

mean.validate.errors <- mean(validcerrors)
sd.validate.errors <- sd(validcerrors)
mean.test.errors <- mean(testcerrors)
sd.test.errors <- sd(testcerrors)

plot(NULL, NULL, type = 'n', xlim = c(0,50), ylim = c(0, max(c(validcerrors, testcerrors))), xlab = 'Replication', ylab = 'MSE', main = 'Test and Best Validation for Many Partitionings of the Data', legend=TRUE)
lines(validcerrors, type = 'b', pch = 16, col = 2)
lines(testcerrors, type = 'b', pch =16, col = 1)
abline(h = mean.validate.errors, col = 2, lwd = 1, lty = 2)
abline(h = mean.test.errors, col = 1, lwd = 1, lty = 2)
legend("topright", legend = c("Validation MSE", "Mean Validation MSE", "Test MSE", "Mean Test MSE"), col=c(2,2,1,1), lty = c(1,2,1,2), cex = .75, pch=16)


#############################
#######  Question 4  ########
#############################

#use applications.train.csv
appdata <- read.csv("applications.train.csv", header=TRUE)

head(appdata)

set.seed(5072)
trainprop <- 0.80
validprop <- 0.20

n <- nrow(appdata)

train  <-  sample(n, trainprop * n)
validate  <-  sample(setdiff(1:n, train), validprop * n) 
test <- setdiff(setdiff(1:n, train), validate)

head(train,1)
head(validate, 1)
head(test,1)

trainset <- appdata[train,]
validateset <- appdata[validate,]
testset <- appdata[test,]


train.x <- trainset[-1]
validate.x <- validateset[-1]
test.x <- testset[-1]
train.y <- trainset[1]
validate.y <- validateset[1]
test.y <- testset[1]

numreps <- 10
validate.errors <- rep(0,numreps)
train.errors <- rep(0, numreps)
for(k in 1:numreps) {
  i <- 2*k - 1
  knn.pred <- knn.reg(train.x, validate.x, train.y, k=i)$pred
  validate.errors[k] <- mse(validate.y, knn.pred)
  
  knn.pred <- knn.reg(train.x, train.x,  train.y, k = i)$pred
  train.errors[k] <- mse(train.y, knn.pred)
  #print(i)
}

bestk <- which.min(validate.errors)

print(paste("Best validate error at k =",bestk))
