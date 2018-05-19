
## K-Nearest neighbors Lab
## RSH 5/28/2018 - MSDS Predictive Modeling

library(class)
library(caret)

#import step
data <- read.csv('Lab1/balance.csv', head=FALSE, sep=',')

#center and scale data
preProcess(data, method = c("center", "scale"))

#Random split 80/20 test and train data
t1 <- sample(1:625,500)
t2 <- setdiff(1:625, t1)

# extract the class label from the training data
# labels
cl <- data[t1,]$V1

#Extract the training data an test data 
train <- subset(data[t1,], select =- V1)
test <- subset(data[t2,], select =- V1)

#run knn function, setting k = 3 for the first example. 
#we can optimize k = i where is maximium values
pred <- knn(train, test, cl, k=3, prob = FALSE, use.all=TRUE)

#CF to look at first results
confusionMatrix(pred, data[t2,]$V1)


### Part 2! 
## Grid Searching dimentional landscape for optimal k.

ktune <- train(train, cl, method = "knn", 
               tuneGrid = data.frame(.k=1:20), #searching 1 - 20
               trControl = trainControl(method='cv'))

# Print out results from Grid Search 
ktune

#Next prediction, using ktune parms and again the test data
pred2 <- predict(ktune, test)

#veiw results, accuracy, significance, performance, and senstivity, etc..
confusionMatrix(pred2, data[t2,]$V1)

saveRDS(ktune, file = 'ktune.rds') #save model for future reference

# to reload model 
# to predict 
reload = readRDS('ktune.rds')
predict(reload, test) #model can be used on new data with OG model. 