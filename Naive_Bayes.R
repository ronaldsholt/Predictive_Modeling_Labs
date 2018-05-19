
### Naive Bayes
### RSH 5/18 MSDS Predictive Modeling

# Import ther data from UCI
data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt")
#
#add data names
# 
names(data)[1] <- "var"
names(data)[2] <- "skew"
names(data)[3] <- "curt"
names(data)[4] <- "ent"
names(data)[5] <- "class"

#Change as factor since it is a classification problem
data$class <- as.factor(data$class)

#Split te data 80/20 for training and testing sets
t1 <- sample(1:1371, 1127)
t2 <- setdiff(1:1371, t1)

# extract the class label from the training data
# LAbels
cl <- data[t2,]$class

# Create new vars
train <- subset(data[t1,])
test <- subset(data[t2,], select =- class)
model <- naiveBayes(class ~ ., data=train) #run algorithm agains all variables in the model. Label given (~) features

#run prediction
pred <- predict(model, test)

# Analysis of preformance, sensitivity extc...
confusionMatrix(pred, as.factor(cl)) 




