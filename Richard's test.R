setwd("C:/Users/Use/Desktop/R codes/bank")


#For LDA I think..
library(MASS)
Raw.data <- read.csv("bank-full.csv", sep = ";", header = T)



#Randomly pick 30% from the raw data
#################################
#NEED TO DISCUSS THIS
m = dim(Raw.data)[1]
set.seed(1)
val = sample(1:m,size=round(m*0.3),replace=FALSE,prob=rep(1/m,m))
train.data = Raw.data[-val,]
validation.data = Raw.data[val,]
View(validation.data)

#################################

#Perform LDA fitting
lda.fit = lda(y ~ .,data = train.data)
lda.fit

#Compare predicted value with validation set
lda.pred = predict(lda.fit, validation.data)
#Confusion Matrix
Matrix = table(lda.pred$class, validation.data$y)
Matrix
#Calculate Recall, Precision and Accuracy
#Recall
Recall.Yes = Matrix[2,2]/sum(Matrix[,2])
Recall.No = Matrix[1,1]/sum(Matrix[,1])

#Precision
Precision.Yes = Matrix[2,2]/sum(Matrix[2,])
Precision.No = Matrix[1,1]/sum(Matrix[1,])

#Accuracy
Accuracy = mean(lda.pred$class == validation.data$y)
Error = mean(lda.pred$class != validation.data$y)

table()

Recall.Yes
Recall.No
Precision.Yes
Precision.No
Accuracy
Error