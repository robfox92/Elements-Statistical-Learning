## K Nearest Neighbours classification
library(class)
# Read data into single variable
zip.train=read.table(gzfile("zip.train.gz"))
zip.test=read.table(gzfile("zip.test.gz"))

# Separate out 2 and 3 for training and test. 1st column is digit, remainder are image representations.
zip.train.23=subset(zip.train,zip.train[,1]==2|zip.train[,1]==3)
zip.test.23=subset(zip.test,zip.test[,1]==2|zip.test[,1]==3)

# Define variable to store results
knn.perform=rbind(c(1,3,5,7,15),c(1,1,1,1,1),c(1,1,1,1,1))

# Apply knn.
knn.pred.test=function(kay,yhat) 1-mean(knn(zip.train.23[,2:257],zip.test.23[,2:257],zip.train.23[,1],k=kay)==zip.test.23[,1])
knn.pred.train=function(kay,y) 1-mean(knn(zip.train.23[,2:257],zip.train.23[,2:257],zip.train.23[,1],k=kay)==zip.train.23[,1])
knn.perform[2,]=sapply(knn.perform[1,],knn.pred.test)
knn.perform[3,]=sapply(knn.perform[1,],knn.pred.train)
par(mfrow=c(2,1))
plot(knn.perform[1,],knn.perform[2,],type="b",main="knn Classification Test Error",xlab="K",ylab="Test Error")
plot(knn.perform[1,],knn.perform[3,],type="b",main="knn Classification Training Error",xlab="K",ylab="Training Error")

## Linear model/least squares regression
zip.lm=lm(zip.train.23[,1]~.,data=zip.train.23[,2:257])
# Predict for training and test
lm.pred.train=predict(zip.lm)
lm.pred.test=predict(zip.lm,zip.test.23[,2:257])
lm.pred.train=ifelse(lm.pred.train>=2.5,3,2)
lm.pred.test=ifelse(lm.pred.test>=2.5,3,2)

# Calculate errors
lm.err.test=mean((lm.pred.test-zip.test.23[,1])^2)
lm.err.train=mean((lm.pred.train-zip.train.23[,1])^2)

# Print
cat("Linear regression test error is: ",lm.err.test,"\n")
cat("Linear regression training error is: ",lm.err.train,"\n")