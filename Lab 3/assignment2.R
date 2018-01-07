#### Lab 3 - Assignment 2 - Neural Networks
### Libraries
library(neuralnet)

### Setup
set.seed(1234567890)
rad = runif(50, 0, 10) # 50 radians between 0 and 10
data = data.frame(rad, sin=sin(rad))
#n = dim(data)[1]
#ids = sample(1:n, n/2)
#train = data[ids,]
#valid = data[-ids,]
train = data[1:25,]
valid = data[26:50,]
y.train = train$sin
y.valid = valid$sin

### Functions
mse = function(y, yfit){
  return(mean((y-yfit)^2))
}

### Implementation
w.init = runif(31, -1, 1)
m = 10
threshold = numeric()
mse.train = numeric()
mse.valid = numeric()
for(i in 1:m){
  nn = neuralnet(formula=sin~rad, data=train, hidden=c(10), threshold = i/1000, startweights = w.init)
  yfit.train = compute(nn, covariate=train$rad)$net.result
  yfit.valid = compute(nn, covariate=valid$rad)$net.result
  threshold[i] = i/1000
  mse.train[i] = mse(y.train, yfit.train)
  mse.valid[i] = mse(y.valid, yfit.valid) 
}

# Plot the MSEs of the different data sets
plot(threshold, mse.train, xlim=c(threshold[m], threshold[1]), type="b", col="blue", ylim=c(0, 0.0012),
     main="MSEs vs. Threshold", ylab="MSE")
points(threshold, mse.valid, xlim=c(threshold[m], threshold[1]), type="b", col="green")
# Comment: Best threshhold = 4/1000, i = 4

# Neural Network with best threshhold
best.threshold = which.min(mse.valid)
nn = neuralnet(sin~rad, data=data, hidden=c(10), threshold=threshold[best.threshold], startweights=w.init)
plot(nn)

# Predictions
pred.all = prediction(nn)$rep1
yfit.train = compute(nn, train$rad)$net.result
yfit.valid = compute(nn, valid$rad)$net.result

# For All
plot(data, col="red", ylim=c(-1, 2), main="Predictions All")
points(pred.all, col="blue")
legend("topright", legend=c("Predictions (All)", "Observed (All)"), col=c("blue","red"), lty=1)

# For Training
plot(data, col="red", ylim=c(-1, 2), main="Predictions Training")
points(train$rad, yfit.train, col="blue")
legend("topright", legend=c("Predictions (Training)", "Observed (All)"), col=c("blue","red"), lty=1)

# For validation
plot(data, col="red", ylim=c(-1, 2), main="Predictions Validation")
points(valid$rad, yfit.valid, col="green")
legend("topright", legend=c("Predictions (Validation)", "Observed (All)"), col=c("green","red"), lty=1)
