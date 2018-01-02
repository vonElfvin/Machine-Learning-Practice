# Appendix 2

###############
# Assignment 2#
###############

# Libraries
library(neuralnet)

# Setup
set.seed(12345)
rad = runif(50, 0, 10) # radians between 0 and 10
data = data.frame(rad, sin=sin(rad)) # data with sinuses from given radians

# Divide data into training set and validation set
n = dim(data)[1]
ids = sample(1:n,n/2)
training = data[ids,]
validation = data[-ids,]

# Functions
# Returns the Mean Squared Error of given observations and predictions
mse = function(obs, pred){
  return(mean((obs-pred)^2))
}

# Implementation
set.seed(12345)
weights.start = runif(50, -1, 1)
mse.training = numeric(10)
mse.validation = numeric(10)

# Compute MSEs for each data set with different threshholds
for(i in 1:10){
  nn = neuralnet(sin~rad, training, hidden=10, threshold=i/1000, startweights=weights.start)
  pred.training = compute(nn, training$rad)
  pred.validation = compute(nn, validation$rad) # Make predictions
  mse.training[i] = mse(training$sin, pred.training$net.result[,1]) # Calculate MSE for training set
  mse.validation[i] = mse(validation$sin, pred.validation$net.result[,1]) # Calculate MSE for validation set
}
# Plot the MSEs -> Pick i=1 -> Threshhold 1/1000=0.001
plot(mse.training, type="b", col="green", ylim=c(0,0.005), xlim=c(0,10),
     main="MSEs for Validation and Training", xlab="i value", ylab="MSE")
points(mse.validation, type="b", col="red")
legend("topright", legend=c("MSE Training", "MSE Validation"), col=c("green","red"), lty=1)


# Neural Network with best threshhold = 1
nn = neuralnet(sin~rad, training, hidden=10, threshold=1/1000, startweights=weights.start)
plot(nn)

# Predictions
prediction.training = prediction(nn)
prediction.validation = compute(nn, validation$rad)
# For Training
plot(prediction.training$rep1, col="blue", ylim=c(-1, 2), main="Predictions Training")
points(data, col="red")
legend("topright", legend=c("Predictions (Training)", "Observed (All)"), col=c("blue","red"), lty=1)
# For validation
plot(data, col="red", ylim=c(-1, 2), main="Predictions Validation")
points(validation$rad, prediction.validation$net.result[,1], col="green")
legend("topright", legend=c("Predictions (Validation)", "Observed (All)"), col=c("green","red"), lty=1)
