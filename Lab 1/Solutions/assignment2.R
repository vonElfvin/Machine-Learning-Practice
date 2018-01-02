# APPENDIX 2

# returns the loglikelihood value for given θ and vector X
loglikelihood = function(x, θ){
  return(length(x)*log(θ)-θ*sum(x))
}

# returns the argmax θ of the logilikelihood the given distribution estimation
max_loglikelihood = function(x){
  return(length(x)/sum(x))
}

# returns log value of the bayesian proportional probability for given θ, λ and vector X
log_bayesian = function(x, θ, λ){
  n = length(x)
  #return(n*log(θ)-θ*sum(x)+n*log(λ)-n*λ*θ)
  return(n*log(θ)-θ*sum(x)+log(λ)-λ*θ)
}

# returns the argmax max value of θ for the bayesian model
max_bayesian = function(x, λ){
  n = length(x)
  return (n/(sum(x)+λ))
}

# Task 1
# Data preparation
dataframe = read.csv("machines.csv", dec=',')
X = dataframe[1]$Length # X vector of data
X_6 = X[1:6] # X vector of first 6 values of the data
θ = seq(from=0, to=20, by=0.025) # θs to test
m = length(θ) # Amount of θs to be tested
loglikelihood_n = numeric(m) # Empty vector for loglikelihood values θ when using all values
loglikelihood_6 = numeric(m) # Empty vector for loglikelihood values θ when using the first 6 values

# Task 2
# Calculating the loglikelihoods and filling the vetors different θ values with the given vectors
for(i in 1:m){
  loglikelihood_n[i] = loglikelihood(X, θ[i]) 
  loglikelihood_6[i] = loglikelihood(X_6, θ[i])
}

# Task 3
# Plot the loglikelihoods for both all x values and first 6 for comparision
plot(θ, loglikelihood_6, xlim=c(0,7), ylim=c(-60, 0), type="l", main="Dependence of Log-Likelihood and θ", xlab="θ", ylab="Log-Likelihood", col="green")
lines(θ, loglikelihood_n, col="blue")
legend("bottomright", legend=c("(green) loglikelihood for n values", "(blue) loglikelihood for n values"))

# θ values for maximum loglikelihoods for all x values and first 6
θstar_n = max_loglikelihood(X)
θstar_6 = max_loglikelihood(X_6)

# Task 4
λ = 10; # given λ value for the prior
log_bayesian_model = numeric(m) # empty vector to be filled with the log values of the proportional probability to P(θ|x)

# Calculating the log values of the proportional probability to given θ
for(i in 1:m){
  log_bayesian_model[i] = log_bayesian(X, θ[i], λ)
}
# Plot the Bayesian Model
plot(θ, log_bayesian_model, type="l", xlim=c(0,5), ylim=c(-500, 0), main="Dependance of l(θ) on θ", xlab="θ", ylab="l(θ)", col="green")
θstar_b = max_bayesian(X, λ) # Calculate the argmax θ of the proportional probability

# Task 5
# Compare new generated observations to original ones
set.seed(12345)
new_observations = rexp(n=50, rate = θstar_n) # Generate new observations with optimal θ from Task 3
#data.frame(X, new_observations)
hist(X, col="green", xlim=c(0,6), ylim=c(0,30), main="Original observations", xlab="x") # Histogram of the original obs
x11()
hist(new_observations, col="blue", breaks=12, xlim=c(0,6), ylim=c(0,30), main="New observations", xlab="x") #Histogram of the new obs












