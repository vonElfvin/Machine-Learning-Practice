#### Assignment 2 - Inference about lifetime of machines

### Libraries

### Setup
## Task 1 - Import data
data.machines = read.csv("machines.csv", dec=',')
x.all = data.machines[,1]
x.6 = x.all[1:6]
θ.seq = seq(0.01, 7, 0.025)
λ = 10

### Functions
## Returns the log-likelihood for given θ and data vector x
log_likelihood =function(θ, x){
  n = length(x)
  return(n*log(θ)-θ*sum(x))
}

## Returns the maximum log-likelihood of θ for given data vector x
max_log_likelihood = function(x){
  n = length(x)
  return(n/sum(x))
}

## Returns the log of the posterior value of given θ, λ and data vector x
log_posterior = function(θ, λ, x){
  n = length(x)
  return(n*log(θ)-θ*sum(x)+log(λ)-λ*θ)
}

## Returns the maximum of the log of the posterior value of given λ and data vector x
max_log_posterior = function(λ, x){
  n = length(x)
  return( n/(sum(x)+λ) )
}

### Implementation
## Task 2 - Curve showing the dependence of log-likelihood on θ
# All observations
log.lh.all = log_likelihood(θ.seq, x.all)
plot(θ.seq, log.lh.all) # Max value at θ~1

## Task 3
# First 6 observations
log.lh.6 = log_likelihood(θ.seq, x.6)
plot(θ.seq, log.lh.6) # Max value at θ~1.8

# θ of max values
θ.max.all = max_log_likelihood(x.all) # 1.126217
θ.max.6 = max_log_likelihood(x.6) # 1.785681

# Plot the curves
plot(θ.seq, log.lh.all, ylim=c(-60, 0), type="l",
      main="Dependence of Log-Likelihood on θ", xlab="θ", ylab="Log-Likelihood", col="green")
lines(θ.seq, log.lh.6, col="blue")
legend("bottomright", legend=c("all values", "6 values"), col=c("green", "blue"), lty=1)
points(θ.max.all, log_likelihood(θ.max.all, x.all))
points(θ.max.6, log_likelihood(θ.max.6, x.6))

# Comment: The estimation using all observations is more reliable since the peak is sharper

## Task 4 - Bayesian Model (Posterior probability)
log.posterior = log_posterior(θ.seq, λ, x.all)
θ.max.posterior = max_log_posterior(λ, x.all)
plot(θ.seq, log.posterior, col="blue", type="l",
      main="Posterior Probability l(θ) for given θ", xlab="θ", ylab="l(θ)")
points(θ.max.posterior, log_posterior(θ.max.posterior, λ, x.all))

## Task 5
# Compare new generated observations to original ones
set.seed(12345)
x.new = rexp(n=50, rate = θ.max.all) # Generate new observations with optimal θ from Task 2
hist.original = hist(x.all, breaks=12)
hist.new = hist(x.new, breaks=12)
plot(hist.original, col=rgb(0,1,0,1/2),
     main="Histogram of new observations in comparison with original", xlab="Length")
plot(hist.new, col=rgb(0,0,1,1/2), add=T)
legend("topright", legend=c("Original observations", "New observations"), col=c("green", "blue"), lty=1)