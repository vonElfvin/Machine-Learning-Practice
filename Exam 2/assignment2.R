#### Assignment 2

### Libraries
library(boot)

### Setup
data.bank = read.csv2("bank.csv")

### Functions

# Returns predictions for parametic bootstrapping, with a distribution added to the predictions
stat.pred = function(..data){
  model = glm(Visitors~Time, data=..data, family=poisson(link="log"))
  yfit = exp(predict(model, data.12.to.13))
  # yfit = rnorm(n, yfit, sd(residuals(glm.poisson)))
  # lambda= mean(yfit) # wrong
  yfit = rpois(n, yfit)
  return(yfit)
}

# Returns data with Visitor values replaced with their prediction based on given Time
# The predictio is "moved" around under the assumption that Y~N(µi, sigma^2)
rng = function(..data, model){
  data = data.frame(Time=..data$Time, Visitors=..data$Visitors)
  yfit = exp(predict(model, data))
  # data$Visitors = rnorm(n, yfit, sd(residuals(model)))
  # lambda = mean(pred)
  data$Visitors = rpois(n, yfit)
  return(data)
}

# Plots band
plot_band = function(data, e, yfit, ..main){
  upper.band = e$point[1,]
  lower.band = e$point[2,]
  plot(data$Time, data$Visitors, ylim=c(min(lower.band), max(upper.band)),
       main=..main, xlab="Time", ylab="Visitors")
  lines(data$Time, yfit)
  lines(data$Time, e$point[2,], col="blue")
  lines(data$Time, e$point[1,], col="blue")
}

### Implementation

## Task 1

glm.poisson = glm(Visitors~Time, data=data.bank, family=poisson(link="log"))
pred1 = exp(predict(glm.poisson, data.bank))
# pred = glm.poisson$fitted.values
plot(data.bank$Time, pred, type="l")
points(data.bank)

## Task 2

# Prediction band Given Data
# set.seed(12345)
# n = dim(data.bank)[1]
# boot1 = boot(data.bank, statistic=stat.pred, R=1000, mle=glm.poisson, ran.gen=rng, sim="parametric")
# e1 = envelope(boot1)
# plot_band(data.bank, e1, pred1, "Prediction band for Parametic Bootstrap")

# Data 12 to 13
times = seq(12,13,0.05)
visitors = numeric(21)
data.12.to.13 = data.frame(Time=times, Visitors=visitors)
n = dim(data.12.to.13)[1]
set.seed(12345)
#data.12.to.13 = rng(data.12.to.13, glm.poisson)

# Prediuction band 12 to 13
set.seed(12345)
n = 61+21
#data.12.to.13 = rbind(data.bank, data.12.to.13)
boot2 = boot(data.12.to.13, statistic=stat.pred, R=1000, mle=glm.poisson, ran.gen=rng, sim="parametric")
e2 = envelope(boot2)
pred2 = exp(predict(glm.poisson, data.12.to.13))
plot_band(data.12.to.13, e2, pred2, "Prediction band for Parametic Bootstrap")
e2$point[,(21+61)] # between 188 and 253 visitors
