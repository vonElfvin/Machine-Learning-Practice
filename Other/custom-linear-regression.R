#### Gradient Descent - Linear Regression

### Setup
set.seed(12345)
X.linear = as.matrix(runif(100, -5, 5))
Y.linear = X.linear + rnorm(100) + 3
data.linear = data.frame(X=X.linear, Y=Y.linear)

### Functions

## Returns beta_hat using closed-form solution
get_beta_hat = function(X, Y){
  X = cbind(1, X)
  β.hat = solve(t(X)%*%X)%*%t(X)%*%Y
  rownames(β.hat)[1] = "Intercept"
  return(β.hat)
}

## Returns beta_hat using gradient decent
get_beta_hat_GD = function(X, Y, learning.rate=0.1, plot=FALSE){
  if(plot){ plot(X, Y) }
  X = cbind(1, X)
  k = dim(X)[2]
  n = dim(X)[1]
  learning.rate = 0.1
  
  # Start with random guesses
  β.hat = rep(0.1, k)
  
  # Gradient Descent
  N.iterations = 200
  for(i in 1:N.iterations){
    # Calculate the error: yfit - y, yfit = Xβ, the "variable" in MSE
    residuals = X %*% β.hat - Y
    
    # Calculate the gradient at that point, from derivative of 1/2 * MSE
    delta = (t(X) %*% residuals) / n
    
    # Move β.hat in opposite direction of gradient, to find local error minima
    β.hat = β.hat - learning.rate * delta
    if(plot){ abline(β.hat[1], β.hat[2], col=rgb(0, 0, 1, 0.1)) }
  }
  rownames(β.hat)[1] = "Intercept"
  return(β.hat)
}

### Implementation
beta.lm = lm(Y~X, data=data.linear)$coefficients # lm 
beta.solution = get_beta_hat(data.linear$X, data.linear$Y) # solution
beta.GD = get_beta_hat_GD(data.linear$X, data.linear$Y, 0.1, TRUE)

