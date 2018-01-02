# APPENDIX 3

library(MASS)
library(glmnet)
# Data
dataframe = read.csv("tecator.csv", dec=',')
dataframe = dataframe[-215,]

# Task 1 - Plot Protein vs. Moisture
protein = dataframe$Protein
moisture = dataframe$Moisture
plot(protein, moisture, main="Moisture vs. Protein") # Plot of protein vs. moisture, linear approximation seems reasonable (normally distributed around a line)

# Task 2
# M1 = w0+w1*p+ε, where ε = N~(0, σ)
# M2 = w0+w1*p+w2*p²+ε, where ε=N~(0, σ)
# M3 = w0+w1*p+w2*p²+w3*p³+ε, where ε=N~(0, σ)
# etc.
# MSE criterion is appropriate to use since it minimizes the errors in our predicions (deviation of ε)

# Task 3 - Models for different polunomial terms of powers
n = dim(dataframe)[1] # number of observations
set.seed(12345)
ids = sample(1:n, floor(n/2))

training = dataframe[ids,] # training data
validation = dataframe[-ids,] # validation (test) data

#M = solve(t(X)%*%X)%*%t(X)%*%Y if I was to calculate it myself, did not work for higher polynomial terms of power in R
## Create the models (Moisture vs. Protein) for the different terms of power
M1 = lm(Moisture ~ Protein, data=training)
M2 = lm(Moisture ~ Protein+I(Protein^2), data=training)
M3 = lm(Moisture ~ Protein+I(Protein^2)+I(Protein^3), data=training)
M4 = lm(Moisture ~ Protein+I(Protein^2)+I(Protein^3)+I(Protein^4), data=training)
M5 = lm(Moisture ~ Protein+I(Protein^2)+I(Protein^3)+I(Protein^4)+I(Protein^5), data=training)
M6 = lm(Moisture ~ Protein+I(Protein^2)+I(Protein^3)+I(Protein^4)+I(Protein^5)+I(Protein^6), data=training)

# Make the predictions with the given models
fitted_training1 = M1$fitted.values
fitted_training2 = M2$fitted.values
fitted_training3 = M3$fitted.values
fitted_training4 = M4$fitted.values
fitted_training5 = M5$fitted.values
fitted_training6 = M6$fitted.values
fitted_validation1 = predict(M1, validation)
fitted_validation2 = predict(M2, validation)
fitted_validation3 = predict(M3, validation)
fitted_validation4 = predict(M4, validation)
fitted_validation5 = predict(M5, validation)
fitted_validation6 = predict(M6, validation)

# Calculate the MSEs

MSE = mean(())

# Training MSEs
mse_t1 = mean((training$Moisture-fitted_training1)^2)
mse_t2 = mean((training$Moisture-fitted_training2)^2)
mse_t3 = mean((training$Moisture-fitted_training3)^2)
mse_t4 = mean((training$Moisture-fitted_training4)^2)
mse_t5 = mean((training$Moisture-fitted_training5)^2)
mse_t6 = mean((training$Moisture-fitted_training6)^2)
mse_t = c(mse_t1, mse_t2, mse_t3, mse_t4, mse_t5, mse_t6) # vector of all MSEs for training

# Validation MSEs
mse_v1 = mean((validation$Moisture-fitted_validation1)^2)
mse_v2 = mean((validation$Moisture-fitted_validation2)^2)
mse_v3 = mean((validation$Moisture-fitted_validation3)^2)
mse_v4 = mean((validation$Moisture-fitted_validation4)^2)
mse_v5 = mean((validation$Moisture-fitted_validation5)^2)
mse_v6 = mean((validation$Moisture-fitted_validation6)^2)
mse_v = c(mse_v1, mse_v2, mse_v3, mse_v4, mse_v5, mse_v6) # vector of all MSEs for validation

# Plot the MSE's of the validation set compared to the training set
plot(1:6, xlab="Mi", ylab="MSE", mse_t, type="l", col="green", ylim=c(20,45), main="MSE for Trainign and Validation set")
lines(1:6, mse_v, type="l", col="blue")
legend("topright", legend=c("Training (Green)", "Validation (Blue)"))

# Create the formula where Fat is to be predicted by all the channels 1-100
idx = !colnames(training) %in% c("Sample", "Fat", "Protein", "Moisture")
formulax = paste("Fat~", paste(colnames(training[,idx]), collapse = "+"))

#Task 4 - Reduce the linear model's features used to make prediction using stepAIC
fit = lm(formula(formulax), data=dataframe)
step = stepAIC(fit, direction="both", trace=FALSE)
step$anova
summary(step)

# Task 5 - Ridge regression
scaled_channels = scale(dataframe[,2:101]) # centers/scales the values of the channels
scaled_fat = scale(dataframe$Fat) # centers/scales the values of fat
model0_ridge = glmnet(as.matrix(scaled_channels), scaled_fat, alpha=0, family="gaussian") # calculate a model to predict fat with channels
plot(model0_ridge, xvar="lambda", main="Ridge Regression") # plot of the coefficients values as compared to the log of λ, converges to 0 for larger λ

# Task 6 - LASSO
model0_lasso = glmnet(as.matrix(scaled_channels), scaled_fat, alpha=1, family="gaussian") # calculate a model to predict fat with channels
plot(model0_lasso, xvar="lambda",  xlim=c(-7,0), TRUE, main="LASSO") # plot of the coefficient values as compared to the log of λ, converges to 0 but not as smoothly as ridge

# Task 7 - Cross-validation LASSO, testing 1000 lambda from 0 (inclusive) to 1
model0_lasso_cv = cv.glmnet(as.matrix(scaled_channels), scaled_fat, alpha=1, family="gaussian", lambda=seq(0,1,0.001)) # cross-validation
plot(model0_lasso_cv, xvar="lambda", label=TRUE, main="Cross-Validation LASSO") # plot of the MSE of the cross-validation, follows the trend of the lasso model from 6 nicely






