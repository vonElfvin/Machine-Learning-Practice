#### SVM
### Libraries
library(kernlab)

### Setup
data.crabs = read.csv("australian-crabs.csv")
data.crabs$sex = ifelse(data.crabs$sex=="Male", 1, 0)

### Functions
mcr = function(y, yfit){
  return(mean(abs(y-yfit)))
}

### Implementation
plot(data.crabs$RW, data.crabs$CL, col=data.crabs$sex+5)

## All
svm.all = ksvm(sex~., data=data.crabs, kernel="rbfdot", kpar=list(sigma=0.05), C=10, cross=2, type="C-svc")
pred.all = predict(svm.all, data.crabs)
#kernlab::plot(svm.all, data=data.crabs) # cant plot since dim > 2
plot(data.crabs$RW, data.crabs$CL, col=pred.all+5)
mcr.all = mcr(data.crabs$sex, pred.all) # 0.03

## RW and CL
svm.RWCL = ksvm(sex~RW+CL, data=data.crabs, kernel="rbfdot", kpar=list(sigma=0.05), C=100, cross=2, type="C-svc")
pred.RWCL = predict(svm.RWCL, data.crabs)
kernlab::plot(svm.RWCL, data=data.crabs)
plot(data.crabs$RW, data.crabs$CL, col=pred.RWCL+5)
mcr.RWCL = mcr(data.crabs$sex, pred.RWCL) # 0.025
