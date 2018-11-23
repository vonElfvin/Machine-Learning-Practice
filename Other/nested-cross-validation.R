#### Nested Cross-Validation
### Libraries
library(kernlab)

### Setup
data(spam)
data.spam = spam

### Functions

get_best_SVM = function(..data){
  # Setup
  set.seed(1234567890)
  n = dim(..data)[1]
  K = 2
  ids = sample(1:n, n)
  data = ..data[ids,]
  KERNEL = c("rbfdot", "vanilladot")
  C = c(1, 5)
  H = c(0.01, 0.05)
  SVMs = list()
  CV.errors = numeric(6)
  best.SVMs = list()
  best.CV.errors = numeric(K)
  width = floor(n/2)
  MC = 0
  for(k in 1:K){
    idx = 1:width + (1-k)*width
    if(k==K) idx =((k-1)*width+1):n
    train = data[idx,]
    test = data[-idx,]
    i = 1
    for(c in C)
      for(kernel in KERNEL)
        for(h in H){
          if(kernel=="rbfdot") 
            SVM = ksvm(type~., data=train, kernel=kernel, C=c, cross=2, kpar=list(sigma=h)) 
          else 
            SVM = ksvm(type~., data=train, kernel=kernel, C=c, cross=2)
          CV.errors[i] = cross(SVM)
          SVMs[i] = SVM
          i = i+1
          if(kernel=="vanilladot") break()
        }
    best.CV.k = which.min(CV.errors)
    best.SVM.k = SVMs[[best.CV.k]]
    yfit.k = ifelse(predict(best.SVM.k, test)=="spam", 1, 0)
    y.test = ifelse(test$type=="spam", 1, 0)
    MC = MC + sum(abs(yfit.k-y.test))
    best.SVMs[k] = best.SVM.k
    best.CV.errors[k] = MC/dim(test)[1]
  }
  MCR = MC/n
  print(MCR)
  best = which.min(best.CV.errors)
  return(best.SVMs[[best]])
}
### Implementation
svm = get_best_SVM(data.spam)