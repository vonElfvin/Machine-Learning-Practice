#### Learn XOR through Neuralnet
### Libraries
library(neuralnet)

### Setup
x1 = c(0,0,1,1)
x2 = c(0,1,0,1)
y  = c(0,1,1,0)
data.xor = data.frame(x1,x2,y)
#w.init = runif(9,-1,1)

### Functions
#custom_max = function(x){
#  # Approximation of max(0,x)
#  x/(1+exp(-2*10*x))
#}

### Implementation
set.seed(12345)
#nn.xor = neuralnet(formula=y~x1+x2, data=data.xor, hidden=c(2), linear.output = FALSE, err.fct="ce")
#nn.xor = neuralnet(formula=y~x1+x2, data=data.xor, hidden=c(2), act.fct = custom_max, threshold=0.001)
nn.xor = neuralnet(formula=y~x1+x2, data=data.xor, hidden=c(2), act.fct = "logistic", threshold=0.001)
#yfit.xor = ifelse(compute(nn.xor, data.frame(x1=data.xor$x1, x2=data.xor$x2))$net.result>0.5, 1, 0)
yfit.xor = compute(nn.xor, data.frame(x1=data.xor$x1, x2=data.xor$x2))$net.result
