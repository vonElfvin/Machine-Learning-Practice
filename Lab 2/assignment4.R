#### Assignment 4
### Libraries
library(fastICA)
library(pls)

### Setup
data.spectra = read.csv2("NIRSpectra.csv")
data = data.spectra
data$Viscosity = c()

### Functions

### Implementation
## Task 1 - PCA
PCA = prcomp(data)
# Extract eigenvalues
λ = PCA$sdev^2
variation.proportion = λ/sum(λ)*100
# screeplot(PCA)
barplot(variation.proportion[1:10], ylim=c(0,100), col="forestgreen", 
        main="Variation explanaition proportions for different eigenvalues", 
        xlab="PCi", ylab="Variation proportion") # The plot shows that 2 PC1 and PC2 should be extracted
sum(variation.proportion[1:2]) # 99.5957% of the total variance is explained by the first 2 PCs

# Scores - There seems to be 2-7 unusual diesiel fuels according to this plot
plot(PCA$x[,1], PCA$x[,2], xlab="Z1", ylab="Z2", main="Projected Values, PCA") # 2 "strong" outliers, 5-7 "medium" outliers

## Task 2 - Trace Plots
U = PCA$rotation # rotation matrix

# Tracing plots
plot(U[,1], main="Traceplot for PC1", ylim=c(-0.11,0.11), ylab="Projection/Rotation Value")
plot(U[,2], main="Traceplot for PC2", ylim=c(-0.3, 0.3), ylab="Projection/Rotation Value") # the last ~10 feutures mainly explain

## Task 3 - Indipendent Component Analysis (ICA)
set.seed(12345)
# 2 components selected -> n.comp=2
ICA = fastICA(data, n.comp=2, alg.typ="parallel", fun="logcosh", alpha=1, method="R", row.norm=FALSE, maxit=200, tol=0.0001, verbose=TRUE)
# W' = K * W
Wtick = ICA$K%*%ICA$W
plot(Wtick[,1], main="Traceplot, W' column 1", ylim=c(-1, 1), ylab="Projection/Rotation Value")
plot(Wtick[,2], main="Traceplot, W' column 2", ylim=c(-11,11), ylab="Projection/Rotation Value") 
# Comment: the "opposite" to PCA, similar information

# Scores
plot(ICA$S[,1], ICA$S[,2], xlab="Z1", ylab="Z2", main="Projected Values, ICA") # ICA, 

# Task 4 - PCR
set.seed(12345)
PCR = pcr(Viscosity~., data=data.spectra, validation="CV")
validationplot(PCR, val.type="MSEP", main="Dependence of MSEP (Mean Squared Error of Prediction) on #components")
