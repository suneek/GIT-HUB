############################################################
# Author: Erinc Sonmezer                                   #
# Examples based on: Elements of Econometrics - Jan Kmenta #
############################################################
########################################################################
# Setting Working Directory                                            #
# Rule of thumb: Keep R Script & flat data files in the same directory #
########################################################################
setwd("C:/Users/svemu/OneDrive/Suneetha_Docs/EMORY_Big_Data/R")


##############################
# Linear Regression Brush-up #
##############################

# 1- Relationship Between Variables
# Characteristic Eqn: y = A + Bx

# A deterministic relationship
Price <- seq(from=1,to=50,by=1)
Quantity <- 100 - 2*Price
plot(Price,Quantity, main ="A Deterministic Relation",cex=2)

# Now consider introduction of random disturbance!
E_plus <- Quantity+5
E_minus <- Quantity-5
plot(Price,Quantity, main ="A Stochastic Relation",cex=2)
points(E_plus,col="dark red",pch=19,cex=1)
points(E_minus,col="dark red",pch=19,cex=1)

# One Possible Realization
Error <- NA
for(i in Price){
  p <- sum(rbinom(n=2,size=1,prob=0.5))
  Error[i] <- ifelse(p==1,0,ifelse(p==0,-5,5))
}

# Re-write the equation including random disturbance (error term)
Quantity <- 100 - 2*Price + Error
plot(Price,Quantity, main ="One Possible Realization",cex=2)

# In modeling we deal with stochastic relations. The simplest form of stochastic
# realation between two variables x & y is "Simple Linear Regression Model"
# of the form --> Y_i = A + B*X_i + E_i

# 2-Simple Linear Regression Model
# Can be time-series
# Can be cross-section (groups of individuals, objects, geographic areas)

# Basic Assumptions
# 1-Normality: E_i is normally distributed
# 2-Zero Mean: E[E_i] = 0
# 3-Homoskedasticity: Var(E_i) = Sigma^2 (Constant)
# 4-Nonautocorrelation: Cov(E_i,E_j) = 0 (for i<>j)
# 5-Nonstochastic X: X is a nonstochastic variable with values fixed in repeated
# samples

# Understanding difference between Population vs. Sample Regression Lines
Price <- seq(from=1,to=50,by=1)
Quantity_Population <- 100 - 2*Price
Quantity_Sample <- 80 - 1.5*Price
plot(Price,Quantity_Population, main ="Population vs. Sample",type="l",ylab="Quantity")
lines(Price,Quantity_Sample,col="dark red")
points(20,90,pch=19,cex=1)

# Linear Regression With rattle
library(rattle)
rattle()