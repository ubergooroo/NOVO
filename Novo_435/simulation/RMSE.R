#This information comes from: http://www.stat.berkeley.edu/~stark/SticiGui/Text/estimation.htm#


# Estimating from a sample is like shooting a rifle. The parameter is the bullseye, and each shot is the value 
# of the estimator from one random sample. A systematic tendency for all the shots to miss the bullseye in the 
# same direction is bias: Bias is the difference between the average location of the shots, and the bullseye. 

# The scatter in the shots is measured by the standard error: the average of the distances between each shot 
# and the average location of all the shots. The average squared distance between the bullseye and where the shots 
# land is the mean squared error. For the mean squared error to be small, both the bias and the standard error must 
# be small. 

# If the standard error is zero, but the bias is not, the estimator is like a very accurate rifle that 
# has its sights mis-calibrated: All the shots hit the same spot, but that spot is not the bullseye. If the bias 
# is zero but the standard error is not, the estimator is like an inaccurate rifle that is sighted in correctly: 
# The shots are scattered around the bullseye, but typically miss the bullseye. If both the bias and the standard 
# error are zero, so is the mean squared error, and the estimator is like a very accurate rifle that is sighted 
# in correctly: All the shots hit the bullseye. 


# We can write the value the estimator takes for a particular random sample as the sum of three terms: 
# the parameter we seek to estimate, systematic bias, and chance variability:

# estimator = parameter + bias + chance variability


# The bias is the long-run average difference between the parameter and the estimate if we 
# repeatedly drew random samples of size n, calculated the value of the estimator for the sample, 
# and subtracted the parameter from the estimate.

# Bias equals the E(estimator) - parameter. So in our case sample mean - population mean. 

# The chance variability (also called sampling error in this context) reflects the luck of the draw i.e. 
# which particular units happened to be in the sample.

# The typical "size" of the chance variability is the standard error (SE) of the estimator.
# The SE of the estimator is the square-root of the expected value of the square of the chance variability.
# The standard error measures the long-run average spread of the estimated values.

# Standard Error of the estimator equals the sqrt(E(estimator - E(estimator))^2).

# Both bias and standard error contribute to the average size of the error of an estimator. 
# If the bias is large, on average the estimator overshoots or undershoots the truth by a large amount. 
# If the SE is large, the estimator typically is far from the truth, even if its average is close to the truth. 

# A common measure of the overall error of an estimator is its mean squared error (MSE): 
# The mean-squared error is the expected value of the square of the error, the difference between the estimator 
# and the true value of the parameter. The units of MSE are the squares of the units of the estimator.

# The root mean-squared error in fact can be written in terms of the bias and SE.


# The MSE and RMSE measure the average error of an estimator. That is, we expect the value of an estimator to differ
# from the value of the parameter by roughly the RMSE. For any particular sample, however, the estimate could differ 
# from the parameter by more than or by less than the RMSE. Typically, we cannot tell how much they differ, 
# because we only know the value of the estimator, and not the true value of the parameter. 


# Function that returns Root Mean Squared Error

rmse <- function(df){    
  
  # Calculate bias and error
  bias <- df$pop.mean - df$control_postbal.mean
  se   <- df$control_postbal.se
  
 
  # Calculate RMSE
  sqrt((se^2+bias^2))

}


eb_df <- read.table(file = "clipboard", sep = "\t", header=TRUE)
eb<-rmse(eb_df)


ipw_df <- read.table(file = "clipboard", sep = "\t", header=TRUE)
ipw<-rmse(ipw_df)


sipw_df <- read.table(file = "clipboard", sep = "\t", header=TRUE)
sipw<-rmse(sipw_df)




