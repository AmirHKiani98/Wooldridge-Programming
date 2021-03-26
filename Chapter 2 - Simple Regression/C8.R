
# Setting the script directory as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# (i) - Start by generating 500 observations xi – the explanatory variable – from the
# uniform distribution with range [0,10]. (Most statistical packages have a command
# for the Uniform[0,1] distribution; just multiply those observations by 10.) What
# are the sample mean and sample standard deviation of the xi ?
x <- runif(500, 0 , 10)
xAvg <- mean(x)
xStd <- sd(x)
xAvg
xStd

# (ii) - Randomly generate 500 errors, ui , from the Normal[0,36] distribution. 
# (If you generate a Normal[0,1], as is commonly available, simply multiply the outcomes
# by six.) Is the sample average of the ui exactly zero? Why or why not? What is the
# sample standard deviation of the ui ?
u <- rnorm(500,0,36)
uAvg <- mean(u)
uAvg
"
not exactly, but it's close to zero. one of the reasons is that the samples size 
is not very large.
"
uStd <- sd(u)
uStd

# (iii) - Now generate the yi as yi = 1 + 2xi + ui which is like b0 1 b1xi 1 ui ;
# that is, the population intercept is one and the population slope is two. Use the
# data to run the regression of yi on xi . What are your estimates of the intercept and
# slope? Are they equal to the population values in the above equation? Explain.
y <- 1 + 2 * x + u
regressionModel <- lm(y~x)
a <- regressionModel$coefficients
modelEquation <- paste("yi = ", round(a[1],2) , " + (" , round(a[2],5)
                       , ") * xi" , sep="")
details <- summary(regressionModel)
rSquareds <- details$r.squared
n <- length(y)
modelEquation
rSquareds
n

"
The intercept is about -5.12 and the slope is about 2.72.
No, these values are not equal to primary equation for yi. becuase the errors will
effect the the intercept and the slope.
"
# (iv) - Obtain the OLS residuals, u^ , and verify that equation (2.60) hold 
# (subject to rounding error).
sumOfResiduals <- sum(regressionModel$residuals)
sumOfResidualsTimesSamples <- sum(regressionModel$residuals*x)
sumOfResiduals
sumOfResidualsTimesSamples
"
As the sumOfResiduals(sum of the residuals' of the model) shows, it's very very close
to zero.
on the other side, the sum of (the reisduals multiply in x(samples)) shows it's too close
to zero,too.
"

# (v) - Compute the same quantities in equation (2.60) but use the errors ui in 
# place of the residuals. Now what do you conclude?
sumOfErrorTimesSamples <- sum(u*x)
sumOfErrorTimesSamples
"
The result it too much bigger than zero.
"

# (vi) - Repeat parts (i), (ii), and (iii) with a new sample of data, starting 
# with generating the xi . Now what do you obtain for beta^0 and betaˆ1 ? Why are 
# these different from what you obtained in part (iii)?
"
(repeat parts (i), (ii) and (iii) by running them again)
Because the variables are random!
"
