
# Setting the script directory as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Reading the data
wage2 <- read.csv("WAGE2.csv")

# (i) - Find the average salary and average IQ in the sample. What is the sample standard
# deviation of IQ? (IQ scores are standardized so that the average in the population
# is 100 with a standard deviation equal to 15.)
salaryAvg <- mean(wage2$wage)
IQAvg <- mean(wage2$IQ)
IQStd <- sd(wage2$IQ)
salaryAvg*1000
IQAvg
IQStd

# (ii) - Estimate a simple regression model where a one-point increase in IQ changes
# wage by a constant dollar amount. Use this model to find the predicted increase in
# wage for an increase in IQ of 15 points. Does IQ explain most of the variation in
# wage ?
regressionModel <- lm(wage2$wage~wage2$IQ)
a <- (regressionModel$coefficients)
modelEquation <- paste("wage = ", round(a[1],2) , " + (" , round(a[2],2)
                       , ") * IQ" , sep="")
wageChange <- a[2]* 15
details <- summary(regressionModel)
rSquared <- details$r.squared
modelEquation
wageChange
rSquared
# (iii) - Now, estimate a model where each one-point increase in IQ has the same percent-
# age effect on wage . If IQ increases by 15 points, what is the approximate percent-
# age increase in predicted wage ?
"
Please pay attention, if we want to clarify the regression model in percentage
mode, we should change the output into natural logaritm.(not the xs', just ys')
"
logWage <- log(wage2$wage)
regressionModel <- lm(logWage~wage2$IQ)
a <- (regressionModel$coefficients)
modelEquation <- paste("log(wage) = ", round(a[1],2) , " + (" , round(a[2],5)
                       , ") * log(IQ)" , sep="")
wageChange <- a[2] * (15)
details <- summary(regressionModel)
rSquared <- details$r.squared
modelEquation
wageChange
rSquared

