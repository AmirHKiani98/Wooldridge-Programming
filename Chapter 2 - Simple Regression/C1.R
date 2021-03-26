
# Setting the script directory as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Reading the data from csv file
k401k <- read.csv("k401k.csv")

# i - Find the average participation rate and the average match rate in the sample of
# plans.

pratesAvg <- mean(k401k$prate)
matchAvg <- mean(k401k$mrate)

# (ii) - Now, estimate the simple regression equation prate

regressionModel <- lm(k401k$prate~k401k$mrate)
a <- (regressionModel$coefficients)
modelEquation <- paste("prate = ", round(a[1],2) , " + " , round(a[2],2)
                       , "*mrate" , sep="")
modelEquation
# (iii) - Interpret the intercept in your equation. Interpret the coefficient on mrate
"
If the match rate changes one dollar, the participation rate will change 5.86
and if the match rate will be 0, 83.08 of the statistic population will 
participate
"

# (iv) - Find the predicted prate when mrate = 3.5. Is this a reasonable prediction?
# Explain what is happening here.
regressionEquation <- function(regressionModel){
  beta1 = regressionModel$coefficients[1]
  beta2 = regressionModel$coefficients[2]
  equation <- function(value){
    return(beta1 + beta2*value[[1]])
  }
  return(equation)
}
equation <- regressionEquation(regressionModel)
equation(3.5)

"
No, it's not reasonable. the maximum of participation rate should be 100% but 
the given number is over 100% and it can not be possible. we have to notice that
the giver line by regression is for all the real numbers but just an interval of
them is acceptable.
"
# (v) - How much of the variation in prate is explained by mrate ? Is this a lot 
# in your opinion?
varianceAnalysis <- anova(regressionModel)
variancePercentage <- round((varianceAnalysis[2][1,1]/sum(varianceAnalysis[2])) 
                            *100 ,
                            2)
variancePercentage
"
No it's not too much and says that participation rate depends on other factors too
"