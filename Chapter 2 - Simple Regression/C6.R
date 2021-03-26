
# Setting the script directory as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Reading the data
meap93 <- read.csv("MEAP93.csv")

# (i) - Do you think each additional dollar spent has the same effect on the 
# pass rate, or does a diminishing effect seem more appropriate? Explain.
"
According to *marginal utility* meaning, we understand that at high level of 
expending, any other one dollar has lower effect on the school score.
So the answer is no, there is not a same effect on pass rate
"

# (ii) - In the population model: math10 = beta0 + beta1 * log(expend) + u
# argue that beta1 /10 is the percentage point change in math10 given a 10% 
# increase in expend .
"
beta1 / 10 is when log(expand) = 0.1. So according to that the natural logaritm
shows changin in **percentage** we can say 10 percentage change (0.1) in expand leads
to beta1 / 10 change in math10.
"
# (iii) - Use the data in MEAP93.RAW to estimate the model from part (ii). 
# Report the estimated equation in the usual way, including the sample size and 
# R -squared.
regressionModel <- lm(meap93$math10~log(meap93$expend))
a <- regressionModel$coefficients
modelEquation <- paste("math10 = ", round(a[1],2) , " + (" , round(a[2],5)
                       , ") * log(expend)" , sep="")
details <- summary(regressionModel)
rSquareds <- details$r.squared
n <- nrow(meap93)
modelEquation
rSquareds
n

# (iv) - How big is the estimated spending effect? Namely, if spending increases 
# by 10%, what is the estimated percentage point increase in math10 ?
"
The magnitude of changing in spending percentage's effect on math10 is shown in 
beta1. as beta1 is 11.1644 so if the expend changes 0.1 (say 10 percet), the changes 
in math10 is about 11.1644*0.1 = 1.11644 percentage
"
# (v) - One might worry that regression analysis can produce fitted values for math10
# that are greater than 100. Why is this not much of a worry in this data set?
"
We should check the largest amount of math10 in this dataset
"
largestMath10 <- max(meap93$math10)
largestMath10
"
The largest amount of math10 in this dataset is 66.7 so we do not close to 100 at all

"
largestMath10Fitted <- a[1] + a[2] * log(max(meap93$expend))
largestMath10Fitted
"
And as you see, the largest fitted value is about 30.15. So again, we do no close
to 100 percent at all.
"