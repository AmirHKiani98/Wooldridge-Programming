
# Setting the script directory as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Reading the data
sleep75 <- read.csv("SLEEP75.csv")

# (i) - Report your results in equation form along with the number of observations and
# R2 . What does the intercept in this equation mean?
regressionModel <- lm(sleep75$sleep~sleep75$totwrk)
a <- (regressionModel$coefficients)
modelEquation <- paste("sleep = ", round(a[1],2) , " + (" , round(a[2],5)
                       , ") * totwrk" , sep="")
details <- summary(regressionModel)
rSquared <- details$r.squared
modelEquation
rSquared

# (ii) - If totwrk increases by 2 hours, by how much is sleep estimated to fall? Do you
# find this to be a large effect?
fellSleep <- a[2]*2*60 
fellSleep
"
No it's not a large effect on sleep as the intercept is much bigger than the fell 
sleep number.
"