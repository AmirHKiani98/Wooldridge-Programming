
# Setting the script directory as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Reading the data
rdchem <- read.csv("RDCHEM.csv")

# (i) - Write down a model (not an estimated equation) that implies a constant elasticity
# between rd and sales . Which parameter is the elasticity?
"
The constant elasticity model is a log-log model(from the solution book)
so the equation is : log(rd) = beta0 + beta1 * log(sales) + u
elasticity = beta1
"
# (ii) - Now, estimate the model using the data in RDCHEM.RAW. Write out the 
# estimated equation in the usual form. What is the estimated elasticity of rd 
# with respect to sales ? Explain in words what this elasticity means.
logRd <- log(rdchem$rd)
logSales <- log(rdchem$sales)
regressionModel <- lm(logRd~logSales)
a <- (regressionModel$coefficients)
modelEquation <- paste("log(rd) = ", round(a[1],2) , " + (" , round(a[2],5)
                       , ") * log(sales)" , sep="")
details <- summary(regressionModel)
rSquared <- details$r.squared
modelEquation
rSquared
"
The elasticity is 1.07573 which shows us that 1 percent change in sales, changes 
about 1.076 percent in rd
"