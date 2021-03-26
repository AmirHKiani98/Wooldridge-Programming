
# Setting the script directory as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Reading the data
ceosal2 <- read.csv("CEOSAL2.csv")

# (i) - Find the average salary and the average tenure in the sample.
salaryAvg <- round(mean(ceosal2$salary)*1000,2)
tenAvg <- round(mean(ceosal2$ceoten),2)
salaryAvg
tenAvg

# (ii) - How many CEOs are in their first year as CEO (that is, ceoten = 0)? What is the
# longest tenure as a CEO?
firstYearCEOs <- length(ceosal2$ceoten[ceosal2$ceoten == 0])
longestTen <- max(ceosal2$ceoten)
firstYearCEOs
longestTen

# (iii) - Estimate the simple regression model log(salary) = beta0 + beta1*ceoten 
# + u 
"
Please pay attention, the book use **log** as **Natural Logaritm**
"
logSalary <- log(ceosal2$salary)
regressionModel <- lm(logSalary~ceosal2$ceoten)
a <- (regressionModel$coefficients)
modelEquation <- paste("log(salary) = ", round(a[1],2) , " + " , round(a[2],5)
                       , " * ceoten" , sep="")
details <- summary(regressionModel)
rSquared <- details$r.squared
rSquared
