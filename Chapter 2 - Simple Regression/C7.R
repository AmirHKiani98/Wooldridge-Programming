
# Setting the script directory as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Reading the data
charity <- read.csv("CHARITY.csv")

# (i) - What is the average gift in the sample of 4,268 people (in Dutch guilders)?
#  What percentage of people gave no gift?
giftAvg <- mean(charity$gift)
noGiftPercent <- (length(charity$gift[charity$gift == 0])/nrow(charity))*100
giftAvg
noGiftPercent

# (ii) - What is the average mailings per year? What are the minimum and maximum
#  values?
mailAvg <- mean(charity$mailsyear)
minMail <- min(charity$mailsyear)
maxMail <- max(charity$mailsyear)
mailAvg
minMail
maxMail

# (iii) - Estimate the model gift = beta0 + beta1 * mailsyear + u by OLS and 
# report the results in the usual way, including the sample size and R -squared
regressionModel <- lm(charity$gift~charity$mailsyear)
a <- regressionModel$coefficients
modelEquation <- paste("gift = ", round(a[1],2) , " + (" , round(a[2],5)
                       , ") * mailsyear" , sep="")
details <- summary(regressionModel)
rSquareds <- details$r.squared
n <- nrow(charity)
modelEquation
rSquareds
n

# (iv) - Interpret the slope coefficient. If each mailing costs one guilder, is 
# the charity expected to make a net gain on each mailing? Does this mean the 
# charity makes a net on every mailing? Explain.
"
The slope which is 2.64955 shows changing in mailsyear by one, changes the gift by
2.64955. if each mailing cost 1 guilder, so the profit is the remain of 2.64955 - 1
"

# (v) - What is the smallest predicted charitable contribution in the sample? 
# Using this simple regression analysis, can you ever predict zero for gift ?
smallestFit <- a[1] + a[2] * min(charity$mailsyear)
smallestFit
"
when there is no mailing, the fitted model shows that there is about 2.01 gift.
whereas , the minimum of the fitted value is about 2.67. so we can't predict the value
of the fitted line in zero mailing.
"
