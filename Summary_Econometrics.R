# Vedang Patel
# R Commands for the Econometrics use


#-------------------------------------------------------------------------------

z = (1/2)           # Computing Z
pnorm(z)            # required probability
# Answer is 0.6914625

z = (-3/4)      # Computing Z
pnorm(z)        # required probability
# Answer is 0.2266274

# Probability = 0.08
qnorm(0.08)         # required lower limit
#Answer is -1.4050


# Purpose:    Sample distributions

# the following creates vector x
x <- c(195, 187, 182, 198, 159, 179, 197, 161, 196, 159)
x <- c(x,151, 171, 180, 180, 185, 167, 159, 166, 167, 155)
x <- c(x,189, 166, 169, 198, 184, 181, 175, 169, 186, 177)
x <- c(x,164, 175, 159, 185, 164, 185, 155, 166, 153, 153)
x <- c(x,158, 161, 184, 161, 158, 186, 193, 195, 198, 184)

# a. creating histogram
hist(x)

# b. 
mean_x <- mean(x)
# sample mean = 174.5

var_x <- var(x)
# sample variance = 206.908

# c.
summary(x)
# We get this output:
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 151.0   161.0   175.0   174.5   185.0   198.0 


# Purpose:   Point estimators

# creating data: reading data to variable mydata
mydata <- c(12,26,18,22,10,16,13,21,31,19,16,21,11,22,33,44,15)

# a.
# The sample mean is an unbiased estimator of the population mean.
# Its value is a point estimate of population mean.
# Her is how we calculate it:
mean(mydata) # 20.58824

# b.
# The sample variance is an unbiased estimator of the population variance.
# Its value is a point estimate of the population variance.
# Here is how we calculate it:
var(mydata) # 78.88235

# c.
# The sample variance divided by sample size is an unbiased estimator
# of the variance of the sample mean. Its value is a point estimate
# of the variance of the sample mean. 
# Here is how we calculate it:
var(mydata)/length(mydata) # 4.640138

#d.
# The sample proportion is an unbiased estimator of the population proportion.
# Its value is a point estimate.
# Here is how we calculate it:
length(mydata[mydata<18])/length(mydata)  # 0.4117647


# Purpose:      point and interval estimation

# creating data:
x1 <- c(10,  12,  15,  43,  25,  32,  12,  35,  21,  15)
x1 <- c(x1, 18,  52,  15,  37,  57,  30,  10,  35,  16,  45)

n <- length(x1) # sample size

# a.
se <- sd(x1)/sqrt(n) # 3.271789

# b.
t.value <- qt(0.95,n-1) # 1.729133

# c.
x1bar <- mean(x1)   # sample mean
ME <- t.value*se  # margin of error
CI <- c(x1bar-ME,x1bar+ME)  # CI = (21.09264, 32.40736)


# We are reading values from .csv file
# read.csv() cmd read the value of .csv file and put in the tabular form
# In case of different file location we need to change the location of file
x2 = read.csv(file = "C:/Users/vedan/Desktop/UWindsor/SEM-6 WINTER 2023/ECON-2120/Assignments/HW3/HPLC1.csv", header = TRUE)
y = x2[ ,1]  # extracting only clo1 from x

# sample variance = sple_var
sple_var = var(y)   # var(x) gives the sample variance of x
# Answer of sple_var = 10.134

# The scientists at the pharmaceutical company could not conclude that the new method of
# determining drug concentration is less variable than the standard method
# H0 : sple_var0 = 9
# H1 : sple_var > 9
# Here, sple_var0 is an unbiased estimator of popln variance, so let's called it as 
# sig-sq = 9

#alpha = 0.01
# Lets calculate critical value first called chisqr with n = 50 => n-1= 49 df
chisqr = qchisq(0.01, 49, lower.tail = FALSE) # Answer = 74.92
# And our teststat (test statistic),
teststat = 49*sple_var/9
# Answer, teststat =  55.18
# Here, teststat < critical-value, therefore, we FAIL TO REJECT H0


# n = 12, two-sided alternative, alpha = 0.05
# We are reading values from .csv file
# read.csv() cmd read the value of .csv file and put in the tabular form
# In case of different file location we need to change the location of file
x3 = read.csv(file = "C:/Users/vedan/Desktop/UWindsor/SEM-6 WINTER 2023/ECON-2120/Assignments/HW3/SALES2.csv", header = TRUE)
# Lets calc dbar first then sample variance of dbar, sdbar
dbar = mean(x3[ ,3]-x3[ ,4])# calc mean from .csv file, by doing col3-col4
# answer mean = 82
sdbar = var(x3[ ,3]-x3[ ,4])   # calc var
# answer dbar = 1023.27
# Now calculate critical value, t
t = qt(0.025, 11, lower.tail = FALSE)   # t = 2.20
# Now lets calc test statistic ts,
ts = dbar/sdbar/sqrt(12)
# answer = 0.023
# So, our |t| < t
# Therefore, decision-rule: We fail to reject null Hypotheses


#Assigning values to the object x
x4<-c(11, 9, 6, 5, 10, 8.5, 7.2)
#Assigning values to the object y
y1<-c(200, 250, 415, 430, 220, 220, 340)
# To create a scatter plot of x and y
plot(x4, y1)

# To find b1
#We need to find sample covariance between x and y, lets say covxy
#Then we need to divide, covxy by sample variance of x, lets say s2x
#So, b1 = covxy/s2x
covxy = cov(x4,y1)  # cov(x,y) find the covariance between x and y, Answer = -199.92
s2x = var(x4)      # Answer = 4.64
b1 = covxy/s2x    # Answer = -43.12
#And, b0 = ybar - b1*xbar

#Lets calculate xbar and ybar first
y1bar = mean(y1)
x4bar = mean(x4)
b0 = y1bar - b1*x4bar     # Answer = 645.67


# sales and price data from excel file
sales <- c(420,380,350,400,440,380,450,420)
price <- c(320,345,390,310,290,330,280,300)
# now we find the coefficients of the regression by using lm() func first
#lm() find the regression of sales on prices
report <- lm(sales ~ price)   # Answer = 
#now to get the report we use coef() func which returns the coeeficients
summary <- coef(report)     # 695.991     -0.908

#Now lets plot the sales vs price graph
plot(price, sales, xlab = "Sales", ylab = "Price", main = "Scatterplot of sales and price with regression line")
#abline() func used to create a regression line, using the coefficient by lm() func
abline(report, col = "blue")

#The coefficent from the lm func helps us to tell/predict about the effect of a $30
#increase in price to have on sale would be.
#Therefore, for $30 price increase, there would be 
#decrease in sale by 30*.908 = 27.24 


# Purpose:  Simple Linear Regression
# Assumption:  data file is in the working directory
# reading data to mydata:
mydata1 <- read.csv(file="C:/Users/vedan/Desktop/UWindsor/SEM-6 WINTER 2023/ECON-2120/Assignments/HW4/New York Stock Exchange Gains and Losses.csv",header=TRUE) 
y2 <- mydata1$Percent.Loss.Y # extracting y; it's more convenient to use shorter names
x5 <- mydata1$Percent.Gain.X # extracting x

# a.
# Approach#1: quick, short solution
myslr <- lm(y2~x5) #run simple linear regression; store results in myslr
se <- summary(myslr)$sigma 
# summary(myslr) contains 12 objects 
# related to the regression stored in myslr
# the list of them can be found by typing names(summary(myslr))
# sigma is one of the 12 objects; reference is via $
sigma.hat.sq <- se^2 # this is what we need to compute
# this is based on eq. (11.14) in text
# sigma.hat.sq=0.4128; 
#    this is "model error variance", AKA "variance of the error terms";
#    it is an estimate for sigma-squared.

# Approach#2: long solution using formulas from text
# Several facts: 
#    se^2 is SSE/(n-2)
#    R^2 = 1- SSE/SST
# Thus, SSE = (1-R^2)*SST
n2 <- length(y2) # number of observations, i.e. sample size
SST <- sum((y2-mean(y2))^2)
R.sq <- summary(myslr)$r.squared # retrieve R^2
SSE <- (1-R.sq)*SST
sigma.hat.sq.alt <- SSE/(n2-2) # alternative computed value for sigma.hat.sq
# We can see that both approaches yield same result.

# b.
# Approach#1: quick, lazy:
summary(myslr)$coefficients
# We see that sample st.dev.(b1)=0.01729417
# Thus, square this number to find sample variance of b1:
sb1.sq <- 0.01729417^2
# sb1.sq = 0.0002990883
# Approach#2: long solution; based on formulas from lectures and text
# Recall: unbiased sample variance estimator for b1: se^2/Sxx
Sxx <- sum((x5-mean(x5))^2) # total sum of squares for x
sb1.sq.alt <- se^2/Sxx         # see first equality in eq.(11.16) in text
# Both approaches yield same result:
# Sample variance of b1 = 0.000299

# c. CI for b1
alpha1 <- 0.01 # for 99% CI
# Approach#1: quick, lazy
myCI1 <- confint(myslr,level=1-alpha1)
# We see that CI = (0.04101534, 0.1381164)
# Approach#2: long solution
mycoef <- coef(myslr) # extracting coefficients of regression
b0 <- mycoef[1]
b1 <- mycoef[2]
sb1 <- sqrt(sb1.sq)
LCL1 <- b1 - qt(1-alpha1/2,n-2)*sb1
UCL1 <- b1 + qt(1-alpha1/2,n-2)*sb1
# We see that CI = (LCL1,UCL1) = (0.04101534, 0.1381164)
# i.e., same result as in Approach#1 (ignoring rounding errors)

# part d
alpha2 <- 0.05 # significance level 5%
# H0: b1=0,  H1: b1 is not equal to zero
# Approach#1: based on CI; we recycle formulas from part c above
#   But remember: our alpha2=0.05 is now different from alpha1 in part c.
LCL2 <- b1 - qt(1-alpha2/2,n-2)*sb1
UCL2 <- b1 + qt(1-alpha2/2,n-2)*sb1
# Since CI2=(LCL2,UCL2)=(0.0538,0.1253) does not contain zero, we reject H0
# Approach#2: based on t-statistic and critical value
summary(myslr)$coefficients
# We can see that the t-statistic for b1 (found under "t value")
# is 5.179.
# The critical value t(n-2,alpha/2) is
qt(1-alpha2/2,n-2)
# and found to be t(n-2,alpha2/2) = t(23,0.025) = 2.069
# Since 5.179 > 2.069, we reject H0.