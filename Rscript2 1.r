# A well known University collects information about all the students enrolled in its "Economics" programme.
# The dataset "d2.csv" includes the following variables:

# sex of the student

# HIGH SCHOOL OUTCOMES:
  ### Type of high school attended by the student: 
     # H.publ = Humanities, public school
     # H.priv = Humanities, private school
     # S.publ = Sciences, public school
     # S.priv = Sciences, private school
  ### Average grades achieved by each student during high school:
    # history, maths, literature, sciences, language. All grades are between 60 and 100.

# UNIVERSITY OUTCOMES:
  # The final grade (60-100).
  # The number of failed exams during the student's career (n.fails). Note that the same exam can be failed multiple times.
  # A variable "on_time" which is 1 if the students completed the programme within 5 years, and zero otherwise.

# Goal: see if high school outcomes are good predictors of the student's performance during university.

#############################################################################################################

# Q0. Load the data and attach them!
setwd("C:/Users/a020554/OneDrive - University of Pisa/Desktop/didattica/Topics in microeconometrics/000 data_and_rscript_for_labs")
d <- read.csv("d2.csv", sep = ";", dec = ",") 
attach(d)

# Q1. Create a data.frame "grades" that only includes the high school grades and the final university grade.
grades <- d[, 3:8]
grep("grade", names(d))
grades <- d[grep("grade", names(d))] #!!!!!!!!
#function that tells me to select only columns whose names contains "grade"
class(grades)


# Q2. For each pair of grades, create a scatterplot.
plot(grades) #works because it is a dataframe (in a matrix does not work)

plot(as.matrix(grades)) #non funziona


# Q3. For each pair of grades, compute Pearson's and Spearman's correlation.
round(cor(grades), 2) #Pearson which is parametric
round(cor(grades, method = "spearman"), 2)

# Q4. Visualize graphically the correlation matrix obtained in Q3 (try corrplot{corrplot}).
# install.packages("corrplot") # if missing
install.packages("corrplot")
library(corrplot)

corrplot(cor(grades)) 

# Q5. For each pair of grades, compute the p-value of Pearson's and Spearman's correlation test (try cor.mtest{corrplot}).
# Reminder: for two variables only, you can do cor.test(x,y)
cor.mtest(grades)
round(cor.mtest(grades)$p, 3) #pearsons linear coff (P). Most corr are Higly significant because H0: cor=0
round(cor.mtest(grades, method = "spearman")$p, 3) #spearman

# Q6. Create a new variable "area" which takes value "H" for "Humanities" and "S" for "Sciences".

area <- ifelse(grepl("H", high_school), "H", "S") # WHY if else(condition,whatiftrue,whatiffalse)

area <- substr(high_school, 1,1) # Easier but not very general we use a substring from number 1 letter to number 1 which is 1,1
area

# Q7. Create a new variable "type" which takes value "publ" or "priv" 
# depending on the type of high school attended by the student.

type <- substr(high_school, 3,6) #because both have 4 characters left
type <- substr(high_school, 3, nchar(high_school) ) #nchar identifies number of characters

# Q8. Compute the quartiles of each variable in "grades", separating public and private high schools.

lapply(grades, function(x) tapply(x, type, quantile, c(0.25, 0.5, 0.75))) # when sapply doesnt work we use lapply 

#sapply does not know how to organize the output for each grade we have a 3x2 matrix

# Q9. For each variable in "grades", calculate the p-value of a test that compares males with females.
round(sapply(grades, function(x) wilcox.test(x ~ sex)$p.value), 3)

# Q10. Compute an average high school grade, by averaging the grades 
  # in history, maths, literature, sciences, and language.
avg_hs <- rowMeans(grades[,1:5])

# Q11. For each individual, compute a "median high school grade", defined as the median 
 # of the 5 grades achieved in history, maths, literature, sciences, and language.
me_hs <- apply(grades[1:5], 1, median)
# apply(dataframe, dimension , function) dimension =1 for rows dim=2 for columns

# Q12. Estimate a linear regression model "m1" in which you predict 
 # the final university grade with the high school grades. Briefly comment.
m1 <- lm(final_grade_uni ~ ., data = grades)
summary(m1)

# Q13. Go back to Q5: as you can see, cor.test(final_grade_uni, grade_history) returns a significant p-value, while
  # grade_history is no longer significant in model "m1". Can you guess how this happens?

# Answer: grade_history is correlated to final_grade_uni only through some common
  # factor (like: intelligence), but is not anymore correlated when the other
  # grades are adjusted for.


# Q14. I just came out of college with the following grades: 
  # history = 87, maths = 97, literature = 99, sciences = 78, language = 86.
  # Based on model m1, what is a "prediction" of my final university grade? And what is the interpretation of such prediction?
predict(m1, newdata = data.frame(
  grade_history = 87,
  grade_math = 97, 
  grade_literature = 99,
  grade_sciences = 78,
  grade_language = 86
))

# I predict a final university grade of about 91.
# The prediction is a conditional MEAN, because of OLS.
# It is the mean predicted uni grade for people with the above characteristics.
  # Based on the model, they will have a normally distributed grade with mean
   # 91 and some variance.


# Q15. Repeat estimation of model "m1" (create model "M1") using maximum likelihood. 
  # Any difference? Also compute standard errors.

loglik.norm.homo <- function(theta, y, X){
  sigma <- exp(theta[1])
  beta <- theta[-1]
  mi <- X%*%beta
  -sum(dnorm(y, mi, sigma, log = TRUE))
}

y <- final_grade_uni
X <- cbind(1, as.matrix(grades[1:5]))
start <- c(log(sd(y)), mean(y), 0,0,0,0,0)

M1 <- nlm(loglik.norm.homo, start, y = y, X = X, hessian = TRUE)

cbind(m1$coef, M1$estimate[-1]) # coefficients are the same, up to numerical approx

exp(M1$estimate[1]) # residual standard deviation = 3.603153.
# In summary(m1), you get residual standard error = 3.617.
# The ML estimator of sigma is always slightly smaller than the OLS,
  # because the variance is divided by "n" in ML, and by n - q in OLS.

se <- sqrt(diag(solve(M1$hessian)))
se <- sqrt(diag(chol2inv(chol(M1$hessian)))) # MUCH BETTER (and faster)

# Compare estimated s.e.
cbind(ML = se[-1], OLS = summary(m1)$coef[,2])
# not exactly the same, again because of different estimates of sigma

# Q16. Use a scatterplot to verify that the residual variance of model m1 
  # is an increasing function of grade_math.
plot(grade_math, m1$residuals)


# Q17. Fit a regression model "M2" in which you allow for data heteroskedasticity, 
 # modeling sigma appropriately.
loglik.norm.hetero <- function(theta, y, X){
  q <- ncol(X)
  phi <- theta[1:q]
  beta <- theta[(q + 1):(2*q)]

  sigma <- exp(X%*%phi)
  mi <- X%*%beta

  -sum(dnorm(y, mi, sigma, log = TRUE))
}

y <- final_grade_uni
X <- cbind(1, as.matrix(grades[1:5])/100)
start <- c(log(sd(y)), 0,0,0,0,0, mean(y), 0,0,0,0,0)

M2 <- nlm(loglik.norm.hetero, start, y = y, X = X, hessian = TRUE, iterlim = 1000)
M2

# To understand how to use the model
predict.mean <- X%*%M2$estimate[7:12] # n predicted means
predict.sd <- exp(X%*%M2$estimate[1:6]) # n predicted standard deviations

# For example, observation n. 1 has the following mean and sd:
predict.mean[1]
predict.sd[1]

# The ML estimator of beta is NOT identical to the OLS (which however is still valid).
# In ML, the OLS criterion will automatically be weighted by the inverse of the variance.



# Q18. Compare M1 with M2, using a suitable test.

# M1 is nested into M2, and assumes homoskedasticity.
# I can use Likelihood Ratio Test, which in this case will be a formal test
  # for the null hypothesis H0: there is homoskedasticity.

l1 <- -M1$minimum
l2 <- -M2$minimum
Lambda <- 2*(l2 - l1)
1 - pchisq(Lambda, df = 5) # p-value
# I reject H0 and conclude that there is heteroskedasticity.

# Q19. Plot a N(0,1) density together with a Logistic(0,1). Generate random numbers from these distributions.

grid <- seq(-10, 10, length = 382)
plot(grid, dnorm(grid), type = "l")
lines(grid, dlogis(grid), col = "red")

par(mfrow = c(1,2))
hist(rnorm(10000), main = "N(0,1)")
hist(rlogis(10000), main = "Logis(0,1)")

# Q20. Fit a model "M3" in which you use a logistic distribution instead of a Normal.
  # Which model would you choose between M2 and M3?

loglik.logis.hetero <- function(theta, y, X){
  q <- ncol(X)
  phi <- theta[1:q]
  beta <- theta[(q + 1):(2*q)]

  sigma <- exp(X%*%phi)
  mi <- X%*%beta

  -sum(dlogis(y, mi, sigma, log = TRUE))
}

y <- final_grade_uni
X <- cbind(1, as.matrix(grades[1:5])/100)
start <- c(log(sd(y)), 0,0,0,0,0, mean(y), 0,0,0,0,0)

M3 <- nlm(loglik.logis.hetero, start, y = y, X = X, hessian = TRUE, iterlim = 1000)
M3

cbind(M2 = M2$estimate, M3 = M3$estimate)
# The two models are NOT nested. I could use AIC or BIC, but
  # M2 and M3 also have same number of parameters and same number of observations...
# so, just compare the two log-likelihoods!!

-M2$minimum
-M3$minimum
# Largest maximized log-likelihood: M2. I prefer the Normal model to the Logistic one.


# Q21. It would be nice to create a function "myfit" that either fits a Normal or a Logistic model,
  # depending on an option "distr", and models the scale parameter depending on an option "sigma" = TRUE/FALSE.

# Q22. Represent graphically the distribution of the variable "n.fails". Create a suitable regression model "A1"
  # to describe how this variable depends on high school outcomes. Briefly comment.

barplot(table(n.fails))
dat <- d[, c(3:7, 9)]

A1 <- glm(n.fails ~ ., data = dat, family = poisson)
summary(A1)

# All predictors are significant and have negative coefficients.
# Larger high school grades reduce the average number of failed exams.
# Do not forget that the coefficients show the effect of the predictors on log(lambda).

# Q23. Generate random numbers from the fitted distribution letting all grades = 60. 
 # Try with all grades = 100, compare the results.

lambda60 = exp(A1$coef[1] + sum(A1$coef[-1])*60)
lambda100 = exp(A1$coef[1] + sum(A1$coef[-1])*100)

lambda60 
lambda100 

par(mfrow = c(1,2))
barplot(table(rpois(10000, lambda60)), main = "lambda(x = 60)", xlab = "n. of failures")
barplot(table(rpois(10000, lambda100)), main = "lambda(x = 100)", xlab = "n. of failures")


# Q24. Repeat estimation using a home-made routine.

loglik.pois <- function(beta, X, y){
  -sum(dpois(y, exp(X%*%beta), log = TRUE))
}

y <- n.fails
X <- cbind(1, as.matrix(grades[1:5])/100)
start <- c(log(mean(n.fails)),0,0,0,0,0) 
# or I could use log(var(n.fails)), as in the Poisson mean = var = lambda.

A2 <- nlm(loglik.pois, start, X = X, y = y, hessian = TRUE)
A2

# Compare coefficients
cbind(A2$estimate, A1$coef*c(1, 100, 100, 100, 100, 100))

sqrt(diag(chol2inv(chol(A2$hessian)))) # s.e.
# Compare with Std. Error in summary(A1)



# Q25. Represent graphically the distribution of the variable "on_time". Create a suitable regression model "B1"
  # to describe how this variable depends on n.fails + high school outcomes + sex + type + area. Briefly comment.

pie(table(on_time))
B1 <- glm(on_time ~ n.fails + sex + type + area + 
  grade_history + grade_math + grade_literature + grade_sciences + grade_language, family = binomial)
summary(B1)
# If you failed more exams, you are less likely to be on_time


# Q26. I am male, and I studied humanities in a public school with the following grades:
  # history = 87, maths = 97, literature = 99, sciences = 78, language = 86. During university, I only had to repeat
  # one exam once. Can you guess if I got my degree on time or not?

predict(B1, type = "response", newdata = data.frame(sex = "m", area = "H", type = "publ", n.fails = 1,
  grade_history = 87, grade_math = 97, grade_literature = 99, grade_sciences = 78, grade_language = 86
  )
)
# about 48%.

# Q27. Considering the same individual of Q26, let the grade in maths range between 60 and 100, and see
  # what happens with the prediction.

x <- 60:100
pred <- predict(B1, type = "response", newdata = data.frame(sex = "m", area = "H", type = "publ", n.fails = 1,
  grade_history = 87, grade_math = x, grade_literature = 99, grade_sciences = 78, grade_language = 86
  )
)
plot(x, pred) # This is going to be a "branch" of a logistic curve!

# Q28. Repeat estimation using a home-made routine.

loglik.logit <- function(beta, y, X){
  eta <- exp(X%*%beta)
  p <- eta/(1 + eta)
  -sum(dbinom(y, 1, p, log = TRUE)) # or -sum(y*log(p) + (1 - y)*log(1 - p))
}

p <- mean(on_time)
start <- c(log(p/(1 - p)), rep(0,9))

B2 <- nlm(loglik.logit, start, y = on_time, X = model.matrix(B1), iterlim = 10000, hessian = TRUE)
# or use X = cbind(1, n.fails, grade_history, grade_math, grade_literature, grade_sciences,
   gade_language, sex == "m", type == "publ", area == "S")

cbind(glm = B1$coef, myfit = B2$estimate) # the same, up to numerical approximation

# for standard errors
sqrt(diag(chol2inv(chol(B2$hessian))))

# Q29. Ask yourself the following question: could I model the final_grade_uni by using a Poisson distribution?
  # It is strictly positive; it is discrete. What do you think?

# NO! final_grade_uni is not a count!

# Q30. What about using a Gamma distribution? Fit a model (no predictors).
  # Any idea for the starting points? Compare the fitted density with the actual histogram. 

loglik.gamma <- function(theta, y){
  -sum(dgamma(y, exp(theta[1]), exp(theta[2]), log = TRUE))
}

# In a gamma(a, b),
# mean = a/b
# var = a/b^2

# mean/var = b
# mean^2/var = a

mu <- mean(final_grade_uni)
v <- var(final_grade_uni)
start <- log(c(mu^2/v, mu/v)) # Moment estimators
# A very simple, inefficient estimator! NOT the same as ML estimator!

U <- nlm(loglik.gamma, start, y = final_grade_uni)

hist(final_grade_uni, freq = FALSE) # to calculate density and not absolute freq
grid <- seq(60,100, length = 1000)
lines(grid, dgamma(grid, exp(U$estimate[1]), exp(U$estimate[2])))



