

# The dataset "d5.csv" includes data on 621 small municipalities:
  # altitude above sea level
  # forest = a binary indicator of whether the town is surrounded by forest
  # pop = population
  # age = average age
  # gov = local government (left/right)

# A while ago, the regional government offered each municipality to participate in a "we_green" program,
  # that included financial support to create a service of car/bike sharing. The goal was to reduce air pollution
  # by limiting the use of private cars. Only some municipalities joined the program, as indicated by the binary
  # variable "we_green".

# The main question to answer: did the we_green program contribute to reduce air pollution?
# The dataset reports the following information:
  # quality = air quality, an indicator between 0 (unbreathable) and 100 (perfectly clean air).
  # car_use = an estimate of how much people use private cars (0-100).
  # pm10 = the amount of PM10, an important component of the particulate suspended matter associated with respiratory diseases.



#####################################################################################################

# Q0. Load the data and attach them!

library(splines) # already installed
library(lmtest) # if missing, install.packages("lmtest")
library(quantreg) # if missing, install.packages("quantreg")


setwd("C:/Users/a020554/OneDrive - University of Pisa/Desktop/didattica/Topics in microeconometrics/000 data_and_rscript_for_labs")
d <- read.csv("d5.csv", sep = ";", dec = ",") 
attach(d)

# Q1. Represent graphically each variable in the dataset.
hist(altitude)
pie(table(forest))
hist(pop)
hist(age)
pie(table(gov))
pie(table(we_green))
hist(quality)
hist(car_use)
hist(pm10)

# Q2. Show how air quality differs between "green" and "not green" municipalities. 
  # Represent the situation graphically, report descriptive statistics, perform a suitable test.

boxplot(quality ~ we_green)
tapply(quality, we_green, summary)
# mean difference is about 10

t.test(quality ~ we_green) # P
wilcox.test(quality ~ we_green) # NP
# significant differences

# Q3. Show how the choice to participate in the "we_green" program may depend on the following variables:
  # local government, average age, population, altitude, forest. Use a suitable regression model. Hint: include
  # population and altitude in hundreds, to get more readable coefficients.

m <- glm(we_green ~ forest + I(pop/100) + I(altitude/100) + age + gov, family = binomial)
summary(m)
# More likely to participate in the program if: larger pop, left-wing government.

# Q4. Use simple plots and tests to show the association between 
  # air quality and each of the following variables:
  # local government, average age, population, altitude, forest.

boxplot(quality ~ gov)
plot(age, quality)
plot(pop, quality)
plot(altitude, quality)
boxplot(quality ~ forest)

# Let's do nonparametric test:
wilcox.test(quality ~ gov)$p.value
cor.test(age, quality, method = "spearman")$p.value
cor.test(pop, quality, method = "spearman")$p.value
cor.test(altitude, quality, method = "spearman")$p.value
wilcox.test(quality ~ forest)$p.value

# all correlations are significant, except from that between quality and gov.


# Q5. Which variables appear to be correlated with both the "treatment" 
  # (we_green) and the response (quality)?
  # What could be the role of these variables?

# It appears that population could be a confounder.
  # Larger population ---> more likely to participate in the program, but also more polluted!
  # The obvious risk is to UNDER-estimate the efficacy of the program. 

# Q6. Estimate a linear regression model "m1" in which you compare 
 # "green" with "not green" municipalities in terms of air quality.

m1 <- lm(quality ~ we_green)
summary(m1)
# The mean difference is about 10, this is equivalent to 
t.test(quality ~ we_green, var.equal = TRUE) # do not forget that "lm" assume homoskedasticity!

# Q7. Do you think that m1 under- or over-estimates the impact of the we_green program?
# UNDER

# Q8. Estimate a model "m2" in which you add pop, age, gov, altitude, forest. What happens, and why?
m2 <- lm(quality ~ we_green + I(pop/100) + age + gov + I(altitude/100) + forest)
summary(m2)

# The effect of the program is about 17! This is because I removed confounding.

# Q9. In "m2", the intercept does not make much sense... why? Is that a real problem?

# 81.65633 = mean air quality in a municipality with the following characterstics:
  # did not participate in the program (ok)
  # pop = 0 (uhm...)
  # avg age = 0 (UHHHMMM...)
  # gov = left (ok)
  # altitude = 0 (ok)
  # forest = 0 (ok)

# This is NOT a real problem. I care about the coefficient of we_green.

# Q10. Fix the non-problem identified in Q9.
m2bis <- lm(quality ~ we_green + I(pop/100 - 5) + I(age - 40) + gov + I(altitude/100) + forest)
summary(m2bis) # now the intercept is mean quality if pop = 500, age = 40.


# Q11. A municipality has the following characteristics: 
  # pop = 1000, average age = 42, gov = left, altitude = 500, forest = 0.
  # Based on model m2, what is the predicted air quality if we_green = 0, and what if we_green = 1?

predict(m2, newdata = data.frame(pop = 1000, age = 42, gov = "left", altitude = 500, 
  forest = 0, we_green = 0:1))
# These two are predicted MEANS, and obviously they differ by 16.6, the coeff of we_green.

# Q12. Both the "forest" and the "altitude" predictors are significant in m2. The effect of altitude, however, may also depend
  # on the presence of forests, and the effect of a forest may depend on altitude. Note, for example, that
  # you have different types of trees at different altitudes. Can you add something to model "m2"?
  # Fit a model "m3". What is the coefficient of altitude when forest = 0? And when forest = 1?
  # What is the difference between towns with and without a forest, when altitude = 500? And when altitude = 1000?

m3 <- update(m2, . ~ . + forest*I(altitude/100))
summary(m3)

# coef of altitude:
  # 1.26755, if forest = 0
  # 1.26755 + 0.86796 = 2.13551, if forest = 1

# Effect of forest:
# Altitude = 500: 2.47612 + 5*0.86796
# Altitude = 1000: 2.47612 + 10*0.86796

# Q13. Add the car_use to model "m3". What happens? What is the role of this predictor?
m4 <- update(m3, . ~ . + car_use)
summary(m4)
# we_green is no longer significant. Car use is a mediator.


# Q14. Revisit model m3 using median regression instead. Compute standard errors using bootstrap (R = 1000). Briefly comment.

q1 <- rq(formula(m3), tau = 0.5)
summary(q1, se = "boot", R = 1661)
# The coefficients describe the effect of each predictor and the median response.

# Q15. Represent graphically the association between car_use and pm10. 
  # Which correlation coefficient is the most appropriate, Pearson's or Spearman's?
plot(car_use, pm10)
cor(car_use, pm10, method = "spearman")

# Q16. Model the association between car_use and pm10 using a linear regression in which you
  # include a 3rd-degree polynomial. Plot the resulting mean regression line. Plot the estimated
  # quantiles 0.05 and 0.95.

a <- lm(pm10 ~ car_use + I(car_use^2) + I(car_use^3))
summary(a)

grid <- 0:100 # seq(0, 100, length = 549)
pred <- predict(a, newdata = data.frame(car_use = grid))
plot(car_use, pm10); lines(grid, pred, col = "red", lwd = 2)
lines(grid, pred - 1.64*2.885, col = "green", lwd = 2)
lines(grid, pred + 1.64*2.885, col = "blue", lwd = 2)
# These are predicted quantiles from a Normal homoskedastic distribution.
  # They will not do well, if the data are non-normal or heteroskedastic.

# Q17. Repeat Q16, using a Normal heteroskedastic model.
loglik.norm.hetero <- function(theta, y, X){
  q <- ncol(X)
  beta <- theta[1:q]
  phi <- theta[(q + 1):(2*q)]

  mu <- X%*%beta
  sigma <- exp(X%*%phi)
  -sum(dnorm(y, mu, sigma, log = TRUE))
}

x <- car_use/100 # always scale variables
X <- cbind(1, x, x^2, x^3)
start <- c(mean(pm10), 0, 0, 0, log(sd(pm10)), 0, 0, 0)
A <- nlm(loglik.norm.hetero, start, y = pm10, X = X)

# To make prediction, I construct the equivalent of "X" but on a grid.
  # Note that the model is based on x = car_use/100: therefore, the grid is in (0,1).
grid <- seq(0,1, length = 515)
G <- cbind(1, grid, grid^2, grid^3)
pred.mu <- G%*%A$est[1:4]
pred.sigma <- exp(G%*%A$est[5:8])

# To be on the same scale as the predictions, I plot car_use/100
plot(car_use/100, pm10); lines(grid, pred.mu, col = "red", lwd = 2)
lines(grid, pred.mu - 1.64*pred.sigma, col = "green", lwd = 2)
lines(grid, pred.mu + 1.64*pred.sigma, col = "blue", lwd = 2)



# Q18. Repeat again, using quantile regression with tau = (0.05, 0.25, 0.5, 0.75, 0.95).
b <- rq(formula(a), tau = c(0.05, 0.25, 0.5, 0.75, 0.95))
b$coef

pred <- predict(b, newdata = data.frame(car_use = grid))
plot(car_use, pm10); for(i in 1:5){lines(grid, pred[,i])}

# Q19. Repeat again, modeling the association with a natural cubic spline 
 # with 3 internal knots at the empirical quartiles of car_use.
b2 <- rq(pm10 ~ ns(car_use, df = 4), tau = c(0.05, 0.25, 0.5, 0.75, 0.95))
pred <- predict(b2, newdata = data.frame(car_use = grid))
plot(car_use, pm10); for(i in 1:5){lines(grid, pred[,i])}

# How to choose among models with df = 3, 4, 5, ...?
# Use AIC or BIC, choose the model with smaller AIC or BIC.
# BIC will always tend to choose "small", parsimonious models. AIC will do the opposite.

# Unfortunately, AIC and BIC are likelihood-based methods and are not available for 
  # quantile regression, which is NOT a likelihood method.


# Q20. Fit a beta distribution to pm10/100. Represent the result graphically.
y <- pm10/100
loglik.beta <- function(theta, y){
  a <- exp(theta[1])
  b <- exp(theta[2])
  -sum(dbeta(y, a, b, log = TRUE))
}
M <- nlm(loglik.beta, c(0,0), y = y)
M

hist(y, freq = FALSE)
grid <- seq(0,1, length = 872)
lines(grid, dbeta(grid, exp(M$est[1]), exp(M$est[2])), lwd = 2)


# Q21. The variable car_use is based on the following information: in each municipality, we collected a random sample 
  # of size n_j = pop_j*0.05. We asked everyone "did you use the car yesterday?". The car_use was then calculated as
  # the sample proportion (x100) of positive answers. This means that, in reality, car_use is just an estimate.
  # Compute two new variables: car_use_L and car_use_R, that correspond to the extremes of a 95% confidence interval.

# 95% confidence interval on a proportion: p +/- 1.96*sqrt(p*(1 - p)/n)
n <- pop*0.05
x <- car_use/100
car_use_L <- x - 1.96*sqrt(x*(1 - x)/n)
car_use_R <- x + 1.96*sqrt(x*(1 - x)/n)

# Q22. Fit a beta distribution to (car_use_L, car_use_R).
loglik.beta.ic <- function(theta, L, R){
  a <- exp(theta[1])
  b <- exp(theta[2])
  -sum(log(pbeta(R, a,b) - pbeta(L, a,b)))
}
fitbeta.ic <- nlm(loglik.beta.ic, c(0,0), L = car_use_L, R = car_use_R, hessian = TRUE)
fitbeta.ic
sqrt(diag(chol2inv(chol(fitbeta.ic$hessian)))) # standard errors of theta_hat.


# Q23. It is known that the instrument to measure pm10 can be inaccurate when the actual value of pm10 is less than 55.
  # For example, we don't believe that a 37 is an actual 37. It is more honest to say that "it is less than 55".
  # What type of data do you generate by using this idea?

pm10_new <- pmax(pm10, 55)
delta <- (pm10 > 55)
cbind(pm10, pm10_new, delta)
# The couple (pm10_new, delta) form a left-censored variable!!

# Q24. Fit a Gamma distribution to pm10, using the idea described in Q23.
loglik_gamma_leftcens <- function(theta, y,delta){
  a <- exp(theta[1])
  b <- exp(theta[2])
  log.f <- dgamma(y,a,b, log = TRUE)
  log.F <- pgamma(y,a,b, log = TRUE)
  -sum(delta*log.f + (1 - delta)*log.F)
}
# method of moments estimator is used as starting point for ML estimators 
 # (pretending there is no censoring):
start <- log(c(mean(pm10_new)^2/var(pm10_new), mean(pm10_new)/var(pm10_new)))
fitgamma_leftcens <- nlm(loglik_gamma_leftcens, start, y = pm10_new, delta = delta)

a <- exp(fitgamma_leftcens$estimate[1])
b <- exp(fitgamma_leftcens$estimate[2])

hist(pm10_new, br = 100, xlim = c(45, 70), freq = FALSE) # The mass is actually only due to left censoring!
g <- seq(45,70, length = 387) # grid
lines(g, dgamma(g,a,b), lwd = 2, col = "red")

# Q25. Fit a Normal homoskedastic model to describe the association between pop and altitude. Start with a linear association;
  # then, use splines (df = 3) to describe a potentially nonlinear association. Proceed in two different ways: y ~ spline
  # or y ~ linear + spline. Verify that the result is "the same". Is there a significant "deviation from linearity"? Perform a suitable test.

# LINEAR
o1 <- lm(pop ~ altitude)
summary(o1)
plot(altitude, pop); abline(o1)

# SPLINE
o2 <- lm(pop ~ ns(altitude, df = 3))
summary(o2)
grid <- seq(500,1500, length = 542)
pred <- predict(o2, newdata = data.frame(altitude = grid))
plot(altitude, pop); lines(grid, pred, col = "red", lwd = 2)

# LINEAR + SPLINE
o3 <- lm(pop ~ altitude + ns(altitude, df = 3))
summary(o3)
grid <- seq(500,1500, length = 542)
pred <- predict(o3, newdata = data.frame(altitude = grid))
plot(altitude, pop); lines(grid, pred, col = "red", lwd = 2)

# Important! The spline INCLUDES the linear function as special case.
# This is why, when you do linear + spline, the last element
  # of the spline cannot be estimated and one coefficient is dropped.
# For the same reason, o2 and o3 generate the SAME predictions
  # and have the SAME R^2.

# Is there a SIGNIFICANT deviation from linearity?

# Use ANOVA on o3:
summary(aov(o3)) # no significant deviation from linearity

# Alternative approach: compare o3 (linear + deviation) with o1 (linear only)
lrtest(o1, o3) # very similar p-value, around 0.24

# ANOVA is designed for linear models. LRT is much more general.
  # They are very similar, but not identical.

# Q25bis. Repeat estimation of the spline-based model, this time choosing between df = {2,3,4,5} using AIC and BIC.

h2 <- lm(pop ~ altitude + ns(altitude, df = 2))
h3 <- lm(pop ~ altitude + ns(altitude, df = 3))
h4 <- lm(pop ~ altitude + ns(altitude, df = 4))
h5 <- lm(pop ~ altitude + ns(altitude, df = 5))
# In general, models with different df are not nested,
 # because they have the knots in different positions.
# I cannot use likehood ratio test! Use AIC or BIC.

AIC(h2)
AIC(h3)
AIC(h4) # wins by a very small margin against h2
AIC(h5)

BIC(h2) # wins by a decent margin. BIC hates parameters.
BIC(h3)
BIC(h4)
BIC(h5)

# I would choose h2.

# Q26. Represent graphically the linear fit. Include a 95% confidence interval.
  # In addition, draw the predicted quantiles 0.025 and 0.975.

plot(altitude, pop)
abline(o1, col = "red", lwd = 2)
# But this is not enough for the rest... I need a grid.

g <- seq(min(altitude), max(altitude), length = 108) # a grid
pred <- predict(o1, newdata = data.frame(altitude = g), se = TRUE)

mean <- pred$fit
low <- pred$fit - 1.96*pred$se.fit
up <- pred$fit + 1.96*pred$se.fit
q0.025 <- mean - sd(o1$residuals)*1.96
q0.975 <- mean + sd(o1$residuals)*1.96

plot(altitude, pop)
lines(g, mean, col = "red", lwd = 2)
lines(g, low, col = "green", lwd = 2) # lower extreme of a 95% confidence interval
lines(g, up, col = "green", lwd = 2) # upper extreme of a 95% confidence interval
lines(g, q0.025, col = "blue", lwd = 2) # predicted quantile 0.025
lines(g, q0.975, col = "blue", lwd = 2) # predicted quantile 0.975

# Blue lines = predicted normal quantiles
# Green lines = confidence interval around the predicted mean.
# They are two totally different things!


# Q27. Represent graphically the spline-based fit. Include a 95% confidence interval.
  # In addition, draw the predicted quantiles 0.025 and 0.975.

# Same as Q26, just replace "o1" with "o2" everywhere!
g <- seq(min(altitude), max(altitude), length = 108) # a grid
pred <- predict(o2, newdata = data.frame(altitude = g), se = TRUE)

mean <- pred$fit
low <- pred$fit - 1.96*pred$se.fit
up <- pred$fit + 1.96*pred$se.fit
q0.025 <- mean - sd(o2$residuals)*1.96
q0.975 <- mean + sd(o2$residuals)*1.96

plot(altitude, pop)
lines(g, mean, col = "red", lwd = 2)
lines(g, low, col = "green", lwd = 2)
lines(g, up, col = "green", lwd = 2)
lines(g, q0.025, col = "blue", lwd = 2)
lines(g, q0.975, col = "blue", lwd = 2)


# Q28. Now use a quantile regression model (tau = 0.1,0.25,0.5,0.75,0.9). 
  # Assume a linear association. Represent the results graphically.


k1 <- rq(pop ~ altitude, tau = c(0.1,0.25,0.5,0.75,0.9))
# Here you have a simple model beta0 + beta1*x, and you can use abline...
plot(altitude, pop)
for(j in 1:5){abline(k1$coef[,j])}

# More general procedure: grid + predict
g <- seq(min(altitude), max(altitude), length = 108) # a grid
pred1 <- predict(k1, newdata = data.frame(altitude = g))
plot(altitude, pop)
for(j in 1:ncol(pred1)){lines(g, pred1[,j])}

# Q29. Repeat, using splines. Verify that rq(y ~ linear + spline) does not work. This is an example of lazy programming.

k0 <- rq(pop ~ altitude + ns(altitude, df = 3), tau = c(0.1,0.25,0.5,0.75,0.9)) # DOES NOT WORK!
  # Someone did not remove automatically the extra columns if X is singular. 
  # Not your fault! Shame on the programmers of the package.

k0 <- rq(pop ~ ns(altitude, df = 3), tau = c(0.1,0.25,0.5,0.75,0.9))
# Here there is no way to use "abline". Use grid + predict.
g <- seq(min(altitude), max(altitude), length = 548) # a grid
pred <- predict(k0, newdata = data.frame(altitude = g))
plot(altitude, pop)
for(j in 1:ncol(pred)){lines(g, pred[,j], lwd = 2, col = "red")}


# Q30. Fit another model as follows: lm(pop ~ ns(altitude, df = 3) + forest + quality). Represent graphically the
  # non-linear association between pop and altitude. How do you proceed? Hint: no scatterplot!

k1 <- lm(pop ~ ns(altitude, df = 3) + forest + quality)
grid <- seq(500, 1500, length = 439)
pred <- predict(k1, 
  newdata = data.frame(altitude = grid, forest = 0, quality = 50))

plot(pop ~ altitude); lines(grid, pred, col = "blue", lwd = 2)
# The above is "wrong". 

  # First, the predictions are not for
  # ALL data points, but only for those with the 
  # selected value of "forest" and "quality"
  # (these are meaningful, yet arbitrary values!).

  # Second, the ADJUSTED effect of a predictor might
  # VERY DIFFERENT from what you see in the plot!
  # Maybe, the plot shows a positive correlation between x and y,
  # but when you adjust for age, sex, whatever, the effect
  # becomes negative!!

# You ONLY plot the predictions! With NO scatterplot.

plot(grid, pred, xlab = "altitude", ylab = "predicted mean", type = "l")


# Q31. What about representing graphically lm(pop ~ ns(altitude, df = 3) + forest)?

e <- lm(pop ~ ns(altitude, df = 3) + forest)
summary(e)

# Here I could draw two lines, one when forest = 0, and the other when forest = 1.
grid <- seq(500, 1500, length = 514)
pred0 <- predict(e, newdata = data.frame(altitude = grid, forest = 0))
pred1 <- predict(e, newdata = data.frame(altitude = grid, forest = 1))
plot(altitude, pop); points(altitude[forest == 1], pop[forest == 1], col = "green")
lines(grid, pred0, lwd = 2); lines(grid, pred1, lwd = 2, col = "green")


# Q32. What can you do with lm(pop ~ ns(altitude, df = 3) * forest)?

# The same! But now the two lines will not be parallel, due to the interaction.
e <- lm(pop ~ ns(altitude, df = 3) * forest)
summary(e)

# all the same as in Q31!
grid <- seq(500, 1500, length = 514)
pred0 <- predict(e, newdata = data.frame(altitude = grid, forest = 0))
pred1 <- predict(e, newdata = data.frame(altitude = grid, forest = 1))
plot(altitude, pop); points(altitude[forest == 1], pop[forest == 1], col = "green")
lines(grid, pred0, lwd = 2); lines(grid, pred1, lwd = 2, col = "green")


# Q33. Fit a Pareto distribution (location = 500, shape = alpha) to the altitude. 
  # Use the "EnvStats" library, and note that "dpareto" does not have the "log" option.
  # Note that location = 500 is just a bit less than the minimum observed altitude. 
  # Model the shape on the log scale, and use stepmax = 10 to avoid numerical problems (I tried this before...).
  # For the starting point, use the fact that mean = alpha*location/(alpha - 1).
  # Create a histogram, and draw the Pareto fit.

library(EnvStats) # install.packages("EnvStats") if missing
loglik.pareto <- function(theta, y){
  alpha <- exp(theta)
  -sum(log(dpareto(y, location = 500, shape = alpha)))
}

# mu = alpha*location/(alpha - 1), which means that alpha = mu/(mu - location).
  # A valid method-of-moment estimator is:
hat_mu <- mean(altitude)
alpha0 <- hat_mu/(hat_mu - 500)

M <- nlm(loglik.pareto, log(alpha0), y = altitude, stepmax = 10)

hist(altitude, freq = FALSE, ylim = c(0,0.004))
grid <- seq(500, 1500, length = 985)
lines(grid, dpareto(grid, location = 500, shape = exp(M$estimate)))

# Note that, in alternative to use "dpareto", you can make your own function...
my_dpareto <- function(x, location, shape){
  (x > location)*(shape*location^shape/x^(shape + 1))
}

x <- c(25, 30, 32) # just some numbers
cbind(dpareto(x, location = 20, shape = 1.5), my_dpareto(x, location = 20, shape = 1.5)) # just the same






