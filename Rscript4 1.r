# Load the data and the "survival" library. Then continue.

library(survival)
setwd("C:/Users/a020554/OneDrive - University of Pisa/Desktop/didattica/Topics in microeconometrics/000 data_and_rscript_for_labs")
d <- read.csv("d4.csv", sep = ";", dec = ",") 
attach(d)


#################################################################################################

# You have a sample of unemployed individuals. 
# The response variable of interest is unemployment duration, expressed in months.
# Predictors: sex, age, education (low/medium/high).

# Below, you find 3 possible experiments, A, B, C.

#################################################################################################
#################################################################################################
#################################################################################################

# Experiment A. 
 # Using register data, you collect information on all people that became unemployed in Italy
 # during the month of march 2020. The unemployment duration is reported in the variable "timeA".
 # There is an indicator "deltaA" of the event. Those who are still unemployed at the end 
 # of follow-up have deltaA = 0.

# A1. What type of survival data are you dealing with?
  # ANS: right-censored data.

# A2. What is the proportion of censored observations?
mean(deltaA == 0) # or mean(1 - deltaA)

# A3. Compute the mean and median unemployment duration. Create a histogram. 
  # Create a boxplot, separately in males and females. Briefly comment.
mean(timeA) # WRONG, must be an under-estimate
median(timeA) # maybe right, depends on WHERE censoring occurs
hist(timeA) # WRONG
boxplot(timeA ~ sex) # maybe, same arguments as for the median

# Regarding the median: how do you calculate it here?
  # X = {5, 8, 12, >15, > 28}
  # X = {> 2, 4, 7, > 9, 50}
# In the first case, you get 12. In the second, you don't know.
# A necessary condition to evaluate the median is that less than 50% of the data are censored.
# However, it also depends on WHERE censoring occurs.

# A4. Create a Kaplan-Meier curve of the unemployment duration. 
 # Ok, so what is the median duration? Try with summary(.).
km1 <- survfit(Surv(timeA, deltaA) ~ 1)
plot(km1, ylab = "Estimated survival", xlab = "time (months)")
summary(km1) # median survival = about 1.21

# A5. Create a Kaplan-Meier curve of the unemployment duration, separately in males and females. 
  # Compare groups using a suitable test.
km2 <- survfit(Surv(timeA, deltaA) ~ sex)
plot(km2, ylab = "Estimated survival", xlab = "time (months)", col = c("red", "blue"))
# Males look a bit faster than females at "dying" (i.e., finding a job)
survdiff(Surv(timeA, deltaA) ~ sex) # log-rank test. No significant differences (p = 0.2)


# A6. Write a "loglikA" function to fit an exponential model including all available predictors.
  # Use start = c(4.5,-0.1,0,-0.7,0) for X = cbind(1,age,sex == "m", edu == "low", edu == "medium").

loglikA <- function(beta, y, delta, X){
  lambda <- exp(X%*%beta)
  -sum(delta*dexp(y, lambda, log = TRUE) + (1 - delta)*pexp(y, lambda, log = TRUE, lower.tail = FALSE))
}

X <- cbind(1, age, sex == "m", edu == "low", edu == "medium")
start <- c(4.5,-0.1,0,-0.7,0)
A1 <- nlm(loglikA, start, y = timeA, delta = deltaA, X = X)
A1$estimate
exp(A1$estimate) # HR
# For example, males have 6% more "risk" than females to find a job,
 # because the HR associated with male gender is 1.06.
# This is not easy to interpret: it is not a probability, it is
  # the LIMIT of a conditional probability as the time interval tends to 0.

# A7. Fit a Cox model, compare results.
A2 <- coxph(Surv(timeA, deltaA) ~ age + sex + edu)
summary(A2)
# almost identical coefficients

#################################################################################################
#################################################################################################
#################################################################################################

# Experiment B. 
 # Using register data, you collect information on all people that became unemployed in Italy
 # during the month of march 2020. The unemployment duration is not known precisely,
 # because updates only come every now and then. For example, you don't know when exactly Albert
 # found a job: you only know that he was unemployed at time timeB.L = 6.3 months, and that he was no longer unemployed
 # at time timeB.R = 7.4 months. Some individuals have an exact time, and have timeB.L = timeB.R.
 # Some other individuals were still unemployed at the end of follow-up, and have timeB.R = Inf.

# B1. What type of survival data are you dealing with?

# ANS: interval-censored data.

# B2. What is the proportion of censored observations? Separate by type of censoring.
mean(timeB.R == Inf) # right-censored
mean(timeB.L == 0) # left-censored
mean(timeB.L == timeB.R) # not censored
# Everything else: interval-censored

# B3. Verify that timeB.L <= timeB.R.
all(timeB.L <= timeB.R)

# B4. Write a "loglikB" function to fit an exponential model including all available predictors.
loglikB <- function(beta, L, R, X){
  lambda <- exp(X%*%beta)
  FL <- pexp(L, lambda)
  FR <- pexp(R, lambda)
  -sum(log(FR - FL))
}

X <- cbind(1, age, sex == "m", edu == "low", edu == "medium")
start <- c(4.5,-0.1,0,-0.7,0)
B1 <- nlm(loglikB, start, L = timeB.L - 0.00001, R = timeB.R + 0.00001, X = X)
B1$estimate
exp(B1$estimate) # HR

# I used a trick: by adding/subtracting 0.00001 I removed exact observations,
  # and treat them all as interval-censored.

# B5. Apply the same method to (timeA, deltaA), compare results with question A6. 
  # Hint: left = timeA, right = timeA/deltaA. Why does this work?
B2 <- nlm(loglikB, start, L = timeA - 0.000001, R = (timeA + 0.000001)/deltaA, X = X)
# This works because right censoring is just a special case of interval censoring:
  # for example, y = 5 and delta = 0 is equivalent to (L = 5, R = Inf).

# B6. Note that no simple nonparametric or semiparametric method works in this case.
  # This is why you were not requested to use Kaplan-Meier or Cox.

#################################################################################################
#################################################################################################
#################################################################################################

# Experiment C.
  # You enrol a sample of currently unemployed people. You have the following variables:
   # enrolC = (time at enrolment - time0), i.e., for how long someone has been unemployed so far.
   # timeC = (time at event - time at enrolment), i.e., the time between the enrolment and the moment someone finds a new job. 
  # There is an indicator deltaC of the event. Those who are still unemployed at the end of follow-up have deltaC = 0.

# C1. What type of survival data are you dealing with?
# ANS: right-censored, left-truncated

# C2. What is the proportion of censored observations? What about truncation?
mean(deltaC == 0)
# I don't know how many people are left-truncated!

# C3. Write a "loglikC" function to fit an exponential model including all available predictors.
  # Be careful! Unemployment time = enrolC + timeC!!!

loglikC <- function(beta, z, y, delta, X){
  lambda <- exp(X%*%beta)
  -sum(
      delta*dexp(y, lambda, log = TRUE) + 
      (1 - delta)*pexp(y, lambda, log = TRUE, lower.tail = FALSE)
      - pexp(z, lambda, log = TRUE, lower.tail = FALSE)
  )
}

X <- cbind(1, age, sex == "m", edu == "low", edu == "medium")
start <- c(4.5,-0.1,0,-0.7,0)
C1 <- nlm(loglikC, start, z = enrolC, y = enrolC + timeC, delta = deltaC, X = X)
C1$estimate
exp(C1$estimate) # HR


# C4. Fit a Cox model, compare results.
C2 <- coxph(Surv(enrolC, enrolC + timeC, deltaC) ~ age + sex + edu)
summary(C2)


# C5. Verify that maximizing loglikA(..., timeA, deltaA) 
  # is the same as maximizing loglikC(..., timeA, deltaA, z = 0). Do you see why?

test1 <- nlm(loglikA, start, y = timeA, delta = deltaA, X = X)
test2 <- nlm(loglikC, start, z = 0, y = timeA, delta = deltaA, X = X)
cbind(test1$estimate, test2$estimate)




