# ---- libreries ----
library(ggplot2)
# ---- simulation -----
b1 <- 20 # known intercept
b2 <- 0.6 # known slope
n <- 25 # sample size

set.seed(1234)
x <- rnorm(n, 100, 25) # values of x
var_e <- runif(1, 2, 20) # error variance
e <- rnorm(n, 0, var_e) # error term
y <- b1 + b2*x + e # linear model

df <- data.frame(x, y)
ggplot(df, aes(x,y)) +
  geom_point() +
  geom_smooth(method = "lm")

# ---- OLS estimation ----
model <- lm(y ~ x)
b1_0 <- model$coefficients[1]
b2_0 <- model$coefficients[2]

# ---- simulation (100 reps) ----
b1_vector <- b1_0
b2_vector <- b2_0
for (seed in seq(1:99)) {
  set.seed(seed)
  x <- rnorm(n, 100, 25) # values of x
  var_e <- runif(1, 2, 20) # error variance
  e <- rnorm(n, 0, var_e) # error term
  y <- b1 + b2*x + e # linear model
  model <- lm(y ~ x) # regression
  b1_hat <- model$coefficients[1] # estimated intercept
  b2_hat <- model$coefficients[2] # estimated slope
  b1_vector <- c(b1_vector, b1_hat)
  b2_vector <- c(b2_vector, b2_hat)
}

df_beta <- data.frame(b1_vector, b2_vector)

mean(b1_vector)
ggplot(df_beta, aes(b1_vector)) +
  geom_histogram(binwidth = 1, aes(y=..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_beta$b1_vector), 
                                         sd = sd(df_beta$b1_vector)))


mean(b2_vector)
ggplot(df_beta, aes(b2_vector)) +
  geom_histogram(binwidth = 0.01, aes(y=..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_beta$b2_vector), 
                                         sd = sd(df_beta$b2_vector)))

# ---- simulation (2000 reps) ----
b1_vector <- b1_0
b2_vector <- b2_0
for (seed in seq(1:1999)) {
  set.seed(seed)
  x <- rnorm(n, 100, 25) # values of x
  var_e <- runif(1, 2, 20) # error variance
  e <- rnorm(n, 0, var_e) # error term
  y <- b1 + b2*x + e # linear model
  model <- lm(y ~ x) # regression
  b1_hat <- model$coefficients[1] # estimated intercept
  b2_hat <- model$coefficients[2] # estimated slope
  b1_vector <- c(b1_vector, b1_hat)
  b2_vector <- c(b2_vector, b2_hat)
}

df_beta <- data.frame(b1_vector, b2_vector)

mean(b1_vector)
ggplot(df_beta, aes(b1_vector)) +
  geom_histogram(binwidth = 1, aes(y=..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_beta$b1_vector), 
                                         sd = sd(df_beta$b1_vector)))


mean(b2_vector)
ggplot(df_beta, aes(b2_vector)) +
  geom_histogram(binwidth = 0.01, aes(y=..density..)) +
  stat_function(fun = dnorm, args = list(mean = mean(df_beta$b2_vector), 
                                         sd = sd(df_beta$b2_vector)))
