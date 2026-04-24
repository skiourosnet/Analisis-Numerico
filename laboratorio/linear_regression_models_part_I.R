###############################################################
# Numerical Analysis Lecture 09
# Least Squares: From Theory to Real Data
# Imelda Trejo Lorenzo
###############################################################

rm(list = ls())

###############################################################
# PART 1 — SIMPLE DATA (LECTURE EXAMPLE)
###############################################################

x <- 1:10
y <- c(1.3, 3.5, 4.2, 5.0, 7.0, 8.8, 10.2, 12.5, 13.0, 15.6)

###############################################################
# PART 2 — LINEAR MODEL (NORMAL EQUATIONS)
###############################################################

A <- cbind(1, x)
a_hat <- solve(t(A) %*% A, t(A) %*% y)

a_0 <- a_hat[1]
a_1 <- a_hat[2]

cat("Linear model (normal equations):\n")
cat("y ≈ a_0 + a_1 x\n")
cat("a_0 =", a_0, "\n")
cat("a_1 =", a_1, "\n")

###############################################################
# PART 3 — LINEAR MODEL USING lm()
###############################################################

model_lin <- lm(y ~ x)
coef_lin <- coef(model_lin)

cat("\nLinear model using lm():\n")
cat("a_0 =", coef_lin[1], "\n")
cat("a_1 =", coef_lin[2], "\n")

###############################################################
# PART 4 — QUADRATIC MODEL
###############################################################

A2 <- cbind(1, x, x^2)
a_hat2 <- solve(t(A2) %*% A2, t(A2) %*% y)

a_0_q <- a_hat2[1]
a_1_q <- a_hat2[2]
a_2_q <- a_hat2[3]

cat("\nQuadratic model:\n")
cat("y ≈ a_0 + a_1 x + a_2 x^2\n")
cat("a_0 =", a_0_q, "\n")
cat("a_1 =", a_1_q, "\n")
cat("a_2 =", a_2_q, "\n")

###############################################################
# PART 5 — EXPONENTIAL MODEL
###############################################################

Y <- log(y)
model_exp <- lm(Y ~ x)

c_0 <- coef(model_exp)[1]
c_1 <- coef(model_exp)[2]

alpha <- exp(c_0)
beta  <- c_1

cat("\nExponential model:\n")
cat("y ≈ α exp(β x)\n")
cat("α =", alpha, "\n")
cat("β =", beta, "\n")

###############################################################
# PART 6 — MODEL COMPARISON (SIMPLE DATA)
###############################################################

plot(x, y, pch = 19,
     main = "Model Comparison (Simple Data)",
     xlab = "x", ylab = "y")

# Linear
abline(a_0, a_1, col = "black", lwd = 2)

# Quadratic
curve(a_0_q + a_1_q*x + a_2_q*x^2,
      add = TRUE, col = "blue", lwd = 2)

# Exponential
curve(alpha * exp(beta * x),
      add = TRUE, col = "red", lwd = 2)

legend("topleft",
       legend = c("Linear", "Quadratic", "Exponential"),
       col = c("black", "blue", "red"),
       lwd = 2)

###############################################################
# PART 7 — GLOBAL TEMPERATURE DATA
###############################################################

library(ggplot2)

url <- "https://raw.githubusercontent.com/michael-franke/intro-data-analysis/master/data_sets/average-world-temperature.csv"
data_temperature <- read.csv(url)

x_temp <- data_temperature$year
y_temp <- data_temperature$avg_temp


###############################################################
# PLOT REAL DATA
###############################################################

ggplot(data_temperature, aes(x = year, y = avg_temp)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Global Temperature Trend",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 14)


###############################################################
# PART 8 — FIT MODELS (REAL DATA)
###############################################################

# Linear
model_lin_temp <- lm(y_temp ~ x_temp)
coef_lin <- coef(model_lin_temp)
a_0_lin <- coef_lin[1]
a_1_lin <- coef_lin[2]

# Quadratic
model_quad_temp <- lm(y_temp ~ x_temp + I(x_temp^2))
coef_quad <- coef(model_quad_temp)
a_0_quad <- coef_quad[1]
a_1_quad <- coef_quad[2]
a_2_quad <- coef_quad[3]

# Exponential
Y_temp <- log(y_temp)
model_exp_temp <- lm(Y_temp ~ x_temp)

c_0_temp <- coef(model_exp_temp)[1]
c_1_temp <- coef(model_exp_temp)[2]

alpha_temp <- exp(c_0_temp)
beta_temp  <- c_1_temp

cat("\n--- Global Temperature Models ---\n")

cat("\nLinear:\n")
cat("a_0 =", a_0_lin, "\n")
cat("a_1 =", a_1_lin, "\n")

cat("\nQuadratic:\n")
cat("a_0 =", a_0_quad, "\n")
cat("a_1 =", a_1_quad, "\n")
cat("a_2 =", a_2_quad, "\n")

cat("\nExponential:\n")
cat("α =", alpha_temp, "\n")
cat("β =", beta_temp, "\n")

###############################################################
# PART 9 — MODEL COMPARISON (REAL DATA)
###############################################################

plot(x_temp, y_temp,
     pch = 19,
     main = "Global Temperature: Model Comparison",
     xlab = "Year",
     ylab = "Temperature (°C)")

# Linear
abline(a_0_lin, a_1_lin, col = "black", lwd = 2)

# Quadratic
curve(a_0_quad + a_1_quad*x + a_2_quad*x^2,
      add = TRUE, col = "blue", lwd = 2)

# Exponential
curve(alpha_temp * exp(beta_temp * x),
      add = TRUE, col = "red", lwd = 2)

legend("topleft",
       legend = c("Linear", "Quadratic", "Exponential"),
       col = c("black", "blue", "red"),
       lwd = 2)

###############################################################
# PART 10 — BAYESIAN REGRESSION
###############################################################

library(rstanarm)
library(bayesplot)
library(posterior)

fit_bayes <- stan_glm(
  y_temp ~ x_temp,
  chains = 4,
  iter = 2000,
  seed = 123,
  refresh = 0
)

cat("\nBayesian model:\n")
print(fit_bayes)

###############################################################
# PART 11 — POSTERIOR SUMMARY
###############################################################

post_sum <- posterior_summary(
  fit_bayes,
  probs = c(0.025, 0.5, 0.975)
)

cat("\nPosterior summary:\n")
print(post_sum)

###############################################################
# PART 12 — POSTERIOR VISUALIZATION
###############################################################

mcmc_areas(
  as.matrix(fit_bayes),
  pars = "x_temp"
)

