################################################################################
#                                                                              #
#       W4451 - Applied Time Series Project With R, summer semester 2025       #
#                                                                              #
#                  Problem 1: Simulation from AR-Processes                     #
#                                                                              #
################################################################################


MatrNr <- 4027045          # Change here '1234567' to the matriculation number of one of your team members

#<------------------------ Provided simulation code -------------------------> #

# Fix this code; there are four technical errors.

set.seed(MatrNr)         # The seed is set to the given matriculation number (!first error MatrNr)
n <- 500                 # The number of observations to simulate
xt <- arima.sim(model = list(ar = 0.75), n = n, mean = 0.25)  #+ 1    # Call to simulation function 
#Technical errors:
# 1. Variable Name Case Mismatch
# 2. Incorrect Function Name for Simulation (arima_sim ---> arima.sim)
# 3. Misspelled Argument Name (modell ---> model)
# 4. Incorrect Process Mean Specification (+1 ---> mean=0.25) and we have to use list()


# <- Begin in the next line with your own code for the remaining tasks of Problem 1 -> #
library(forecast) 
library(ggplot2)


#<------------------------ 1 B). -------------------------> #
#Create a plot of the simulated time series 


#plot(xt, type = "l", main = "Simulated AR(1) Time Series", xlab = "Time", ylab = "x_t")
#abline(h=1, col="red", lty=2)

# I like better ggplot2 for the figures... So i will use a dataframe first to plot
df <- data.frame(Time = 1:length(xt), x_t = as.numeric(xt))

ggplot(df, aes(x = Time, y = x_t)) +
  geom_line() +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  labs(title = "Simulated AR(1) Time Series",
       x = "Time",
       y = expression(x[t])) +
  theme_gray() + ## (grey bg)
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

#<------------------------ 1 c). -------------------------> #
#create a graphic of the sample ACF of the simulated series.
##acf(xt, main = "Sample ACF of Simulated AR(1) Series")
## I like more this one ggplot2 style

ggAcf(xt) +
  ggtitle("ACF of Simulated AR(1) Series") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#<------------------------ 1 d). -------------------------> #

# Fit AR(1) model to the simulated series
fit <- ar.yw(xt, aic = FALSE, order.max = 1)
fit

mu_hat <- mean(xt)
mu_hat
#or:
#fit$x.mean

beta_hat <- fit$ar
beta_hat

#<------------------------ 1 E). -------------------------> #
# Innovation variance  (using model residual variance)
sigma2_e_hat <- fit$var.pred  
sigma2_e_hat


var_mean_hat <- (sigma2_e_hat / n) * (1 / (1 - beta_hat))^2
var_mean_hat


## OR  in the tutorial we use, this! 
# Adjustment for time series sample variance
# (here probably irrelevant due to the large number
# of observations)
#x_var_ts <- var(xt) * (n - 1) / n 
# Estimated innovation variance
#sig2_e <- x_var_ts * (1 - beta_hat^2)
#sig2_e
# Variance of sample mean in time series
#x_bar_var_ts <- (sig2_e / n) * (1 / (1 - beta_hat))^2 
#x_bar_var_ts

#<------------------------ 1 F). -------------------------> #

se_mean_hat <- sqrt(var_mean_hat)
z <- 1.96  # 95% normal quantile

lower <- mu_hat - z * se_mean_hat
upper <- mu_hat + z * se_mean_hat

c(lower, upper)



