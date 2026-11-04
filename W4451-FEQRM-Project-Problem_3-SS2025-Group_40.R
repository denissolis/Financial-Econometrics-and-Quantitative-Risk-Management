################################################################################
#                                                                              #
#           W4451 - Applied project using R, summer semester 2025              #
#                                                                              #
#          Problem 3: Estimation and Forecasting with GARCH Models             #
#                                                                              #
################################################################################

# Automatic adjustment of the working directory to the the directory of this script file

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
# Information:

# Use this file to save your code for solving Problem 3 of the applied project.

# <--------------- Begin in the next line with your own code  ---------------> #



library(ggpubr)
library(ggplot2)
library(fEGarch)
library(tidyr)
library(forecast)
### <---------------  PROBLEM 3a) Cocacola close-price using   library(yfR)---------------> #


# library(yfR)
# # Period from August 2020 to today (no crisis,  post-pandemic, almost stable)
# library(yfR)
# first_date <- as.Date("2020-08-01")
# last_date  <-  as.Date("2025-06-20") #Sys.Date()
# 
# # Daily prices
# data <- yf_get(tickers = "KO",
#                first_date = first_date,
#                last_date = last_date,
#                freq_data = "daily")
# # Save to CSV
# write.csv(data, "KO_logreturns.csv", row.names = FALSE)



### <---------------  PROBLEM 3B) Cocacola close-price using data from csv---------------> #

# Load data from CSV
data <- read.csv("KO_logreturns.csv")
#transform date 
data <- data[order(data$ref_date), ]
data$ref_date <- as.Date(data$ref_date)
# log-returns
data$log_ret <- c(NA, diff(log(data$price_close)))

#omitir valores null/na
data <- na.omit(data)


# how many points?
nrow(data)
max(data$ref_date )



#----------PLOT-----
# Define the two plots as before (p1 and p2):
p11 <- ggplot(data, aes(x = ref_date, y = price_close)) +
  geom_line() +
  theme_minimal() +
  theme_gray() + ## (grey bg)
  labs(
    title = "Coca-Cola (KO) Daily Closing Price",
    x = "Date",
    y = "Closing Price (USD)"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

p12 <- ggplot(data, aes(x = ref_date, y = log_ret)) +
  geom_line() +
  geom_hline(yintercept = mean(data$log_ret) , color = "red", linetype = "dashed") +
  theme_minimal() +
  theme_gray() + ## (grey bg)
  labs(
    title = "Coca-Cola (KO) Daily Log-Returns",
    x = "Date",
    y = "Log-Return (rt)"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))

# Arrange the plots vertically using ggpubr
ggarrange(p11, p12, 
          ncol = 1, 
          nrow = 2) 



### <---------------  PROBLEM 3C) Cocacola ACF of the returns and also of the squared returns---------------> #

# Plot the ACF of returns
acf_returns <- ggAcf(data$log_ret) +
  ggtitle("Sample ACF of Log-Returns (KO)") +
  ylim(-0.2, 0.2) +
   theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Plot the ACF of squared returns
acf_sq_returns <- ggAcf(data$log_ret^2) +
  ggtitle("Sample ACF of Squared Log-Returns (KO)") +
  ylim(-0.2, 0.2) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
# Show the plots in RStudio

plotB4.2d <- ggarrange(acf_returns, acf_sq_returns, 
                       ncol = 1, 
                       nrow = 2) 
plotB4.2d




### <---------------  PROBLEM 3D)      ---------------> #

#library(fEGarch)
# ?garch
GARCH11=garch(data$log_ret,order=c(1,1), cond_dist = "norm")
GARCH11
pars(GARCH11)

pars <- pars(GARCH11)
omega <- pars["omega"]
mu <- pars["mu"]
phi1 <- pars["phi1"]
beta1 <- pars["beta1"]




### <---------------  PROBLEM 3E)     ---------------> #

# "std" <<< t distribution
#  ?gjrgarch
GJR_GARCH11 = gjrgarch(data$log_ret,orders = c(1, 1), cond_dist = "norm")
GJR_GARCH11
pars(GJR_GARCH11)


pars_GJR_GARCH <- pars(GJR_GARCH11)
mu_GJR_GARCH <- pars_GJR_GARCH["mu"]
omega_GJR_GARCH <- pars_GJR_GARCH["omega"]
phi1_GJR_GARCH <- pars_GJR_GARCH["phi1"]
beta1_GJR_GARCH <- pars_GJR_GARCH["beta1"]
gama1_GJR_GARCH <- pars_GJR_GARCH["gamma1"]




### <---------------  PROBLEM 3F)     ---------------> #

# extract estimated conditional standard deviations
garch11_vol <-  sigt(GARCH11)       # Std dev from GARCH(1,1)
gjr_garch11_vol <- sigt(GJR_GARCH11)    # Std dev from GJR-GARCH(1,1)



n <- nrow(data)
# COnbine te data in a new df with all the data:
plot_df <- data.frame(
  Date = data$ref_date[seq_along(garch11_vol)],   
  LogReturn = data$log_ret[seq_along(garch11_vol)],
  GARCH11_SD = as.numeric(garch11_vol),
  GJRGARCH11_SD = as.numeric(gjr_garch11_vol)
)


# subplots:
p1 <- ggplot(plot_df, aes(x = Date, y = LogReturn)) +
  geom_line() +
  labs(title = "(i) Daily Log-Returns") +
  theme_minimal()+
  theme_gray()

p2 <- ggplot(plot_df, aes(x = Date, y = GARCH11_SD)) +
  geom_line(color="dodgerblue3") +

  labs(title = "(ii) GARCH(1,1) Volatility (Conditional Std Dev)") +
  ylab("Std Dev") +
  theme_minimal()+
  theme_gray() 


p3 <- ggplot(plot_df, aes(x = Date, y = GJRGARCH11_SD)) +
  geom_line(color="firebrick3") +
  theme_gray() + ## (grey bg)
  labs(title = "(iii) GJR-GARCH(1,1) Volatility (Conditional Std Dev)") +
  ylab("Std Dev") +
  theme_minimal()+
  theme_gray()  


# Three vertical subplots:
combined_plot <- ggarrange(
  p1, p2, p3,
  ncol = 1, nrow = 3,
  labels = c("(i)", "(ii)", "(iii)"),
  heights = c(1, 1, 1)
)

print(combined_plot)


ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = GARCH11_SD, color = "GARCH(1,1)")) +
  geom_line(aes(y = GJRGARCH11_SD, color = "GJR-GARCH(1,1)")) +
  labs(
    title = "GARCH vs GJR-GARCH Volatility Comparison",
    y = "Conditional Std Dev",
    color = "Model"
  ) +
  scale_color_manual(values = c("GARCH(1,1)" = "dodgerblue3", "GJR-GARCH(1,1)" = "firebrick3")) +
  theme_minimal() +
  theme_gray()+
  theme(legend.position = "bottom")




### <---------------  PROBLEM 3G)     ---------------> #

# 50-step (multi-step) forecasts from the end of sample
n_forecast <- 500
garch_pred <- predict(GARCH11, n.ahead = n_forecast)

# The fitted in-sample conditional stddev:
fitted_vol <- as.numeric(sigt(GARCH11))

# The out-of-sample (forecasted) stddev:
forecast_vol <- as.numeric(sigma(garch_pred))



#Prepare a combined time series for plotting
tail_length <- 200  # number of last fitted points to show
fitted_tail <- tail(fitted_vol, tail_length)
dates_tail <- tail(data$ref_date, tail_length)

# Create new dates for forecasts (could be arbitrary, just to the right of last date)
last_date <- max(dates_tail)
forecast_dates <- seq(last_date + 1, by = 1, length.out = n_forecast) # not actual trading dates, but for plotting


plot_df_F <- data.frame(
  Date = c(dates_tail, forecast_dates),
  Volatility = c(fitted_tail, forecast_vol),
  Type = c(rep("Fitted", tail_length), rep("Forecast", n_forecast))
)


# Plot
ggplot(plot_df_F, aes(x = Date, y = Volatility, color = Type)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Fitted" = "black", "Forecast" = "dodgerblue3")) +
  labs(
    title = "Last 200 Fitted and 50 Forecasted GARCH(1,1) Conditional Std Devs",
    x = "Time",
    y = "Conditional Std Dev"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+  theme_gray()  

theorical_limit = sqrt( omega/ (1-phi1-beta1))
theorical_limit