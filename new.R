rm(list=ls())

# Set working directory
setwd("/Users/andrewclark/Documents/FM442 Quantitative Methods in Finance and Risk Analysis/Presentation")

# Import libraries 
library(reshape2)
library(lubridate)
library(rugarch)
library(matrixStats)
library(dplyr)
library(data.table)
library(car)
library(ggplot2)

# Function to process data
process_data <- function(file_path) {
  # Read data
  data <- fread(file_path)
  
  # Adjust for CFA factor
  data$Unadjusted_PRC <- data$PRC
  data$Adjusted_PRC <- data$PRC / data$CFACPR
  
  # Select individual stocks
  MSFT <- data[data$PERMNO == 10107, c("date", "Adjusted_PRC")]
  names(MSFT)[2] <- "MSFT"
  
  AAPL <- data[data$PERMNO == 14593, c("date", "Adjusted_PRC")]
  names(AAPL)[2] <- "AAPL"
  
  UAL <- data[data$PERMNO == 91103, c("date", "Adjusted_PRC")]
  names(UAL)[2] <- "UAL"
  
  # Merge into a single table called PRC
  PRC <- merge(AAPL, MSFT)
  PRC <- merge(PRC, AAPL)
  PRC <- merge(PRC, UAL)
  
  # Reshape using data.table
  PRC <- dcast(as.data.table(data), date ~ PERMNO, value.var = "Adjusted_PRC", fun.aggregate = sum)
  names(PRC) <- c("date", "AAPL", "MSFT", "UAL")
  
  RET <- dcast(as.data.table(data), date ~ PERMNO, value.var = "RET", fun.aggregate = sum)
  names(RET) <- c("date", "AAPL", "MSFT", "UAL")
  
  Y <- log(1 + RET[,1:4])
  #Alternatively, if we only had prices
  #Y<-diff(log(PRC[,2:7]))
  
  
  #Now let us change the class of "date"
  Y$date <- RET$date
  head(Y)
  class(Y$date)
  
  
  # Save the original int type date to int_date
  Y$int_date <- Y$date
  # Use the function ymd() to transform the column into Dates
  Y$date <- ymd(Y$date)
  # Check if it worked
  class(Y$date)
  
  
  # Lets do the same for PRC
  PRC$int_date <- PRC$date
  PRC$date <- ymd(PRC$date)
  
  
  # Saving the data frame of returns
  save(Y, file = "Y.RData")
  
  # Saving the data frame of prices
  save(PRC, file = "PRC.RData")
  
  # Return the processed data
  return(list(Y = Y, PRC = PRC, UAL = UAL))
}

processed_data <- process_data("Data.csv")

############################# Implementing HS to estimate VaR and ES ##################################

# Load the data
load("Y.RData")

# Extract the returns for AAPL, MSFT and UAL
aaplReturns <- Y$AAPL
msftReturns <- Y$MSFT
ualReturns <- Y$UAL

plot_time_series <- function(processed_data) {
  ggplot(processed_data$UAL, aes(x = as.Date(as.character(date), format="%Y%m%d"), y = UAL)) +
    geom_line(color = "cornflowerblue") +
    labs(title = "UAL Historical Time Series",
         x = "Date",
         y = "Adjusted Close Price") +
    theme_minimal() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "5 year") +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
}

plot_time_series(processed_data)



# Function to plot returns
plot_returns <- function(date, returns, asset_name) {
  plot(date, returns, type = "l", col = "cornflowerblue",
       main = paste("Returns for", asset_name), xlab = "Date", ylab = "Returns")
}

# Example usage
plot_returns(processed_data$Y$date, aaplReturns, "AAPL")


# Plot returns 
plot_returns(Y$date, aaplReturns, "AAPL")
plot_returns(Y$date, msftReturns, "MSFT")
plot_returns(Y$date, ualReturns, "UAL")


############## HS VaR and Plot ##############


# Function to calculate and plot Historical simulation VaR 
calculate_hs_var_and_plot <- function(returns, portfolio_value = 1000, probability = 0.05) {
  # Sort returns
  sorted_returns <- sort(returns)
  
  # Number of observations
  n <- length(returns)
  
  # Calculate the quantile
  M <- ceiling(n * probability)
  quantile_value <- sorted_returns[M]
  
  # Visualize the sorted returns and the quantile
  plot(sorted_returns, type = "l", main = "UAL Sorted Returns", xlab = "Observations", ylab = "Returns")
  segments(x0 = M, y0 = min(sorted_returns) - 0.5, y1 = quantile_value, lty = "dashed", lwd = 2, col = "red")
  segments(x0 = -1000, y0 = quantile_value, x1 = M, lty = "dashed", lwd = 2, col = "red")
  axis(1, at = c(M, 2000, 4000, 6000, 8000), label = c("M", "2000", "4000", "6000", "8000"))
  axis(2, at = quantile_value, label = round(quantile_value, 3), las = 1)
  
  # Calculate the actual VaR
  actual_var <- -quantile_value * portfolio_value
  
  # Print and return the results
  cat("HS VaR:", actual_var, "\n")
  
  return(actual_var)
}

# Save historical VaR data 
aapl_hs_VaR <- calculate_hs_var_and_plot(aaplReturns)
save(aapl_hs_VaR, file = "APPLVaR.RData")

msft_hs_VaR <- calculate_hs_var_and_plot(msftReturns)
save(msft_hs_VaR, file = "MSFTVaR.RData")

ual_hs_VaR <- calculate_hs_var_and_plot(ualReturns)
save(ual_hs_VaR, file = "UALVaR.RData")

# Function to calculate expected shortfall
calculate_expected_shortfall <- function(returns, portfolio_value = 1000, probability = 0.05) {
  
  # Sort returns
  sorted_returns <- sort(returns)
  # Number of observations
  n <- length(sorted_returns)
  
  # Calculate the quantile
  M <- ceiling(n * probability)
  
  # Calculate ES
  es <- -mean(sorted_returns[1:M]) * portfolio_value
  
  return(es)
}

aapl_ES <- calculate_expected_shortfall(aaplReturns)
msft_ES <- calculate_expected_shortfall(msftReturns)
ual_ES <- calculate_expected_shortfall(ualReturns)

# Print Expected Shortfall:
cat("Expected Shortfall for AAPL:", aapl_ES, "\n")
cat("Expected Shortfall for MSFT:", msft_ES, "\n")
cat("Expected Shortfall for UAL:", ual_ES, "\n")


#Different Estimation Windows---------------------------------
# Historical simulation with fixed estimation window
historical_simulation_with_window <- function(returns, estimation_window = 1000, probability = 0.05, portfolio_value = 1000) {
  n <- length(returns)
  
  # Initialize an empty vector to hold VaR forecasts
  VaR_vector <- vector(length = (n - estimation_window))
  
  # Loop to calculate one VaR per day
  for (i in 1:(n - estimation_window)) {
    sorted_returns <- sort(returns[i:(i + estimation_window)])
    quant <- ceiling(probability * length(sorted_returns))  # Cutoff for the 100p% worse return
    VaR_vector[i] <- sorted_returns[quant] * portfolio_value
  }
  
  # Plot the VaR forecasts
  dates_var <- Y$date[(estimation_window + 1):length(Y$date)]
  plot(dates_var, VaR_vector, type = "l", main = "UAL VaR HS with Estimation Window",
       col = "red", las = 1, ylab = "USD", xlab = "Date")
  
  return(VaR_vector)
}

# Fixed estimation window plots 
aapl_VaR_vector <- historical_simulation_with_window(aaplReturns)
msft_VaR_vector <- historical_simulation_with_window(msftReturns)
ual_VaR_vector <- historical_simulation_with_window(ualReturns)

# Function to plot HS with multiple estimation windows
historical_simulation_with_multiple_windows <- function(returns, windows = c(100, 500, 1000, 2000), probability = 0.05, portfolio_value = 1000) {
  n <- length(returns)
  
  # Create an empty data frame to fill with the forecasts
  HS <- data.frame(matrix(numeric(), nrow = n, ncol = length(windows), dimnames = list(NULL, paste0("HS", windows))))
  
  # Do a loop for every element of windows
  for (window in windows) {
    
    # Perform a daily HS VaR
    for (i in 1:(n - window)) {
      
      ys <- sort(returns[i:(i + window)])
      
      # Get the quantile
      quant <- ceiling(probability * length(ys))
      
      # Allocate the result to the corresponding column of HS
      # Use which() to find the index of the window and allocate to that column
      column <- which(windows == window)
      HS[i + window, column] <- -ys[quant] * portfolio_value
    }
  }
  
  # Plotting VaR forecasts
  plot_dates <- Y$date[(max(windows) + 1):n]
  plot(HS$HS100, main = "UAL HS with different estimation windows", ylab = "VaR in USD",
       xlab = "Date", type = "l", col = "red", lwd = 2)
  lines(HS$HS500, col = "blue", lwd = 2)
  lines(HS$HS1000, col = "green", lwd = 2)
  lines(HS$HS2000, col = "black", lwd = 2)
  
  legend("topleft", legend = names(HS), lty = 1, col = c("red", "blue", "green", "black"))
  
  # Subset HS to keep only the observations that fit the most restrictive estimation window
  # This is now the window = 5000
  HS_subset <- HS[(max(windows) + 1):n, ]
  
  # Plotting them all together
  plot(plot_dates, HS_subset$HS100, main = "UAL HS with different estimation windows", ylab = "VaR in USD",
       xlab = "Date", ylim = c(10, 150), type = "l", col = "red", lwd = 2)
  lines(plot_dates, HS_subset$HS500, col = "blue", lwd = 2)
  lines(plot_dates, HS_subset$HS1000, col = "green", lwd = 2)
  lines(plot_dates, HS_subset$HS2000, col = "black", lwd = 2)
  
  # cex controls the size of the legend
  legend("topleft", legend = names(HS_subset), cex = 0.8, lty = 1, col = c("red", "blue", "green", "black"))
  
  return(HS)
}

# Plot multiple window estimations
aapl_HS <- historical_simulation_with_multiple_windows(aaplReturns)
msft_HS <- historical_simulation_with_multiple_windows(msftReturns)
ual_HS <- historical_simulation_with_multiple_windows(ualReturns)

aaplVaR <- as.data.frame(aapl_HS)
save(aaplVaR, file = "aaplVaR.RData")

msftVaR <- as.data.frame(msft_HS)
save(msftVaR, file = "msftVaR.RData")

ualVaR <- as.data.frame(ual_HS)
save(ualVaR, file = "ualVaR.RData")

compute_means_and_sds <- function(HS, windows = c(100, 500, 1000, 2000)) {
  HS <- HS[(max(windows) + 1):nrow(HS), ]
  
  # Compute means
  means <- colMeans(HS)
  
  # Compute standard deviations
  sds <- sapply(HS, sd)
  
  return(list(means = means, sds = sds))
}

# Means and standard deviations
aapl_means_and_sds <- compute_means_and_sds(aapl_HS)
msft_means_and_sds <- compute_means_and_sds(msft_HS)
ual_means_and_sds <- compute_means_and_sds(ual_HS)
aapl_means_and_sds
msft_means_and_sds
ual_means_and_sds






###################### Garch VaR #########################

# Load files from the previous seminar
load("ualVaR.RData")
VaRold<-ualVaR

# Load returns
load("Y.RData")

#We pick one asset, say Apple
y <- Y$UAL
dates <- Y$date

# Parameters
portfolio_value = 1000
p = 0.05 #probability for VaR





#Compute GARCH VaR
#Use data from day 1 to day 1000 to fit the GARCH model
#Find optimal coefficients
#Find conditional vol at t+1
#Compute VaR based on the conditional vol



# We will create a function, with inputs:
# GARCH spec, here the default
# Probability, here 0.05
# Portfolio value, here 1000
# Estimation window, here 1000
# Do a loop for the forecast of conditional vol
#NOTE: THIS WILL TAKE A LOT OF TIME, whenever we call the function


# Function that creates a GARCH forecast

DoGARCH <- function(y, spec, probability = 0.05, portfolio_value = 1, WE = 1000){
  # GARCH function that takes as argument:
  # y: A vector of returns, ordered by date
  # spec: The ugarchspec object with the GARCH specification
  # probability: The probability to be used for VaR - Default 5%
  # portfolio_value: The portfolio value - Default 1
  # WE: Estimation window for the forecast - Default 1000 days
  
  # To calculate elapsed time, first get the current time
  old <- Sys.time()
  
  # Print message
  cat("Doing GARCH VaR forecast", "\n",
      "Estimation window:", WE, "\n",
      "Number of observations:", length(y), "\n",
      "VaR probability:", probability, "\n",
      "Portfolio value:", portfolio_value)
  
  # Number of observations
  n <- length(y)
  
  # Initialize empty VaR vector
  VaR <- rep(NA, n)
  
  # Do a loop for the forecast
  for (i in 1:(n-WE)){
    
    # Subset the dataset to the estimation window
    window <- y[i:(i+WE-1)]
    
    # Fit the GARCH
    res <- ugarchfit(spec = spec, data = window, solver = "hybrid")
    
    # Save coefficients
    omega <- coef(res)['omega']
    alpha <- coef(res)['alpha1']
    beta <- coef(res)['beta1']
    
    # Estimate sigma2 using the last observation of window
    sigma2 <- omega + alpha*tail(window,1)^2 + beta*tail(res@fit$var,1)
    
    # Allocate the VaR forecast in the vector
    VaR[i+WE] <- -sqrt(sigma2) * qnorm(probability) * portfolio_value
  }
  
  # Get the new time and print the elapsed time
  time <- difftime(Sys.time(), old, units = "secs")
  cat("\n", "Elapsed time:", round(time,4), "seconds")
  
  # Return the VaR vector
  return(VaR)
}

DoTGARCH <- function(y, spec, probability = 0.05, portfolio_value = 1, WE = 1000){
  # GARCH function that takes as argument:
  # y: A vector of returns, ordered by date
  # spec: The ugarchspec object with the GARCH specification
  # probability: The probability to be used for VaR - Default 5%
  # portfolio_value: The portfolio value - Default 1
  # WE: Estimation window for the forecast - Default 1000 days
  
  # To calculate elapsed time, first get the current time
  old <- Sys.time()
  
  # Print message
  cat("Doing GARCH VaR forecast", "\n",
      "Estimation window:", WE, "\n",
      "Number of observations:", length(y), "\n",
      "VaR probability:", probability, "\n",
      "Portfolio value:", portfolio_value)
  
  # Number of observations
  n <- length(y)
  
  # Initialize empty VaR vector
  VaR <- rep(NA, n)
  
  # Do a loop for the forecast
  for (i in 1:(n-WE)){
    
    # Subset the dataset to the estimation window
    window <- y[i:(i+WE-1)]
    
    # Fit the GARCH
    res <- ugarchfit(spec = spec, data = window, solver = "hybrid")
    
    # Save coefficients
    omega <- coef(res)[1]
    alpha <- coef(res)[2]
    beta <- coef(res)[3]
    nu <- coef(res)[4]
    
    # Estimate sigma2 using the last observation of window
    sigma2 <- omega + alpha*tail(window,1)^2 + beta*tail(res@fit$var,1)
    
    # Allocate the VaR forecast in the vector
    VaR[i+WE] <- -sqrt(sigma2) * portfolio_value * qt(probability, nu) /sqrt(nu/(nu-2))
  }
  
  # Get the new time and print the elapsed time
  time <- difftime(Sys.time(), old, units = "secs")
  cat("\n", "Elapsed time:", round(time,4), "seconds")
  
  # Return the VaR vector
  return(VaR)
}



# Normal garch (1,1)
normalspec <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0))
)

# Student t garch (1,1)
studenttspec <- ugarchspec(
  variance.model = list(garchOrder = c(1, 1)),
  distribution.model = 'std',
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
)


# GARCH VaR for 300 days
GARCH300 <- DoGARCH(y, spec = normalspec, probability = 0.05, portfolio_value = 1000, WE = 300)



#Let's save it so that we don't have to wait every time: 
save(GARCH300, file = "GARCH300.RData")

#--------------------------------------------------------


# GARCH VaR for different window (2000 days)
GARCH2000 <- DoGARCH(y, spec = normalspec, probability = 0.05, portfolio_value = 1000, WE = 2000)


#Let's save it again
save(GARCH2000, file = "GARCH2000.RData")

#--------------------------------------------------------

# Student t GARCH VaR for 300 days
stdGARCH300 <- DoTGARCH(y, spec = studenttspec, probability = 0.05, portfolio_value = 1000, WE = 300)


#Let's save it so that we don't have to wait every time: 
save(stdGARCH300, file = "stdGARCH300.RData")

#--------------------------------------------------------


# Student t GARCH VaR for different window (2000 days)
stdGARCH2000 <- DoTGARCH(y, spec = studenttspec, probability = 0.05, portfolio_value = 1000, WE = 2000)


#Let's save it again
save(stdGARCH2000, file = "stdGARCH2000.RData")

#--------------------------------------------------------


#To save time, let's simply load them:
#(assuming you have already saved them) 
load("GARCH300.RData")
load("GARCH2000.RData")
load("stdGARCH300.RData")
load("stdGARCH2000.RData")


# Combining all VaR forecasts------------------------
#Now let's extend it by adding GARCH VaRs
VaR <- cbind(ualVaR, GARCH300, GARCH2000, stdGARCH300, stdGARCH2000)

JUST_GARCH <- cbind(GARCH300, stdGARCH300, GARCH2000, stdGARCH2000)

GARCH300_VaR<- cbind(GARCH300, stdGARCH300)

GARCH300_VaR_and_HS_VaR <- cbind(ualVaR, GARCH300, stdGARCH300)

GARCH2000_VaR_and_HS_VaR <- cbind(ualVaR, GARCH2000, stdGARCH2000)

#Comparisons---------------------

# Means for each forecast
#na.rm=TRUE removes all NA (since columns have different "length")
round(colMeans(VaR, na.rm = TRUE),3)

# Standard deviations 
round(colSds(as.matrix(VaR),na.rm=TRUE))


######################## HS and Garch VaR Plots ########################

# Plot all
matplot(dates, VaR, type = "l", lty = 1, col = 1:6, xaxt = "n", main = "All models VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

# Legend
legend("top", legend = colnames(VaR),cex=0.6, lty = 1, col = 1:6)

matplot(dates, GARCH300_VaR, type = "l", lty = 1, col = 1:6, xaxt = "n", main = "Garch 300 VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

# Legend
legend("top", legend = colnames(GARCH300_VaR),cex=0.6, lty = 1, col = 1:6)

matplot(dates, GARCH300_VaR_and_HS_VaR, type = "l", lty = 1, col = 1:6, xaxt = "n", main = "Garch 300 and HS VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

## Legend
legend("top", legend = colnames(GARCH300_VaR_and_HS_VaR),cex=0.6, lty = 1, col = 1:6)

matplot(dates, JUST_GARCH, type = "l", lty = 1, col = 1:6, xaxt = "n", main = "Garch only VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(min(dates), max(dates), by = "years"))

## Legend
legend("top", legend = colnames(JUST_GARCH),cex=0.6, lty = 1, col = 1:6)




# Find maximum estimation window to make everything comparable
#Number of NA's corresponds to window length
windows <- colSums(is.na(VaR))
windows

Garch300_windows <- colSums(is.na(GARCH300_VaR_and_HS_VaR))
Garch300_windows
#restrict to largest estimation window
start <- max(Garch300_windows) + 1
end <- length(dates)

Garch2000_windows <- colSums(is.na(GARCH2000_VaR_and_HS_VaR))
Garch2000_windows
#restrict to largest estimation window
start <- max(Garch2000_windows) + 1
end <- length(dates)

just_garch_windows <- colSums(is.na(JUST_GARCH))
just_garch_windows

# Plot all
matplot(dates[start:end], GARCH300_VaR_and_HS_VaR[start:end,], type = "l", lty = 1, col = 1:6, xaxt = "n",
        main = "Largest estimation window Garch 300 and HS VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(dates[max(Garch300_windows)], max(dates), by = "years"))

# Legend
legend("topright", legend = colnames(GARCH300_VaR_and_HS_VaR),cex=0.6, lty = 1, col = 1:6)

matplot(dates[start:end], GARCH2000_VaR_and_HS_VaR[start:end,], type = "l", lty = 1, col = 1:6, xaxt = "n",
        main = "Largest estimation window Garch 2000 and HS VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(dates[max(Garch2000_windows)], max(dates), by = "years"))

# Legend
legend("topright", legend = colnames(GARCH2000_VaR_and_HS_VaR),cex=0.6, lty = 1, col = 1:6)

matplot(dates[start:end], JUST_GARCH[start:end,], type = "l", lty = 1, col = 1:6, xaxt = "n",
        main = "Largest estimation window Garch only VaR forecasts", xlab = "Date", ylab = "VaR USD")
axis.Date(1, at = seq(dates[max(just_garch_windows)], max(dates), by = "years"))

# Legend
legend("topright", legend = colnames(JUST_GARCH),cex=0.6, lty = 1, col = 1:6)



#BACKTESTING---------------------------------------


#Testing how often VaR is violated in different models
#Violation is when return is smaller than -VaR(p)
#We know this should happen approximately 100p% of times
#Happening more often means we are underforecasting risk
#Less often means we are overforecasting risk

#Violation ratio=#violations/#Expected_violations


# Let's transform VaR to a data.frame
VaR <- as.data.frame(VaR)

# Initialize a Violations data.frame, same dim and colnames as VaR, fill with NA
Violations <- VaR
Violations[] <- NA

# For every model (columns in VaR) restricted to largest estimation window
for(i in 1:dim(VaR)[2]){
  
  # Fill the column in Violations with TRUE/FALSE
  # TRUE if the realized return is lower than VaR
  # FALSE otherwise
  Violations[,i] <- (y*portfolio_value < -VaR[,i])
}


# Restrict to largest estimation window
Violations[1:(start-1),] <- NA

head(Violations) #We have just set first observations to NA
tail(Violations)

# Find where violations happened in GARCH300_VaR model
dates[which(Violations$GARCH300)]
dates[which(Violations$stdGARCH300)]
dates[which(Violations$GARCH2000)]
dates[which(Violations$stdGARCH2000)]


# Get a random day where VaR is violated using sample()
# sample() returns specified size of elements from input
random_day <- sample(dates[which(Violations$GARCH300)],1)

# Find the index in dates using which()
day_index <- which(dates == random_day)

# See that row in Violations
paste0("Violation for HS2000 on ",random_day)

Violations[day_index,] #checks in which other models there was a violation


# Plotting the violations
plot(dates[start:end], VaR$GARCH300[start:end], xlab = "Dates", ylab = "GARCH 300 VaR", 
     type = "l", col = "cornflowerblue", main = "Garch 300 VaR with violations")

# Add points where the violations happened
#cex can again control the size of the points
points(dates[Violations$GARCH300], VaR$GARCH300[Violations$GARCH300], cex=0.5,pch = 16, col = "red")

# Plotting the violations
plot(dates[start:end], VaR$stdGARCH300[start:end],xlab="Dates", ylab="stdGARCH 300 VaR", type = "l", col = "cornflowerblue",main = "t-Garch 300 VaR with violations")

# Add points where the violations happened
#cex can again control the size of the points
points(dates[Violations$stdGARCH300], VaR$stdGARCH300[Violations$stdGARCH300], cex=0.5,pch = 16, col = "red")


# Check dates where all models have a violation
w <- apply(Violations, 1, all)

# na.rm =TRUE means we will firstly remove all NA elements
sum(w, na.rm = TRUE)


# Plotting the returns and adding the days where all models had a violation
plot(dates, y, main = "UAL returns with violations", type = "l",col = "cornflowerblue", lwd = 2, las = 1,
     xlab = "Date", ylab = "Returns")
points(dates[w], y[w], cex=0.5,pch = 16, col = "red")


#Compare-----------------------------------------------------
# Counting Violations by model
colSums(Violations, na.rm = TRUE)

#Violation Ratio object

# Remove the rows with NA
#Remember "!" is the "NOT" operator
Violations <- Violations[!is.na(Violations[,1]),]

# Get the column sums
V <- colSums(Violations)
V

# Calculate expected violations
EV <- dim(Violations)[1]*p
EV

# Violation Ratios
VR <- V/EV

# Call object, rounding to 3 decimals
round(VR,3)



# We can write a function that uses a rule of thumb to assess the model
model_assessment <- function(VR) {
  if (VR > 0.8 & VR < 1.2) {
    paste0(names(VR), "Model is good")
  } else if ((VR > 0.5 & VR <= 0.8) | (VR > 1.2 & VR <= 1.5)) {
    paste0(names(VR), "Model is acceptable")
  } else if ((VR > 0.3 & VR <= 0.5) | (VR > 1.5 & VR <= 2)) {
    paste0(names(VR), "Model is bad")
  } else {
    paste0(names(VR), "Model is useless")
  }
}

# We can use sapply(), the vector version of apply()
sapply(VR, model_assessment)

# Best performing - VR closest to 1
#Subtract 1 to make it easier to compare
sort(round(abs(VR-1),3)) 


# Stress Testing---------------------------------------------------

# Subset for crisis periods
crisis <- year(dates) >= 2019 & year(dates) < 2022
y_crisis <- y[crisis]
VaR_crisis <- VaR[crisis,]

Violations_crisis <- VaR_crisis
Violations_crisis[] <- NA

for(i in 1:dim(VaR_crisis)[2]){
  Violations_crisis[,i] <- y_crisis*portfolio_value < -VaR_crisis[,i]
}


# Remove the rows with NA
Violations_crisis <- Violations_crisis[!is.na(Violations_crisis[,1]),]

# Get the column sums
V_crisis <- colSums(Violations_crisis)

# Calculate expected violations
EV_crisis <- dim(Violations_crisis)[1]*p

# Violation Ratios
VR_crisis <- V_crisis/EV_crisis

# Call object, rounding to 3 decimals
round(VR_crisis,3)

sapply(VR_crisis, model_assessment)

# Best performing - VR closest to 1
sort(round(abs(VR_crisis-1),3))


