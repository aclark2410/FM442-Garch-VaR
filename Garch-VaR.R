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
  return(list(Y = Y, PRC = PRC))
}

processed_data <- process_data("data.csv")

# Access Y and PRC
Y <- processed_data$Y
PRC <- processed_data$PRC
head(Y)
head(PRC)

# Save returns as CSV files
write.csv(Y, file = "Y.csv")
write.csv(PRC, file = "PRC.csv")

load("Y.RData")

#################### Test for Normality in the returns distribution ####################

check_normality <- function(returns, company_name) {
  # Get the mean and sd of returns
  mean_returns <- mean(returns)
  sd_returns <- sd(returns)
  
  # Display mean and standard deviation
  cat("Mean:", round(mean_returns, 3), "\n")
  cat("Standard Deviation:", round(sd_returns, 3), "\n")
  
  # Create the histogram
  hist(returns, freq = FALSE, main = paste("Returns of", company_name), col = "lightgrey", breaks = 50)
  
  # Add the normal distribution
  x <- seq(-3, 3, 0.001)
  lines(x, dnorm(x, mean = mean_returns, sd = sd_returns), lwd = 3, col = "red")
  
  # Test for normality
  jb_test <- jarque.bera.test(returns)
  cat("Jarque-Bera Test p-value:", jb_test$p.value, "\n")
  
  # qqPlot of the normal distribution
  qqPlot(returns, distribution = "norm", envelope = FALSE, main = "Q-Q Plot")
  
  # Q-Q Plots for t-distributions with different degrees of freedom
  for (df in c(2, 3, 4)) {
    qqPlot(returns, distribution = "t", df = df, envelope = FALSE, main = paste(df, "Degrees of Freedom"))
  }
}

# Normality output
check_normality(Y$AAPL, "AAPL")
check_normality(Y$MSFT, "MSFT")
check_normality(Y$UAL, "UAL")



############################# Implementing HS to estimate VaR and ES ##################################

rm(list=ls())
# Load the data
load("Y.RData")

# Extract the returns for AAPL, MSFT and UAL
aaplReturns <- Y$AAPL
msftReturns <- Y$MSFT
ualReturns <- Y$UAL

# Function to plot returns 
plot_returns <- function(date, returns, asset_name) {
  plot(date, returns, type = "l", main = paste("Returns for", asset_name), xlab = "Date", ylab = "Returns")
}

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
  plot(sorted_returns, type = "l", main = "Sorted Returns", xlab = "Observations", ylab = "Returns")
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
  plot(dates_var, VaR_vector, type = "l", main = "VaR HS with Estimation Window",
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
  plot(HS$HS100, main = "HS with different estimation windows", ylab = "VaR in USD",
       xlab = "Date", type = "l", col = "red", lwd = 2)
  lines(HS$HS500, col = "blue", lwd = 2)
  lines(HS$HS1000, col = "green", lwd = 2)
  lines(HS$HS2000, col = "black", lwd = 2)
  
  legend("topleft", legend = names(HS), lty = 1, col = c("red", "blue", "green", "black"))
  
  # Subset HS to keep only the observations that fit the most restrictive estimation window
  # This is now the window = 5000
  HS_subset <- HS[(max(windows) + 1):n, ]
  
  # Plotting them all together
  plot(plot_dates, HS_subset$HS100, main = "HS with different estimation windows", ylab = "VaR in USD",
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


######################### Garch VaR ##############################

# Process and prepare data
load_and_process_asset_data <- function(asset_var, y_r_data_path, asset_name) {
  
  VaRold <- asset_var
  
  # Load returns
  load(y_r_data_path)
  
  # Select the specified asset
  y <- Y[[asset_name]]
  dates <- Y$date
  
  # Return a named list with multiple elements
  return(list(VaRold = VaRold, Returns = y, Dates = dates))
}

# Assign data to variables 
aapl_processed_asset_data <- load_and_process_asset_data(aapl_var, "Y.RData", "AAPL")
msft_processed_asset_data <- load_and_process_asset_data(msft_var, "Y.RData", "MSFT")
ual_processed_asset_data <- load_and_process_asset_data(ual_var, "Y.RData", "UAL")

# AAPL data
aaplVaRold <- aapl_processed_asset_data$VaRold
aaplReturns <- aapl_processed_asset_data$Returns
aaplDates <- aapl_processed_asset_data$Dates

# MSFT data
msftVaRold <- msft_processed_asset_data$VaRold
msftReturns <- msft_processed_asset_data$Returns
msftDates <- msft_processed_asset_data$Dates

# UAL data
ualVaRold <- ual_processed_asset_data$VaRold
ualReturns <- ual_processed_asset_data$Returns
ualDates <- ual_processed_asset_data$Dates

# Parameters
portfolio_value = 1000
p = 0.05 #probability for VaR

# Function that creates a GARCH forecast
DoGARCH <- function(y, spec, probability = 0.05, portfolio_value = 1, WE = 1000) {
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
      "Portfolio value:", portfolio_value, "\n")
  
  # Number of observations
  n <- length(y)
  
  # Initialize empty VaR vector
  VaR <- rep(NA, n)
  
  # Do a loop for the forecast
  for (i in 1:(n - WE)) {
    
    # Subset the dataset to the estimation window
    window <- y[i:(i + WE - 1)]
    
    # Fit the GARCH
    res <- ugarchfit(spec = spec, data = window, solver = "hybrid")
    
    # Save coefficients
    omega <- coef(res)['omega']
    alpha <- coef(res)['alpha1']
    beta <- coef(res)['beta1']
    
    # Estimate sigma2 using the last observation of window
    sigma2 <- omega + alpha * tail(window, 1)^2 + beta * tail(res@fit$var, 1)
    
    # Allocate the VaR forecast in the vector
    VaR[i + WE] <- -sqrt(sigma2) * qnorm(probability) * portfolio_value
    
    # Print progress
    if (i %% 100 == 0) {
      cat("\rProgress: ", round((i / (n - WE)) * 100, 2), "%", sep = "")
    }
  }
  
  # Get the new time and print the elapsed time
  time <- difftime(Sys.time(), old, units = "secs")
  cat("\nElapsed time:", round(time, 4), "seconds\n")
  
  # Return the VaR vector
  return(VaR)
}


# Calculate Garch over time frame period

calculate_GARCH_VaR <- function(returns, window_length, probability, portfolio_value, file_name) {
  spec <- ugarchspec(
    variance.model = list(garchOrder= c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean=FALSE)
  )
  
  GARCH_result <- DoGARCH(returns, spec = spec, probability = probability, portfolio_value = portfolio_value, WE = window_length)
  
  save(GARCH_result, file = file_name)
  
  return(GARCH_result)
}
############# SAVING GARCH TIMEFRAME DATA ############# (please read instructions below for successful data saving)
# *** Wait for progress bar to complete and/or time to elapse otherwise data won't save! ***
# AAPL:
aaplGARCH300 <- calculate_GARCH_VaR(aaplReturns, window_length = 300, probability = 0.05, portfolio_value = 1000, file_name = "AAPLGARCH300.RData")

aaplGARCH2000 <- calculate_GARCH_VaR(aaplReturns, window_length = 2000, probability = 0.05, portfolio_value = 1000, file_name = "AAPLGARCH2000.RData")

aaplGARCH_VaR <- cbind(aaplGARCH300, aaplGARCH2000)

# MSFT
msftGARCH300 <- calculate_GARCH_VaR(msftReturns, window_length = 300, probability = 0.05, portfolio_value = 1000, file_name = "MSFTGARCH300.RData")

msftGARCH2000 <- calculate_GARCH_VaR(msftReturns, window_length = 2000, probability = 0.05, portfolio_value = 1000, file_name = "MSFTGARCH2000.RData")

msftGARCH_VaR <- cbind(msftGARCH300, msftGARCH2000)

# UAL
ualGARCH300 <- calculate_GARCH_VaR(ualReturns, window_length = 300, probability = 0.05, portfolio_value = 1000, file_name = "UALGARCH300.RData")

ualGARCH2000 <- calculate_GARCH_VaR(ualReturns, window_length = 2000, probability = 0.05, portfolio_value = 1000, file_name = "UALGARCH2000.RData")

ualGARCH_VaR <- cbind(ualGARCH300, ualGARCH2000)

# Function to plot VaR forecasts
plot_var_forecasts <- function(dates, VaR, ylim = NULL, legend_pos = "topright") {
  matplot(dates, VaR, type = "l", lty = 1, col = 1:ncol(VaR), xaxt = "n",
          main = "VaR forecasts", xlab = "Date", ylab = "VaR USD", ylim = ylim)
  axis.Date(1, at = seq(min(dates), max(dates), by = "years"))
  
  # Legend
  legend(legend_pos, legend = colnames(VaR), cex = 0.6, lty = 1, col = 1:ncol(VaR))
}

# Plot VaR forecasts
plot_var_forecasts(Y$date, aaplGARCH_VaR, ylim = c(0, 200), legend_pos = "topleft")
plot_var_forecasts(Y$date, msftGARCH_VaR, ylim = c(0, 200), legend_pos = "topleft")
plot_var_forecasts(Y$date, ualGARCH_VaR, ylim = c(0, 350), legend_pos = "topleft")

# Function that combines VaR and Garch forecasts and plots them 
combine_and_plot_VaR <- function(VaRold, GARCH300, GARCH2000, dates, ylim) {
  # Combine all VaR and Garch forecasts
  VaR <- cbind(VaRold, GARCH300, GARCH2000)
  
  # Print means for each forecast
  cat("Means for each forecast:\n")
  print(round(colMeans(VaR, na.rm = TRUE), 3))
  
  # Print standard deviations for each forecast
  cat("\nStandard deviations for each forecast:\n")
  print(round(colSds(as.matrix(VaR), na.rm = TRUE), 3))
  
  # Plot all VaR forecasts
  matplot(dates, VaR, type = "l", lty = 1, col = 1:6, xaxt = "n",
          main = "VaR forecasts", xlab = "Date", ylab = "VaR USD", ylim = NULL)
  axis.Date(1, at = seq(min(dates), max(dates), by = "years"))
  
  # Add legend
  legend("topleft", legend = colnames(VaR), cex = 0.6, lty = 1, col = 1:6)
}

# Var and Garch Plots
combine_and_plot_VaR(aaplVaRold, aaplGARCH300, aaplGARCH2000, aaplDates, ylim = c(0,200))
combine_and_plot_VaR(msftVaRold, msftGARCH300, msftGARCH2000, msftDates, ylim = c(0,200))
combine_and_plot_VaR(ualVaRold, ualGARCH300, ualGARCH2000, ualDates, ylim = c(0,350))

# Function for Comparison Plots
compare_and_plot_VaR <- function(dates, VaR, ylim = NULL, legend_pos = "topright") {
  # Means for each forecast
  means <- round(colMeans(VaR, na.rm = TRUE), 3)
  cat("Means for each forecast:\n")
  print(means)
  
  # Standard deviations
  sds <- round(colSds(as.matrix(VaR), na.rm = TRUE), 3)
  cat("\nStandard deviations for each forecast:\n")
  print(sds)
  
  # Plot all
  matplot(dates, VaR, type = "l", lty = 1, col = 1:ncol(VaR), xaxt = "n", 
          main = "VaR forecasts", xlab = "Date", ylab = "VaR USD", ylim = ylim)
  axis.Date(1, at = seq(min(dates), max(dates), by = "years"))
  
  # Legend
  legend(legend_pos, legend = colnames(VaR), cex = 0.6, lty = 1, col = 1:ncol(VaR))
}

# Comparison plots
compare_and_plot_VaR(Y$date, aaplGARCH_VaR, ylim = c(0, 200), legend_pos = "topleft")
compare_and_plot_VaR(Y$date, msftGARCH_VaR, ylim = c(0, 200), legend_pos = "topleft")
compare_and_plot_VaR(Y$date, ualGARCH_VaR, ylim = c(0, 350), legend_pos = "topleft")


############# Violations ############# *** NEED TO FIX DOESN"T WORK YET ***

#Testing how often VaR is violated in different models
#Violation is when return is smaller than -VaR(p)
#We know this should happen approximately 100p% of times
#Happening more often means we are underforecasting risk
#Less often means we are overforecasting risk

#Violation ratio=#violations/#Expected_violations

aaplVaR <- cbind(aaplVaRold, aaplGARCH300, aaplGARCH2000)
msftVaR <- cbind(msftVaRold, msftGARCH300, msftGARCH2000)
ualVaR <- cbind(ualVaRold, ualGARCH300, ualGARCH2000)

# Initialize a Violations data.frame, same dim and colnames as VaR, fill with NA
Violations <- aaplVaR
Violations[] <- NA

# For every model (columns in VaR) restricted to largest estimation window
for(i in 1:dim(aaplVaR)[2]){
  
  # Fill the column in Violations with TRUE/FALSE
  # TRUE if the realized return is lower than VaR
  # FALSE otherwise
  Violations[,i] <- (aaplReturns*portfolio_value < -aaplVaR[,i])
}


# Restrict to largest estimation window
Violations[1:(start-1),] <- NA

head(Violations) #We have just set first observations to NA
tail(Violations)

calculate_Violation_Ratio <- function(returns, VaR, portfolio_value, start) {
  # Initialize a Violations data.frame, same dim and colnames as VaR, fill with NA
  Violations <- VaR
  Violations[] <- NA
  
  # For every model (columns in VaR) restricted to the largest estimation window
  for (i in 1:dim(VaR)[2]) {
    # Fill the column in Violations with TRUE/FALSE
    # TRUE if the realized return is lower than VaR
    # FALSE otherwise
    Violations[, i] <- (returns * portfolio_value < -VaR[, i])[1:nrow(Violations)]
  }
  
  # Restrict to the largest estimation window
  Violations[1:(start - 1), ] <- NA
  
  # Find where violations happened in each model
  violation_indices <- apply(Violations, 2, function(col) which(col))
  
  # Get a random day where a violation occurred using sample()
  random_day <- sample(violation_indices$VaR, 1)
  
  # Find the index in dates using which()
  day_index <- random_day
  
  # Return the Violations data.frame and the randomly selected day
  return(list(Violations = Violations, random_day = day_index))
}

# Example usage:
result <- calculate_Violation_Ratio(aaplReturns, VaR = aaplVaR, portfolio_value = 1000, start = 20111230)

# Access the Violations data.frame
head(result$Violations)

# Access the randomly selected day where a violation occurred
random_day <- dates[result$random_day]
cat("Random Violation Day:", random_day, "\n")



# Plotting the violations
plot(dates[start:end], VaR$EWMA_VaR[start:end],xlab="Dates", ylab="EWMA VaR", type = "l", main = "EWMA VaR with violations",)

# Add points where the violations happened
#cex can again control the size of the points
points(dates[Violations$EWMA_VaR], VaR$EWMA_VaR[Violations$EWMA_VaR], cex=0.5,pch = 16, col = "red")

# Check dates where all models have a violation
w <- apply(Violations, 1, all)

# na.rm =TRUE means we will firstly remove all NA elements
sum(w, na.rm = TRUE)


# Plotting the returns and adding the days where all models had a violation
plot(dates, y, main = "United Airlines returns", type = "l", lwd = 2, las = 1,
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

# Calculate expected violations
EV <- dim(Violations)[1]*p

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



########################### Stress Testing ############################

# Function to calculate violatiion ratios 
calculate_Violation_Ratios <- function(dates, returns, VaR, portfolio_value, p) {
  # Subset for crisis periods
  crisis <- year(dates) >= 2019 & year(dates) < 2022
  y_crisis <- returns[crisis]
  VaR_crisis <- VaR[crisis, ]
  
  # Initialize Violations data.frame
  Violations_crisis <- VaR_crisis
  Violations_crisis[] <- NA
  
  # Calculate Violations
  for (i in 1:dim(VaR_crisis)[2]) {
    Violations_crisis[, i] <- y_crisis * portfolio_value < -VaR_crisis[, i]
  }
  
  # Remove rows with NA
  Violations_crisis <- Violations_crisis[!is.na(Violations_crisis[, 1]), ]
  
  # Get column sums
  V_crisis <- colSums(Violations_crisis)
  
  # Calculate expected violations
  EV_crisis <- dim(Violations_crisis)[1] * p
  
  # Calculate Violation Ratios
  VR_crisis <- V_crisis / EV_crisis
  
  # Print Violation Ratios
  cat("Violation Ratios:\n")
  print(round(VR_crisis, 3))
  
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
  
  # Model Assessment
  cat("\nModel Assessment:\n")
  sapply(VR_crisis, model_assessment)
  
  # Best performing - VR closest to 1
  cat("\nBest Performing Models (VR closest to 1):\n")
  print(sort(round(abs(VR_crisis - 1), 3)))
  
  # Return Violation Ratios
  return(VR_crisis)
}

# Violation ratio output
calculate_Violation_Ratios(aaplDates, aaplReturns, aaplVaR, portfolio_value = 1000, p = 0.05)
calculate_Violation_Ratios(msftDates, msftReturns, msftVaR, portfolio_value = 1000, p = 0.05)
calculate_Violation_Ratios(ualDates, ualReturns, ualVaR, portfolio_value = 1000, p = 0.05)

