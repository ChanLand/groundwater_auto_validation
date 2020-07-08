# Load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)


# Load data
#######################################################
file <- read_csv("Data/R54_S1_20181017_20191018.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/cdv-9-1i_pz2_101718_101719.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/R10a_010118_063019.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/R33S2_010118_063019.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/R20S2_010118_063019.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/cdv-16-1i_102818_102819.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/R63_110418_110419.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/R23i_110418_110419.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/TA53i_110418_110419.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/R55S1_110418_110419.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/R26PZ2_110418_110419.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

file <- read_csv("Data/R26_110418_110419.csv", 
                 col_types = list(Received = col_datetime("%m/%d/%Y %H:%M")))

# Datacenter exports files with most recent date first - change order so that it starts with earliest data
file <- file %>%
  arrange(Received)


##################################################################################
# From http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/

movingAverage <- function(x, n=1, centered=FALSE) {
  
  
  if (centered) {
    before <- floor  ((n-1)/2)
    after  <- ceiling((n-1)/2)
  } else {
    before <- n-1
    after  <- 0
  }
  
  # Track the sum and count of number of non-NA items
  s     <- rep(0, length(x)) # this gives a vector of 0s for the length of the dataset
  count <- rep(0, length(x))  # same as s here
  
  # Add the centered data 
  new <- x
  # Add to count list wherever there isn't a 
  count <- count + !is.na(new) # add a vector of 0s and 1s (1s if not NA) to vector of zeros - count of how many NAs there are
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0 # NAs become zeros
  s <- s + new # s is the vector of non-NA data
  
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new   <- c(rep(NA, i), x[1:(length(x)-i)]) # a vector starting with NA, followed by values of x from 1 to x-1; 
    # shifts out 1 each time so final vector is "before" number of NAs or 0s, then data
    
    count <- count + !is.na(new) # count (this should be 1 less than original count due to NA just added)
    new[is.na(new)] <- 0 
    s <- s + new
    
    i <- i+1
  }
  
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new   <- c(x[(i+1):length(x)], rep(NA, i))
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i+1
  }
  
  # return sum divided by count
  s/count
}

########################################################
# Identify anomalies in water level data
########################################################

# Find trendline using movingAverage function and append to dataframe
trend <- movingAverage(file[[4]], n = 360, centered = TRUE)
file <- bind_cols(file, as.data.frame(trend))

# Detrend the data
file <- mutate(file, detrend = file[[4]] - trend)

# Calculate MAD of detrended data
mad <- median(abs(file[[7]] - median(file[[7]], na.rm = TRUE)), na.rm = TRUE) * 1.4826 # multiplier for normally distributed data

# Add MAD thresholds to MA of water level data (not MA of detrended data)
file <- file %>%
  mutate(mad_up = 4 * mad + trend, mad_low = (-4) * mad + trend) %>%
  mutate(mad_anomaly = ifelse(`Elevation Ft` > mad_up | `Elevation Ft` < mad_low, 'Yes', 'No'))

# plot the anomalies 
ggplot(file, aes(Received, `Elevation Ft`, color = mad_anomaly)) +
  geom_point(alpha = 0.5) + 
  scale_color_manual(values = c('mediumseagreen', 'red')) +
  geom_line(aes(Received, trend), color = 'black') +
  geom_line(aes(Received, mad_up), linetype = 'dashed', color = 'black') +
  geom_line(aes(Received, mad_low), linetype = 'dashed', color = 'black') +
  xlab('Date') +
  ylab('Water level (ft)') +
  theme_bw()

# plot the detrended data
#ggplot(file, aes(Received, detrend, color = mad_anomaly)) + 
  #geom_point(alpha = 0.5) +
  #scale_color_manual(values = c('mediumseagreen', 'red')) +
  #geom_line(aes(Received, detrend_MA), color = 'black') +
  #xlab('Date') +
  #ylab('Detrended Water level (ft)') +
  #theme_bw()


############################################
# Identify anomalies in temperature data
############################################

# Find trendline using movingAverage function and append to dataframe
trend_t <- movingAverage(file[[3]], n = 360, centered = TRUE)
file <- bind_cols(file, as.data.frame(trend_t))

# Detrend the data
file <- mutate(file, detrend_t = file[[3]] - trend_t)

# Calculate 3*SD of detrended temperature data - these will be the error bounds - add to moving average
file <- file %>%
  mutate(sd_t_up = (3 * sd(detrend_t, na.rm = TRUE) + trend_t) , sd_t_low = ((-3) * sd(detrend_t, na.rm = TRUE) + trend_t)) %>%
  mutate(t_anomaly = ifelse(`Temperature C` > sd_t_up | `Temperature C` < sd_t_low, 'Yes', 'No'))

# Plot temperature anomalies
ggplot(file, aes(Received, `Temperature C`, color = t_anomaly)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c('dodgerblue', 'red')) +
  geom_line(aes(Received, sd_t_up), linetype = 'dashed', color = 'black') +
  geom_line(aes(Received, sd_t_low), linetype = 'dashed', color = 'black') +
  xlab('Date') +
  ylab('Temperature (C)') +
  theme_bw()

# plot detrended data with anomalies
#ggplot(file, aes(Received, detrend_t, color = t_anomaly)) +
  #geom_point(alpha = 0.5) +
  #scale_color_manual(values = c('gold', 'red')) +
  #xlab('Date') +
  #ylab('Detrended Temperature (C)') +
  #theme_bw()

