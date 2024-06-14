##########################################################################
# Governor's Challenge
# GDP Components Forecasting Code
# Created on: Sep 15, 2023
# Last Modified: Feb 13, 2024

# Initialize
rm(list = ls())

setwd("C:/Users/amrit/Desktop/ECON 485/R")

# Packages ####
library(cansim)       # Get data from StatsCan
library(fredr)        # Get data from FRED
library(forecast)     # Time Series Forecasting
library(vars)         # Vector Autoregression
library(lubridate)    # Easy date conversions
library(openxlsx)     # Simplify creation of excel files
library(stringr)      # Working with Strings
library(xts)          # Extends Time Series Class
library(tsbox)        # Handle Time Series as Data Frames
library(tis)          # Time Index Series
library(writexl)      # Convert TS to excel
library(tseries)      # ADF Tests

# Functions ####
source("functions/ts_cansim.R")   
source("functions/ts_fred.R")
source("functions/forecast_conditional_var.R")

############################### 1. Data ##################################

########## 1.1 Reading in Data ####

# Read in Excel file containing breakeven rate data
breakeven <- read.xlsx("Data/Breakeven Rate 2.xlsx")

# Convert the numeric date to a human-readable date format
breakeven$Date <- as.Date(breakeven$date, origin = "1899-12-30")

# Set start and end dates with frequency
strt <- c(2001,1)
end <- c(2024,1)
freq <- 12

# Create time series 
breakeven.ts <- ts(data = breakeven$Rate, start = strt, end = end, frequency = freq)

# Setting start date
date.start <- "2001-01-01"

# Statistics Canada Data 

cpi        <- ts_cansim("v41690914", start = date.start)  # CPI, Monthly
target.d   <- ts_cansim("v39079", start = date.start)     # Target rate, Daily
wage       <- ts_cansim("v2132579", start = date.start)   # wage: Average hourly wage rate, Current dollars
unemployment  <- ts_cansim("v2062815", start = date.start) # Unemployment rate, Monthly, Seasonally
cons       <- ts_cansim("v62305723", start = date.start) # Consumption at basic prices, Dollars
ex         <- ts_cansim("v62305745", start = date.start)  # Exports at basic prices, Dollars
inv        <- ts_cansim("v62305733", start = date.start) # Investment at basic prices, Dollars
im         <- ts_cansim("v62305748", start = date.start) # Imports at basic prices, Dollars
bus_cap    <- ts_cansim("v62143959", start = date.start) # part of non-inventory investment
nonprofit_cap  <- ts_cansim("v62144005", start = date.start) #part of non-inventory investment

# Private Non-inventory investment
private <- bus_cap + nonprofit_cap

# FRED Data 
fredr_set_key('7b15ffc9ff456a8d5e3e579d2b04a9f8')        # FRED Key
wti        <- ts_fred('MCOILWTICO', start = date.start)   # WTI oil price, Monthly

# Monthly US GDP estimates from: https://ihsmarkit.com/products/us-monthly-gdp-index.html
url.usgdp  <- "https://cdn.ihsmarkit.com/www/default/1020/US-Monthly-GDP-History-Data.xlsx"
df.usgdp   <- read.xlsx(url.usgdp, sheet = 'Data')
gdp.us     <- ts(df.usgdp$Monthly.Real.GDP.Index, start = c(1992, 1), freq = 12)


## a) Converting Daily Target Rate to Quarterly

# Convert to xts object
seq.time       <- seq(as.Date(date.start), by = 'day', length.out = length(target.d))
target.xts.d   <- xts(as.numeric(target.d), seq.time)

# Create the most frequent value aggregation function
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Aggregate to quarterly frequency
target.xts <- aggregate(target.xts.d, as.yearqtr, calculate_mode)
target <- as.ts(target.xts)

## b) Converting Monthly Data to Quarterly 
freq.convert <- 4
cpi <- aggregate.ts(cpi,nfrequency = freq.convert, FUN = mean)
wti <- aggregate.ts(wti,nfrequency = freq.convert, FUN = mean)
wage <- aggregate.ts(wage, nfrequency = freq.convert, FUN = mean)
gdp.us <- aggregate.ts(gdp.us, nfrequency = freq.convert, FUN = mean)
unemployment <- aggregate.ts(unemployment, nfrequency = freq.convert, FUN = mean)
breakeven <- aggregate.ts(breakeven.ts, nfrequency = freq.convert, FUN = mean)


## c) Check for stationarity 
# Uses Augmented Dickey - Fuller test
# Use p-value, not Pr(>|t|)
# adf.test(cons) # passes
# adf.test(private) # fails
# adf.test(ex) # fails
# adf.test(im) # fails
# adf.test(inv) # fails
# adf.test(breakeven) # passes

# adf.test(target) # Fails
# adf.test(wage) # Fails
# adf.test(wti) # Fails
# adf.test(cpi) # Fails
# adf.test(unemployment) # Passes

## e) Making Series Stationary for Variables that Didn't Pass
TARGET         <- target                                           # Leave as rate
INF            <- diff(log(cpi), 4)                                # Convert cpi to yoy inflation
CONS           <- diff(log(cons),4)                                # Convert to yoy growth
INV            <- diff(log(inv),4)                                 # Convert to yoy growth
EX             <- diff(log(ex),4)                                  # Convert to yoy growth
IM             <- 100*diff(log(im),4)                              # Convert to yoy growth
GDP.US         <- diff(log(gdp.us), 4)                             # Convert to yoy GDP growth
WTI            <- diff(wti)                                        # Leave as price level
WAGE           <- diff(log(wage), 4)                               # Convert to yoy wage growth
UNEMPLOYMENT   <- unemployment                                     # leave as rate
PRIVATE        <- diff(log(private), 4)  
BREAKEVEN      <- breakeven                                        # leave as rate

## f) Check if difference is enough
# adf.test(CONS) # passes
# adf.test(EX) # fails 
# adf.test(IM) # passes
# adf.test(INV) # fails 
# adf.test(PRIVATE)
# 
# adf.test(TARGET) # Fails
# adf.test(WAGE) # Fails 
# adf.test(WTI) # Passes
# adf.test(INF) # Fails 
# adf.test(UNEMPLOYMENT) # Passes

## g) Check that lengths line up
# length(INF)
# length(GDP.US)
# length(WAGE)
# length(WTI)

## Bind all the VAR endogenous data into an object ####
data <- cbind(TARGET, EX, BREAKEVEN, WAGE, WTI, GDP.US, UNEMPLOYMENT) # Rotate in CONS, IM, INV in place of EX
                                                                      # Don't change location in dataframe otherwise you need to re-restrict the coefficients

# write.xlsx(data,"governor's challenge components data private.xlsx")
tail(data)
plot(data)

## Trim the data into same length ####
trim.strt <- c(2002,1)
est.end <- c(2023,3)
gdp.end <- c(2023,3)
final.end <- c(2023,3)

# var.data.est is for estimating parameters
var.data.est <- window(data, start = trim.strt, end = est.end)
# var.data.gdp is for estimating gdp one-step ahead model
var.data.gdp <- window(data, start = trim.strt, end = gdp.end)
# var.data.final is for estimating final model
var.data.final <- window(data, start = trim.strt, end = final.end)

# Select number of lags, HQ(n) is BIC 
VARselect(var.data.est, lag.max = 3) # HQ, SC, FPE suggest 1
n.lag             <- 1
mod.est           <- VAR(var.data.est, p = n.lag)

########## 2.2 Restrict Coefficients ####
mat.coef          <- sapply(coef(mod.est), function(x) x[,'Estimate']) # extract 'Estimate' column and save

# Transpose Matrix to get Correct Dimensions
mat.coef.res      <- t(mat.coef)
mat.coef.res[,]   <- 1 # select all elements and assign value of 1

# Impose Restrictions - Dependent on Lag Length (1 Lag)
mat.coef.res[c('WTI','GDP.US'), 1:4] <- 0
mat.coef.res[c('WTI','GDP.US'), 7:7] <- 0
mat.coef.res

########## 2.3 Re-estimate Model ####
mod.restrict       <- restrict(mod.est, method = "man", resmat = mat.coef.res)
coef(mod.restrict)

  ###### INF #####
    # coefficients_INV <- mod.restrict$varresult$INV$coefficients
    # 
    # # Set a new value for the CPI INF intercept
    # new_intercept_value <- -0.016955623  # DEFAULT: -0.019955623 for inv
    # 
    # # Update the constant (intercept) in the coefficients
    # coefficients_INV["const"] <- new_intercept_value
    # 
    # # Update the coefficients in the model
    # mod.restrict$varresult$INV$coefficients <- coefficients_INV
    # mod.restrict


  ##### EXPORTS ####
    coefficients_EX <- mod.restrict$varresult$EX$coefficients

    # # Set a new value for the CPI INF intercept
    # new_intercept_value <- -0.025003797730  # DEFAULT: -0.0503797730
    # 
    # # Update the constant (intercept) in the coefficients
    # coefficients_EX["const"] <- new_intercept_value
    # 
    # # Update the coefficients in the model
    # mod.restrict$varresult$EX$coefficients <- coefficients_EX
    # mod.restrict

    
###### IMPORTS #####
    coefficients_IM <- mod.restrict$varresult$IM$coefficients
    
    # Set a new value for the CPI INF intercept
    new_intercept_value <- -10  # DEFAULT: -11.5778616
    
    # Update the constant (intercept) in the coefficients
    coefficients_IM["const"] <- new_intercept_value
    
    # Update the coefficients in the model
    mod.restrict$varresult$IM$coefficients <- coefficients_IM
    mod.restrict    
    
    
########## 2.4 Granger Causality Tests ####
  # causality(mod.restrict, cause = 'INF')
  # causality(mod.restrict, cause = 'WAGE')
  # causality(mod.restrict, cause = 'TARGET')
  # causality(mod.restrict, cause = 'WTI')
  # causality(mod.restrict, cause = 'GDP.US')
  # causality(mod.restrict, cause = 'UNEMPLOYMENT')
  # causality(mod.restrict, cause = 'BREAKEVEN')

########## 2.5 Stability ####
roots(mod.restrict) # calculating and retrieving the characteristic roots
# all less than 1, so model is stable

########## 3.1 Set Up for 3-Year Forecast (Quarterly) ####
h <- 12  # Set the forecast horizon to 12 quarters for 3 years

# Calculate the start and end dates for the 3-year forecast period (quarterly)
date.fc.start <- as.yearqtr(final.end)

date.fc.end   <- date.fc.start + 2.25

# Other setup remains the same
varnames      <- colnames(var.data.est)

# Inspect Unconditional Forecast
fc.uncond     <- forecast(mod.restrict, h = h)
plot(fc.uncond$forecast$IM, include = 12)  # To visualize all 12 quarters

# Plot Individually
# autoplot(fc.uncond$forecast$TARGET)
# autoplot(fc.uncond$forecast$INV)
# autoplot(fc.uncond$forecast$CONS)
# autoplot(fc.uncond$forecast$IM)
# autoplot(fc.uncond$forecast$EX)


########## 3.2 Create Time Series Matrix with known values ####

# Create a time series window with known values (quarterly)
data.fc <- window(data, start = date.fc.start, end = date.fc.end, extend = TRUE)

########## 3.3 Paths of Conditioned on Variables ####
window(data.fc[, "TARGET"], start = c(2024, 1), end = c(2026, 1))      <-
  c(rep(5, 3), rep(4.75, 4), rep(4.5, 2))

# Checking Imposed Values have the Right Scale and Timing
data.fc

# Check the number of rows in the data.fc matrix
num_rows_data.fc <- nrow(data.fc)

# Conditional Forecast
fc <- forecast_conditional_var(mod.restrict, h, data.fc)

plot(fc, include = 32)

# Plot forecasts - make sure to comment out GDP components not currently
# being forecasted
autoplot(fc, colour = FALSE,  showgap = TRUE)  +
  autolayer(data.fc, color = 'red')

 autoplot(fc$forecast$TARGET, include = 32) +
   autolayer(data.fc[,'TARGET'], color = 'magenta')

 autoplot(fc$forecast$INV, include = 32) +
   autolayer(data.fc[,'INV'], color = 'magenta')

 autoplot(fc$forecast$CONS, include = 32) +
   autolayer(data.fc[,'CONS'], color = 'magenta')

 autoplot(fc$forecast$IM, include = 32) +
   autolayer(data.fc[,'IM'], color = 'magenta')

 autoplot(fc$forecast$EX, include = 32) +
   autolayer(data.fc[,'EX'], color = 'magenta')

fc$forecast


##########################################################################

############################ 4. XLSX Export ##############################

write.xlsx(fc.uncond,"governor's challenge components forecast.xlsx")

