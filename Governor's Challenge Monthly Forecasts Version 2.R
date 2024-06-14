##########################################################################
# Governor's Challenge
# Master Forecasting Code
# Created on: Nov 6, 2023
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
library(readxl)       # More excel
library(zoo)

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
plot(breakeven.ts)

## Time Horizon of Data 
date.start <- '2001-01-01' 

# Statistics Canada Data 
target.d   <- ts_cansim("v39079", start = date.start)     # Target rate, Daily
cpi        <- ts_cansim("v41690914", start = date.start)  # CPI, Monthly
cpi.trim   <- ts_cansim("v108785715", start = date.start) # CPI-trim inflation, Monthly
cpi.common <- ts_cansim("v108785713", start = date.start) # CPI-common, Monthly 
cpi.median <- ts_cansim("v108785714", start = date.start) # CPI-median, Monthly
gdp       <- ts_cansim("v65201210", start = date.start)  # GDP Current, Monthly, by industry
wage       <- ts_cansim("v2132579", start = date.start)   # wage: Average hourly wage rate, Current dollars
unemployment  <- ts_cansim("v2062815", start = date.start) # Unemployment rate, Monthly, Seasonally

# FRED Data 
fredr_set_key('7b15ffc9ff456a8d5e3e579d2b04a9f8')        # FRED Key
wti        <- ts_fred('MCOILWTICO', start = date.start)   # WTI oil price, Monthly

# Monthly US GDP estimates from: https://ihsmarkit.com/products/us-monthly-gdp-index.html
url.usgdp  <- "https://cdn.ihsmarkit.com/www/default/1020/US-Monthly-GDP-History-Data.xlsx"
df.usgdp   <- read.xlsx(url.usgdp, sheet = 'Data')
gdp.us     <- ts(df.usgdp$Monthly.Real.GDP.Index, start = c(1992, 1), freq = 12)

## a) Converting Daily Target Rate to Monthly 

# Convert to xts object
seq.time       <- seq(as.Date(date.start), by = 'day', length.out =  length(target.d))
target.xts.d   <- xts(as.numeric(target.d), seq.time)

# seq.time       <- seq(as.Date(date.start), by = 'day', length.out =  length(corporate.bonds))
# corporate.xts.d <- xts(as.numeric(corporate.bonds), seq.time)
# 
# seq.time       <- seq(as.Date(date.start), by = 'day', length.out =  length(gov.bonds))
# gov.bonds.xts.d <- xts(as.numeric(gov.bonds), seq.time)

# Create the most frequent value aggregation function
calculate_mode <- function(x) {
  uniqx        <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]}

# Aggregate Target Rate
target.xts     <- aggregate(target.xts.d, as.yearmon, calculate_mode)
target         <- as.ts(target.xts)

## d) Check for stationarity 
# Uses Augmented Dickey - Fuller test
# Use p-value, not Pr(>|t|)

# Uncomment the summary block below to run the tests

# adf.test(target) # Fails
# adf.test(wage) # Fails
# adf.test(gdp) # Fails
# adf.test(wage) # Fails
# adf.test(wti) # Fails
# adf.test(cpi) # Fails
# adf.test(unemployment) # Passes
# adf.test(cpi.median) # fails
# adf.test(breakeven.ts) # fails


## e) Making Series Stationary for Variables that Didn't Pass

# when forecasting inflation indicators, be sure to only include one at a time 
# and comment out the other two

TARGET         <- target                                            # Leave as rate
#INF           <- diff(log(cpi), 12)                               # Convert to yoy inflation
INF            <- cpi.trim      
#INF           <- cpi.median
GDP            <- diff(log(gdp), 12)                                # Convert to yoy GDP growth
GDP.US         <- diff(log(gdp.us), 12)                             # Convert to yoy GDP growth
WTI            <- diff(wti)                                         # Leave as price level
WAGE           <- diff(log(wage), 12)                               # Convert to yoy wage growth
UNEMPLOYMENT   <- unemployment                                     # leave as rate
BREAKEVEN      <- breakeven.ts                                      # leave as rate

## e) Check if difference is enough
# adf.test(TARGET) 
# adf.test(WAGE) # Passes
# adf.test(GDP) # Passes
# adf.test(GDP.US) # Passes
# adf.test(WTI) # Passes
# adf.test(UNEMPLOYMENT) # Passes
# adf.test(INF) # passes

## e) Combining All Series in Time Series Matrix
data         <- cbind(INF, GDP, TARGET, GDP.US, WTI, BREAKEVEN, WAGE, UNEMPLOYMENT)  

########## 1.3 Creating Complete Data Set ####

# Inspecting data
tail(data, 24)
# data

ind.complete        <- complete.cases(data)
date.complete       <- yearmon(time(data)[ind.complete])
date.complete.start <- min(date.complete)
date.complete.end   <- max(date.complete)
data.complete       <- window(data, start = c(year(date.complete.start), month(date.complete.start)), 
                              end = c(year(date.complete.end), month(date.complete.end)))

# Check
data.complete
plot(data.complete)       # Plot all TS variables
any(is.na(data.complete)) # Check for missing values


########## 2.1 Lag Selection ####
VARselect(data.complete, lag.max = 3) # AIC suggests 3 lags

# Since we have a large sample size, we will use AIC given that HQ is designed for smaller ones.
n.lag             <- 3
mod.est           <- VAR(data.complete, p = n.lag)

coef(mod.est)

########## 2.2 Restrict Coefficients ####
mat.coef          <- sapply(coef(mod.est), function(x) x[,'Estimate']) # extract 'Estimate' column and save

# Check
mat.coef

# Transpose Matrix to get Correct Dimensions
mat.coef.res      <- t(mat.coef)
mat.coef.res[,]   <- 1


# Impose Restrictions - Exogenous variables should only be allowed to interact with each other - Dependent on Lag Length

mat.coef.res                                     # Pre Restriction
mat.coef.res[c('WTI','GDP.US'), 1:3]    <- 0     # 1. Lag = 3  
mat.coef.res[c('WTI','GDP.US'), 6:11]   <- 0     # 2. Lag = 3  
mat.coef.res[c('WTI','GDP.US'), 14:19]  <- 0     # 3. Lag = 3  
mat.coef.res[c('WTI','GDP.US'), 22:24]  <- 0     # 3. Lag = 3 

# Check restrictions are correct
mat.coef.res                                     # Post Restriction

########## 2.3 Re-estimate Model ####
mod.restrict       <- restrict(mod.est, method = "man", resmat = mat.coef.res)
coef(mod.restrict)

    ######## INF ##########
    coefficients_INF <- mod.restrict$varresult$INF$coefficients
    
    # Set a new value for CPI intercept
    # new_intercept_value <- -0.00155332 # DEFAULT -1.754846e-03
    
    # Set a new value for the CPI trim intercept
    new_intercept_value <- -0.2049005332 # DEFAULT -0.1899005332 for cpi trim
    
    # Set a new value for the CPI median intercept
    # new_intercept_value <- -0.2121471980  # DEFAULT -0.1251471980 for cpi median  
    
    # Update the constant (intercept) in the coefficients
    coefficients_INF["const"] <- new_intercept_value
    
    # Update the coefficients in the model
    mod.restrict$varresult$INF$coefficients <- coefficients_INF
    mod.restrict

    
    ######## UNEMPLOYMENT ##########
      # coefficients_un <- mod.restrict$varresult$UNEMPLOYMENT$coefficients
      # 
      # # Set a new value for unemployment intercept
      # new_intercept_value <- 0.7520909547 # DEFAULT 0.820909547
      # 
      # # Update the constant (intercept) in the coefficients
      # coefficients_un["const"] <- new_intercept_value
      # 
      # # Update the coefficients in the model
      # mod.restrict$varresult$UNEMPLOYMENT$coefficients <- coefficients_un
      # mod.restrict
    
########## 2.4 Granger Causality Tests ####
  # causality(mod.restrict, cause = 'INF')
  # causality(mod.restrict, cause = 'GDP')
  # causality(mod.restrict, cause = 'WAGE')
  # causality(mod.restrict, cause = 'TARGET')
  # causality(mod.restrict, cause = 'WTI')
  # causality(mod.restrict, cause = 'GDP.US')
  # causality(mod.restrict, cause = 'UNEMPLOYMENT')
  # causality(mod.restrict, cause = 'BREAKEVEN')

########## 2.5 Stability ####
roots(mod.restrict) # calculating and retrieving the characteristic roots
# all less than 1, so model is stable

##########################################################################

######################## 3. Iterative Forecast ###########################

########## 3.1 Set Up for Forecast ####
h = 4 + 12 + 12 

date.fc.start <- date.complete.end + 1/12
date.fc.end   <- date.complete.end + h/12

varnames      <- colnames(data.complete)

# Inspect Unconditional Forecast
fc.uncond     <- forecast(mod.restrict, h = h)
plot(fc.uncond$forecast$WAGE, include = 60)


########## 3.2 Create Time Series Matrix with known values ####

data.fc       <- window(data, start = c(year(date.fc.start), month(date.fc.start)), 
                        end = c(year(date.fc.end), month(date.fc.end)), 
                        extend = TRUE) 

########## 3.3 Paths of Conditioned on Variables ####
window(data.fc[, "TARGET"], start = c(2023, 12), end = c(2025, 12))      <-
  c(rep(5, 3), rep(4.75, 6), rep(4.5, 6), rep(4.25, 6), rep(4, 4))

# Checking Imposed Values have the Right Scale and Timing
data.fc 

########## 3.4 Use Function for Iterative Forecast ####

fc <- forecast_conditional_var(mod.restrict, h, data.fc)


plot(fc, include = 32)


autoplot(fc, colour = FALSE,  showgap = TRUE)  +
  autolayer(data.fc, color = 'red')

autoplot(fc$forecast$TARGET, include = 32) + 
  autolayer(data.fc[,'TARGET'], color = 'magenta')

autoplot(fc$forecast$GDP, include = 32) + 
  autolayer(data.fc[,'GDP'], color = 'magenta')

autoplot(fc$forecast$INF, include = 32) + 
  autolayer(data.fc[,'INF'], color = 'magenta')

autoplot(fc$forecast$UNEMPLOYMENT, include = 32) + 
  autolayer(data.fc[,'UNEMPLOYMENT'], color = 'magenta')

autoplot(fc$forecast$WAGE, include = 32) + 
  autolayer(data.fc[,'WAGE'], color = 'magenta')

autoplot(fc$forecast$BREAKEVEN, include = 32) + 
  autolayer(data.fc[,'BREAKEVEN'], color = 'magenta')

fc$forecast 


##########################################################################

############################ 4. XLSX Export ##############################

write.xlsx(fc,"forecast data.xlsx")
