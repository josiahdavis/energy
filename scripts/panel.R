# ========================
# PANEL DATA ANALYSIS
# ========================

# Clear working space and load packages
rm(list = ls()); gc()
library(plm)

# Read in the data
fileLoc <- "/Users/josiahdavis/Documents/GitHub/energy/"
d <-read.csv(paste(fileLoc, "data.csv", sep="")) 
d <- d[,c("CUSTOMER_KEY", "month_Year", "general_KWH", "hdd", "cdd")]
names(d) <- c("customer", "time", "general", "hdd", "cdd")


# Format variables and subset time interval
d$time <- as.Date(paste0("01-", d$time), "%d-%m-%Y")
d <- filter(d, time >= as.Date("2012-05-01"), 
            time < as.Date("2014-03-01"))

str(d)
summary(d)

# Set up the panel data
formula <- general ~ hdd + cdd
pdata <- plm.data(d, index=c("customer", "time"))

# Pooled OLS estimator
pooling <- plm(formula, data=pdata, model= "pooling")
summary(pooling)

# Between estimator
between <- plm(formula, data=pdata, model= "between")
summary(between)

# First differences estimator
firstdiff <- plm(formula, data=pdata, model= "fd")
summary(firstdiff)

# Fixed effects or within estimator
fixed <- plm(formula, data=pdata, model= "within")
summary(fixed)

# Random effects estimator
random <- plm(formula, data=pdata, model= "random")
summary(random)

# Lagrange Multiplier test for random effects versus OLS
plmtest(pooling)

# Lagrange Multiplier test for fixed effects versus OLS
pFtest(fixed, pooling)

# Hausman test for fixed versus random effects model
# Use the Fixed Effects model when p < 0.01
phtest(random, fixed)
