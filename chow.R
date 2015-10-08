# Import packages
library(dplyr)
library(ggplot2)

# Read in the data
fileLoc <- "C:/Users/josiahd/Documents/PGandE/"
d <-read.csv(paste(fileLoc, "electricity.csv", sep="")) 
names(d) <- c("customer", "time", "general", "offPeak", "grossGen", "netGen")
str(d)

# Format and add variables
d$time <- as.Date(d$time, "%d/%m/%Y %H:%M")
d$customer <- as.factor(d$customer)
d$month <- as.Date(
              paste0("01-",strftime(d$time, format="%m-%Y")), 
              "%d-%m-%Y")
  
# Calculate the total daily usage for customer on each day
group <- group_by(d, customer, month)
dm <- summarise(group, gen = sum(general))
dm <- filter(dm, month > as.Date("2013-03-01"), 
                  month < as.Date("2014-03-01"))

# Plot the data with the average for each month
ggplot(dm, aes(month, gen)) +
  geom_point(alpha = 1/4) +
  geom_smooth() +
  scale_size_area() + 
  scale_x_date(breaks = seq.Date(min(dm$month), max(dm$month), "month"))

# Choose a breaking point for the chow test (arbitrary for now)
breakPoint <- as.Date("2013-09-01")

## Create a linear regression
mc = lm(gen ~ month, data = dm)
m1 = lm(gen ~ month, data = dm[dm$month <= breakPoint,])
m2 = lm(gen ~ month, data = dm[dm$month > breakPoint,])

# Calculate the chow test statistic
# Reference: https://en.wikipedia.org/wiki/Chow_test
SC <- sum(mc$residuals^2)
S1 <- sum(m1$residuals^2)
S2 <- sum(m2$residuals^2)
k <- mc$rank
N1 <- length(m1$residuals)
N2 <- length(m2$residuals)
chow <- ((SC - (S1 + S2)) / k) / ((S1 + S2) / (N1 + N2 - 2 * k))
chow

# Check for significance
Fcrit <- qf(.99, k, N1 + N2 - 2*k)
chow > Fcrit 
1 - pf(chow, k, N1 + N2 - 2*k)

# Visualize lines of best fit
ggplot(dm, aes(month, gen)) +
  geom_point(alpha = 1/4) +
  scale_size_area() + 
  stat_smooth(method="lm", se=FALSE, color = "#1f77b4") + 
  stat_smooth(data = dm[dm$month <= breakPoint,], method="lm", se=TRUE, color = "#ff7f0e") + 
  stat_smooth(data = dm[dm$month > breakPoint,], method="lm", se=TRUE, color = "#2ca02c") + 
  scale_x_date(breaks = seq.Date(min(dm$month), max(dm$month), "quarter")) + 
  scale_y_continuous(limits = c(-0, 1500))