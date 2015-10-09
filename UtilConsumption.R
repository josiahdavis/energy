
# Load packages -----------------------------------------------------------
# install.packages("lubridate")
# install.packages("tidyr")
# install.packages("xlsx")

loadP <- c("lubridate","dplyr","tidyr","ggplot2","xlsx")
lapply(loadP, library, character.only = TRUE)

# Load Sample Data --------------------------------------------------------
# meter sample data
meterSample <- read.csv("smallelectricitytimeofusedataset.csv", header = TRUE, check.names = TRUE, 
                        strip.white = TRUE,stringsAsFactors = FALSE)

# customer infomation sample data
customerData <- read.csv("SGSC-CTCustomer-Household-Data.csv", header = TRUE)

# offer response data
customerOffers <- read.csv("SGSC-CTOffers-and-Acceptances.csv", header = TRUE)

# daily meter reading
dailyReads <- read.csv("energyWide.csv", header = TRUE)

## load historic weather data Sydney (Observatory Hill)
# 66062 Lat: 33.86° SLon: 151.21° EElevation: 39 m

tempHistoricMax <- read.csv("IDCJAC0010_066062_1800_Data.csv")
tempHistoricMin <- read.csv("IDCJAC0011_066062_1800_Data.csv")

# Clean Sample Data -------------------------------------------------------

# meter sample data
meterSample$FullTime <- strptime(meterSample$End.Datetime, format = "%d/%m/%Y %H:%M")
as.Date(meterSample$End.Datetime, format = "%d/%m/%Y")
meterSample$Year <- year(meterSample$FullTime)
meterSample$Year <- ifelse(nchar(meterSample$Year) == 2, paste("20",year(meterSample$FullTime), sep=""),year(meterSample$FullTime))
meterSample$Month <- month(meterSample$FullTime)
meterSample$Day <- day(meterSample$FullTime)

meterSample$CUSTOMER_KEY <- factor(meterSample$CUSTOMER_KEY) # change customer key to factor

meterGroup <- meterSample %>% 
  select(-FullTime) %>% # drop time data
  group_by(CUSTOMER_KEY, Year, Month) %>% 
  select(CUSTOMER_KEY, Month, General.Supply.KWH:Net.Generation.KWH) %>% 
  summarise(n = n())

list(unique(meterSample$CUSTOMER_KEY)) # find unique customer keys

customerDataSub <- customerData[customerData$CUSTOMER_KEY %in% meterSample$CUSTOMER_KEY,] #subset on customers in meter samples

customerDataSubRank  <-  customerDataSub %>% # create groupwise summary of customer data
  group_by(TRIAL_REGION_NAME) %>% # find most common 
  summarize(count = n()) %>% # summarize
  arrange(desc(count))


## clean historic weather data Sydney (Observatory Hill)

tempHistoricMax <- read.csv("IDCJAC0010_066062_1800_Data.csv")
tempHistoricMin <- read.csv("IDCJAC0011_066062_1800_Data.csv")

tempHistoricCombine <- tempHistoricMax %>% 
  left_join(tempHistoricMin,by = c("Year","Month","Day")) %>% # join on YMD
  filter(Year >= 2012) %>% # filter greater than 2012
  select(stationNumber = Bureau.of.Meteorology.station.number.x,Year, Month, Day, Maximum.temperature..Degree.C., Minimum.temperature..Degree.C.) %>%
  mutate(MaxTempF = round(Maximum.temperature..Degree.C.*(9/5) + 32,digits = 0), # convert to F
         MinTempF = round(Minimum.temperature..Degree.C.*(9/5) + 32,digits = 0)) %>% # convert to F
  mutate(baseCDD = 80, # constant for CDD
         baseHDD = 60) %>% # constant for HDD
  mutate(CDD = ifelse(MaxTempF <= baseCDD, 0, # calculate CDD
                       ifelse(MaxTempF > baseCDD & MinTempF <= baseCDD,
                              (MaxTempF - baseCDD) / (2*(MaxTempF - MinTempF)),
                              (MaxTempF + MinTempF) / (2)-baseCDD))) %>%
  mutate(HDD = ifelse(MinTempF >= baseHDD, 0, # calculate HDD
                      ifelse(MinTempF < baseHDD & MaxTempF >= baseHDD,
                             (baseHDD - MinTempF) / (2*(MaxTempF - MinTempF)),
                             (baseHDD - (MaxTempF + MinTempF)/2)))) %>% 
  select(stationNumber, Year, Month, Day, MaxTempF, MinTempF, CDD, HDD) # select columns of interest

# Mutate -------------------------------------------------------

# sydney observatory data (min and max temperature)
tempHistoricSub <- tempHistoricCombine %>% 
  mutate(month_Year = paste(Month, Year, sep = "-")) %>% # create new variables
  group_by(month_Year) %>% # group by year and month year dummy
  summarise(max_Monthly = max(MaxTempF), # find highest temp per month
            min_Monthly = min(MinTempF), # find lowest temp per month
            hdd = sum(HDD), # sum hdd
            cdd = sum(CDD), # sum cdd
            median_temp = median(c(MaxTempF, MinTempF)), #median daily temp
            median_High = median(MaxTempF), # median high
            median_Low = round(median(MinTempF),0), # median low
            measured_Days = n(), # measured days per month
            days_80 = sum(MaxTempF > 80), # days max temp was above 80
            days_65 = sum(MinTempF < 65)) %>% # days max temp was below 65
  arrange(month_Year)
head(tempHistoricSub)


# list(unique(meterSample$CUSTOMER_KEY)) # 30 unique households
# dim(meterSample[meterSample$CUSTOMER_KEY == "8170837",] ) # 15977 rows
# dim(meterSample) #618189 rows

# summarize sample meter and combine with sydney observatory data 
meterSampleSub <- meterSample %>% 
  select(CUSTOMER_KEY, Month, Year, General.Supply.KWH:Net.Generation.KWH) %>% 
  mutate(month_Year = paste(Month, Year, sep = "-")) %>% # create new variables
  group_by(CUSTOMER_KEY, Year, month_Year) %>% 
  summarize(general_KWH = sum(General.Supply.KWH),
            offPeak_KWH = sum(Off.Peak.KWH),
            grossGen_KWH = sum(Gross.Generation.KWH),
            netGen_KWH = sum(Net.Generation.KWH),
            total_KWH = general_KWH+offPeak_KWH+grossGen_KWH+netGen_KWH) %>% 
  left_join(tempHistoricSub, by = "month_Year")# join with temperature data

# Export to CSV -----------------------------------------------------------

write.csv(x = meterSampleSub, file = "data.csv",row.names = FALSE)
