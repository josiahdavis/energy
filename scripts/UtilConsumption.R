
# Load packages -----------------------------------------------------------
# install.packages("lubridate")
# install.packages("tidyr")
# install.packages("xlsx")

loadP <- c("lubridate","dplyr","tidyr","ggplot2")
lapply(loadP, library, character.only = TRUE)

# Load Sample Data --------------------------------------------------------
# meter sample data
meterSample <- read.csv("smallelectricitytimeofusedataset.csv", header = TRUE, check.names = TRUE, 
                        strip.white = TRUE,stringsAsFactors = FALSE)

## load historic weather data Sydney (Observatory Hill)
# 66062 Lat: 33.86° SLon: 151.21° EElevation: 39 m

tempHistoricMax <- read.csv("IDCJAC0010_066062_1800_Data.csv")
tempHistoricMin <- read.csv("IDCJAC0011_066062_1800_Data.csv")

# customer infomation sample data
customerData <- read.csv("SGSC-CTCustomer-Household-Data.csv", header = TRUE)

# offer response data
customerOffers <- read.csv("SGSC-CTOffers-and-Acceptances.csv", header = TRUE)

# Clean Sample Data -------------------------------------------------------

# meter sample data
meterSample$FullTime <- strptime(meterSample$End.Datetime, format = "%d/%m/%Y %H:%M") # change to correct class
# meterSample$End.Datetime <- as.Date(meterSample$End.Datetime, format = "%d/%m/%Y")
meterSample$Year <- year(meterSample$FullTime) # generate year
meterSample$Year <- ifelse(nchar(meterSample$Year) == 2, paste("20",year(meterSample$FullTime), sep=""),year(meterSample$FullTime)) # make all years YYYY
meterSample$Month <- month(meterSample$FullTime) # generate month as numeric
meterSample$MonthDay <- day(meterSample$FullTime) # generate day of month as numeric (1 to 31)
meterSample$WeekDay <- wday(meterSample$FullTime) # day of the week as numeric (1 to 7)
meterSample$WeekNum <- week(meterSample$FullTime) # week of the year as numeric (1 to 53)
# meterSample$HMS <- hms(meterSample$FullTime, quiet = TRUE) # hms
meterSample$CUSTOMER_KEY <- factor(meterSample$CUSTOMER_KEY) # change customer key to factor

# single line for substring dates - depricate for now 
# dateSep <- separate(meterSample,End.Datetime, c("d","m","y","H","M")

meterGroupMonth <- meterSample %>% 
  select(-FullTime) %>% # drop time data
  group_by(CUSTOMER_KEY, Year, Month) %>% # group by month
  select(CUSTOMER_KEY, Month, General.Supply.KWH:Net.Generation.KWH) %>% 
  summarise(n = n())

meterGroupDay <- meterSample %>% 
  select(-FullTime) %>% # drop time data
  group_by(CUSTOMER_KEY, Year, Month, DayMonth) %>% # group by day of month (1-31)
  select(CUSTOMER_KEY, Month, General.Supply.KWH:Net.Generation.KWH) %>% 
  summarise(n = n())

# list(unique(meterSample$CUSTOMER_KEY)) # find unique customer keys

# customerDataSub <- customerData[customerData$CUSTOMER_KEY %in% meterSample$CUSTOMER_KEY,] #subset on customers in meter samples
# 
# customerDataSubRank  <-  customerDataSub %>% # create groupwise summary of customer data
#   group_by(TRIAL_REGION_NAME) %>% # find most common 
#   summarize(count = n()) %>% # summarize
#   arrange(desc(count))


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
  mutate(yearMonth = paste(Year, Month, sep = "-")) %>% 
  mutate(yearMonthDay = mutate(yearMonth = paste(Year, Month, Day, sep = "-"))) %>% # create new variables) %>% # create new variables
  select(stationNumber, Year, Month, Day, yearMonth, yearMonthDay, MaxTempF, MinTempF, CDD, HDD) # select columns of interest

# Mutate -------------------------------------------------------

# sydney observatory data (min and max temperature) monthly
tempHistoricSubMonth <- tempHistoricCombine %>% 
  mutate(yearMonth = paste(Year, Month, sep = "-")) %>% # create new variables
  group_by(yearMonth) %>% # group by year and month year dummy
  summarise(max_Monthly = max(MaxTempF, na.rm = TRUE), # find highest temp per month
            min_Monthly = min(MinTempF, na.rm = TRUE), # find lowest temp per month
            hdd = sum(HDD), # sum hdd
            cdd = sum(CDD), # sum cdd
            median_temp = median(c(MaxTempF, MinTempF), na.rm = TRUE), #median daily temp
            median_High = median(MaxTempF, na.rm = TRUE), # median high
            median_Low = round(median(MinTempF),0), # median low
            measured_Days = n(), # measured days per month
            days_80 = sum(MaxTempF > 80), # days max temp was above 80
            days_65 = sum(MinTempF < 65)) %>% # days max temp was below 65
  arrange(yearMonth)
head(tempHistoricSub$yearMonthDay)

# sydney observatory data (min and max temperature) daily
tempHistoricSubDay <- tempHistoricCombine %>% 
  group_by(yearMonthDay) %>% # group by year and month year dummy
  summarise(max_Daily = max(MaxTempF, na.rm = TRUE), # find highest temp per month
            min_Daily = min(MinTempF, na.rm = TRUE), # find lowest temp per month
            hdd = sum(HDD), # sum hdd
            cdd = sum(CDD), # sum cdd
            median_temp = median(c(MaxTempF, MinTempF), na.rm = TRUE), #median daily temp
            median_High = median(MaxTempF, na.rm = TRUE), # median high
            median_Low = round(median(MinTempF),0), # median low
            measured_Days = n(), # measured days per month
            days_80 = sum(MaxTempF > 80), # days max temp was above 80
            days_65 = sum(MinTempF < 65)) %>% # days max temp was below 65
  arrange(yearMonthDay)
summary(tempHistoricSubDay)

# list(unique(meterSample$CUSTOMER_KEY)) # 30 unique households
# dim(meterSample[meterSample$CUSTOMER_KEY == "8170837",] ) # 15977 rows
# dim(meterSample) #618189 rows

# summarize sample meter and combine with sydney observatory data - monthly
meterSampleSub <- meterSample %>% 
  select(CUSTOMER_KEY, Month, Year, General.Supply.KWH:Net.Generation.KWH) %>% 
  filter(!is.na(CUSTOMER_KEY)) %>% # remove NA values
  mutate(yearMonth = paste(Year, Month, sep = "-")) %>% # create new variables
  group_by(CUSTOMER_KEY, Year, yearMonth) %>% 
  summarize(general_KWH = sum(General.Supply.KWH),
            offPeak_KWH = sum(Off.Peak.KWH),
            grossGen_KWH = sum(Gross.Generation.KWH),
            netGen_KWH = sum(Net.Generation.KWH),
            total_KWH = general_KWH+offPeak_KWH+grossGen_KWH+netGen_KWH) %>% 
  left_join(tempHistoricSubMonth, by = "yearMonth")# join with temperature data

# summarize sample meter and combine with sydney observatory data - daily
meterSampleSubDay <- meterSample %>% 
  select(CUSTOMER_KEY, Month, Year, MonthDay, General.Supply.KWH:Net.Generation.KWH) %>% 
  filter(!is.na(CUSTOMER_KEY)) %>% # remove NA values
  mutate(yearMonthDay = paste(Year, Month, MonthDay, sep = "-")) %>% # create new variables
  group_by(CUSTOMER_KEY, Year, Month, MonthDay, yearMonthDay) %>% 
  summarize(general_KWH = sum(General.Supply.KWH),
            offPeak_KWH = sum(Off.Peak.KWH),
            grossGen_KWH = sum(Gross.Generation.KWH),
            netGen_KWH = sum(Net.Generation.KWH),
            total_KWH = general_KWH+offPeak_KWH+grossGen_KWH+netGen_KWH) %>% 
  left_join(tempHistoricSubDay, by = "yearMonthDay") %>% # join with temperature data
  arrange(Month)
head(meterSampleSubDay)

# Export to CSV -----------------------------------------------------------
# monthly data
write.csv(x = meterSampleSub, file = "data.csv",row.names = FALSE)

# daily data
write.csv(x = meterSampleSubDay, file = "dataDaily.csv", row.names = FALSE)

getwd()
table(is.na(meterSampleSub))
tempHistoricSub[tempHistoricSub$hdd != 0,]
meterSampleSub[is.na(meterSampleSub),]
