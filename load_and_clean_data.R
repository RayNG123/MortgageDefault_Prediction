library(tidyverse)
##read in data
#read in mortgage data set and assign with appropriate column types
dcrfull <- read_csv(here::here('dataset/dcr_full.csv'), col_types=cols(
  id = col_double(),
  time = col_double(),
  orig_time = col_double(),
  first_time = col_double(),
  mat_time = col_double(),
  res_time = col_double(),
  balance_time = col_double(),
  LTV_time = col_double(),
  interest_rate_time = col_double(),
  rate_time = col_double(),
  hpi_time = col_double(),
  gdp_time = col_double(),
  uer_time = col_double(),
  REtype_CO_orig_time = col_double(),
  iREtype_PU_orig_time = col_double(),
  REtype_SF_orig_time = col_double(),
  investor_orig_time = col_double(),
  balance_orig_time = col_double(),
  FICO_orig_time = col_double(),
  LTV_orig_time = col_double(),
  Interest_Rate_orig_time = col_double(),
  state_orig_time = col_character(),
  hpi_orig_time = col_double(),
  default_time = col_double(),
  payoff_time = col_double(),
  status_time = col_character()
))
#filter out empty values for state_orig_time
dcrfull1 <- dcrfull %>% filter(!is.na(state_orig_time))
#add year/quarter/month/day
date_seq<-seq(as.Date("2000/03/31"), by = "quarter", length.out = 60)
date_dataframe<-data.frame(date_seq)
date_dataframe<- date_dataframe%>%separate(date_seq,into=c('year','month','day'),sep='-')
date_dataframe$quater=1
date_dataframe$quater[date_dataframe$month=='07']<-2
date_dataframe$quater[date_dataframe$month=='10']<-3
date_dataframe$quater[date_dataframe$month=='12']<-4
date_dataframe$time<-seq(1,60,1)
dcrfull2 <- dcrfull1 %>% left_join(date_dataframe, by = "time")

mortgage <- dcrfull2


#Then, we notice that since we have a panel dataset--multiple records refer to a same mortgage over different period. We would not know whether a mortgage is default or payoff until we see last record of this mortgage.  Therefore, it is a little bit complicated for us to build a logistic linear regression. We then decided to normalized the dataset again and try to collapse them to build a dataset with have only one record for one mortgage, including their start time, end time, their age,avergae GDP during the mortgage, status at last(default or pay off) and etc.


#create a new column for the age of a mortgage
mortgage$age<- mortgage$time-mortgage$orig_time
#create a new column for the time to maturity of a mortgage
mortgage$TTM<-mortgage$mat_time-mortgage$orig_time
#create a continuous time variable for each records
mortgage$date=paste(as.character(mortgage$year),as.character(mortgage$month),as.character(mortgage$day),sep='-')
#found out the first-time record for all mortgages
mortgage_first<-mortgage[match(unique(mortgage$id), mortgage$id),]
#create a column for start time of each mortgage
mortgage_first$first_time_date<-mortgage_first$date
mortgage_first$first_time<-mortgage_first$time
#leave only the collumn that contains first time data of the mortgage
first <- mortgage_first %>% select(id, first_time, orig_time,mat_time,rate_time,REtype_CO_orig_time,REtype_PU_orig_time,REtype_SF_orig_time,investor_orig_time,balance_orig_time:Interest_Rate_orig_time,state_orig_time,hpi_orig_time,first_time_date, age, TTM)


#As followed, we would like to determine the ending status of each mortgage (default, pay off or unfnished):

#Get the last record for all mortgages in the dataset
get_last <- mortgage %>% select(id,date,time,default_time,payoff_time,status_time) %>% group_by(id) %>% summarise_all(last)
#determine the end time of a mortgage
get_last$last_time_date<-get_last$date
get_last$last_time<-get_last$time
#determine the status of the mortgage
get_last$status_last<-get_last$status_time
atlast <- get_last %>% select(id,last_time_date:status_last,default_time)


#As followed, we would like to get the mean value of GDP growth, Unemployment Rate, Risk_free rate, interest rate, house price index for each mortgage from their start to their end.

meanvalue <- mortgage %>% group_by(id) %>% summarise(interest_rate_mean = mean(interest_rate_time),gdp_mean = mean(gdp_time), risk_free_mean = mean(rate_time),hpi_mean = mean(hpi_time),uer_mean = mean(uer_time))

#Now We would like to join the three dataset together to form the new dataset with one record for one mortgage:

mortgage_all <- first %>% left_join(atlast, by = 'id') %>% left_join(meanvalue, by = 'id')


#We would like to add one more variable, time between the start of the mortgage and global financial crisis(time 37) to measure the influence of Financial Crisis to the mortgage

mortgage_all$time_to_GFC <- 37-mortgage_all$first_time



#Now, we will need to filter out mortgages that are unfinished.

mortgage_all <- mortgage_all %>% filter(status_last != 0)

#Combine dataset: S&P Index:

SP <- read_csv(here::here('dataset/Combine Dataset/S&P .csv'))
SP <- SP %>% mutate(MeanSP = (High + Low)/2) %>% select(Date,Volume,MeanSP) 
SP$Date <- as.character(SP$Date)
SP <- SP %>% filter(MeanSP != 1414.070)
SP <- SP %>% separate(Date,into=c('year','month','day'),sep='-') %>% arrange(year,month)
SP$time<- rep(1:60,each = 3)
SP <- SP %>% group_by(time) %>% summarise(mean_sp = mean(MeanSP),mean_volume = mean(Volume)) 


#Combine dataset: Dow Jones Real Estate Index:
Index <- read_csv(here::here('dataset/Combine Dataset/DJ Real Estate Data.csv')) 
Index <- Index %>% separate(Date,into=c('year','month','day'),sep='/') %>% filter(year != 2015 & Price != 172.76) 
Index$time <- rep(50:1,each = 3)
Index <- Index %>% arrange(time)
Index$time <- rep(1:50,each = 3)
Index$time <- Index$time + 10
Index <- Index %>% group_by(time) %>% summarise(mean_index = mean(Price))

#Combine dataset-US housing market: New Construction, House Sales, House Supply, House Inventory

Start <- read_csv(here::here('dataset/Combine Dataset/House start.csv')) %>% separate(Period,into=c('year','month','day'),sep='-') 
names(Start)[4] = c('house_start')
Start <- Start %>% filter(year > 1999 & year < 2015) %>% arrange(year,month)
Start$time <- rep(1:60,each = 3)
Start <- Start %>% group_by(time) %>% summarise(house_start = mean(house_start))

#Combine dataset: New House Sales:
Sales <- read_csv(here::here('dataset/Combine Dataset/Sales.csv'))  %>% separate(Period,into=c('year','month','day'),sep='-') 
names(Sales)[4] = c('Sales')
Sales <- Sales %>% filter(year > 1999 & year < 2015) %>% arrange(year,month)
Sales$time <- rep(1:60,each = 3)
Sales <- Sales %>% group_by(time) %>% summarise(Mean_sale = mean(Sales))

#Combine dataset: House Supply:
Supply <- read_csv(here::here('dataset/Combine Dataset/Supply.csv')) %>% separate(Period,into=c('year','month','day'),sep='-') 
names(Supply)[4] = c('Supply')
Supply <- Supply %>% filter(year > 1999 & year < 2015) %>% arrange(year,month)
Supply$time <- rep(1:60,each = 3)
Supply <- Supply %>% group_by(time) %>% summarise(Mean_supply = mean(Supply))

#Combine dataset: House Inventory:
Inventory <- read_csv(here::here('dataset/Combine Dataset/House Inventory.csv')) %>% separate(Period,into=c('year','month','day'),sep='-') 
names(Inventory)[4] = c('Inventory')
Inventory <- Inventory %>% filter(year > 1999 & year < 2015) %>% arrange(year,month)
Inventory$time <- rep(1:60,each = 3)
Inventory <- Inventory %>% group_by(time) %>% summarise(Mean_invenotry = mean(Inventory))

#Combine All Combine dataset
Housing_Market <- Start %>% left_join(Sales, by = 'time') %>% left_join(Supply, by = 'time') %>% left_join(Inventory, by = 'time')
names(Housing_Market) = c('time','Start','Sale','Supply','Inventory')
Housing_Market


#Combine dataset：RMBS Insurance：

Insurance <- read_csv(here::here('dataset/Combine Dataset/RMBS insurance.csv'))  
names(Insurance)[1] = c('time')
names(Insurance)[6] = c('agency_mortgage_related_securities')
names(Insurance)[7] = c('non_agency_mortgage_related securities')
RMBS_Issue <- Insurance %>% filter(time > 1999 & time < 2015)
RMBS_Issue




