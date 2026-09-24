
#***********************KEMRI- Wellcome Trust********************************************************************************************
#************************Health Service Unit*********************************************************************************************
#********Limit Validation Records to Seven (7) Days From Date Today**********************************************************************

data = cinObj$.__cache$raw_records
#This lines limits records from 2015-10-01
#---------------------------------------------------------------------------------------------------------------------------------------
#data = data[is.na(data$date_today) | str_trim(data$date_today) == "" | as.Date(data$date_today) >= as.Date("2016-01-01"), ] 
#Limitting records to today date minus 7
#---------------------------------------------------------------------------------------------------------------------------------------
data = data[is.na(data$date_today) | str_trim(data$date_today) == "" | as.Date(data$date_today) >= as.Date(as.POSIXlt(Sys.time()))-7, ]

#data = data[is.na(data$date_today) | str_trim(data$date_today) == "" | as.Date(data$date_today) == (as.Date(as.POSIXlt(Sys.time())) - as.difftime(5, unit="days")),]

cinObj$.__cache$raw_records = data
rm(data)
####ENJOY ................................................................................................................................