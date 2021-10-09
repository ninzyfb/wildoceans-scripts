# packages
library(raster)
library(stringr)
library(dplyr)
library(lubridate)

# data
sst = list.files(pattern = "^OSTIA_SST_5KM_", recursive = TRUE)

# create list of SST dates
# this will be to group the rasters to calculate monthly averages
sst_dates = list()
for(i in 1:length(sst)){
  # extract date in yearmonthday
  string1 = str_split(sst[i],pattern = "_")[[1]][11]
  string2 = str_split(string1, pattern = ".tif")[[1]][1]
  # set as Date formate
  date = as.Date(string2, format ="%Y%m%d")
  # add to list of dates
  # list stores dates as numeric, reproject using as.Date(as.numeric(13515), origin="1970-01-01")
  sst_dates[i] = date}

# remove uneccessary variable
rm(i,string1,string2,date)

# create dataframe of dates
sst_dates_df = do.call(rbind.data.frame, sst_dates)
rm(sst_dates)

# rename numeric date column
colnames(sst_dates_df) = "date_numeric"
# create actual date column
sst_dates_df$date_yyyymmdd = as.Date(as.numeric(sst_dates_df$date_numeric), origin="1970-01-01") 
# create month column
sst_dates_df$yearmonth = format(as.Date(sst_dates_df$date_yyyymmdd,format = "%Y-%m-%d"),"%Y%m")

# create dataframe with days in month
months = sst_dates_df %>%
  group_by(month = floor_date(date_yyyymmdd,"month"))%>%
  group_by(month,yearmonth)%>%
  summarize(daysinmonth = n())

months_vector = as.numeric(months$daysinmonth)
yearmonth_vector = months$yearmonth
ends = cumsum(months_vector)

# loop to calculate averaged monthly SST values
start = 1
for(i in 1:length(months_vector)){
  end = ends[i]
  # stack group of dates from start to end of month
  sst_stack = stack(sst[start:end])
  # calculate average
  sst_mean = calc(sst_stack, fun =  mean)
  # save raster
  writeRaster(sst_mean,paste("sst",yearmonth_vector[i],".tif", sep = ""))
  # this end index now becomes start of next index
  # +1 makes sure that the start is the first day of the next month
  start = end + 1
}
rm(ends,months_vector,yearmonth_vector)
