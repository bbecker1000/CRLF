library(tidyverse)
raw_data <- read_csv("CRLF_EGG_RAWDATA.csv")

#checking type of each column
str(raw_data)

#many ints in Survey_MONTH are empty but the values in Date look to be full. need to get month from Date

#maybe we should have survey length, width, water depth, etc as 1 standardized value for each pond per breeding year? Not sure how we want to do that

#test if my push works