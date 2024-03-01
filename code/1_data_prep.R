library(tidyverse)
library(here)

setwd(here::here("code"))
raw_data <- read_csv(here::here("data", "CRLF_EGG_RAWDATA.csv"))

#checking type of each column, remove NA's
str(raw_data,na.rm = TRUE)

#other cleaning to be done:
# --> get months as ints from date
# --> remove unnecessary columns once we've all agreed on it