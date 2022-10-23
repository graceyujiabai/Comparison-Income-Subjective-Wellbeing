library(tidyverse)
library(haven)
gc()

setwd("C:/Users/yujia/OneDrive - The University of Chicago/Desktop/Research/gallup_us_daily/gb_code/raw_data")

#load data
usdaily.2008 <- read_sav("US_DAILY_2008_DATA.SAV")
usdaily.2009 <- read_sav("US_DAILY_2009_DATA.SAV")
usdaily.2010 <- read_sav("US_DAILY_2010_DATA.SAV")
usdaily.2011 <- read_sav("US_DAILY_2011_DATA.SAV")
usdaily.2012 <- read_sav("US_DAILY_2012_DATA.SAV")
usdaily.2013 <- read_sav("US_DAILY_2013_DATA.SAV")
usdaily.2014 <- read_sav("US_DAILY_2014_DATA.SAV")
usdaily.2015 <- read_sav("US_DAILY_2015_DATA.SAV")
usdaily.2016 <- read_sav("US_DAILY_2016_DATA.SAV")
usdaily.2017 <- read_sav("US_DAILY_2017_DATA.SAV")
