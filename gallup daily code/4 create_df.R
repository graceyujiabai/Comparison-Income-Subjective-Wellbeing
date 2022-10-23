#This is a file to generate cleaned gallup daily datasets in separate steps
#Not a function

library(tidyverse)
library(haven)
library(labelled)

daily.2008 <- munge_data(usdaily.2008)
daily.2009 <- munge_data(usdaily.2009)
daily.2010 <- munge_data(usdaily.2010)
daily.2011 <- munge_data(usdaily.2011)
daily.2012 <- munge_data(usdaily.2012)
daily.2013 <- munge_data(usdaily.2013)
daily.2014 <- munge_data(usdaily.2014)
daily.2015 <- munge_data(usdaily.2015)
daily.2016 <- munge_data(usdaily.2016)
daily.2017 <- munge_data(usdaily.2017) #how to make this process more efficient?


#select_variable_gb

#create dataframes with desired variables
#make a list of dataframes with all of the data
ls <- list(daily.2008,
     daily.2009,
     daily.2010,
     daily.2011,
     daily.2012,
     daily.2013,
     daily.2014,
     daily.2015,
     daily.2016,
     daily.2017)

df <- list()

#select variables for all years of data with for loop
for (i in seq_along(ls)){
  df[[i]] <- select_variable(ls[[i]])
}

#create new dataframes
gd.2008 <- df[[1]]
gd.2009 <- df[[2]]
gd.2010 <- df[[3]]
gd.2011 <- df[[4]]
gd.2012 <- df[[5]]
gd.2013 <- df[[6]]
gd.2014 <- df[[7]]
gd.2015 <- df[[8]]
gd.2016 <- df[[9]]
gd.2017 <- df[[10]]

#write rds
gd.2008 <- df[[1]] %>% write_rds("gd_2008.rds")
gd.2009 <- df[[2]] %>% write_rds("gd_2009.rds")
gd.2010 <- df[[3]] %>% write_rds("gd_2010.rds")
gd.2011 <- df[[4]] %>% write_rds("gd_2011.rds")
gd.2012 <- df[[5]] %>% write_rds("gd_2012.rds")
gd.2013 <- df[[6]] %>% write_rds("gd_2013.rds")
gd.2014 <- df[[7]] %>% write_rds("gd_2014.rds")
gd.2015 <- df[[8]] %>% write_rds("gd_2015.rds")
gd.2016 <- df[[9]] %>% write_rds("gd_2016.rds")
gd.2017 <- df[[10]] %>% write_rds("gd_2017.rds")

#combine all data
gd.full <- bind_rows(gd.2008,
                 gd.2009,
                 gd.2010,
                 gd.2011,
                 gd.2012,
                 gd.2013,
                 gd.2014,
                 gd.2015,
                 gd.2016,
                 gd.2017)

#remove labels
gd.full <- remove_labels(gd.full)


#write data
write_rds(gd.full, "gd_full.rds")
write_csv(gd.full, "gd_full.csv")
