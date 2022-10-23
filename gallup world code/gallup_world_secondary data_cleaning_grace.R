# if(!require(installr)) {
#   install.packages("installr"); 
#   require(installr)
# } 
# updateR()
# 
# ## get packages installed
# packs = as.data.frame(installed.packages(.libPaths()[1]), stringsAsFactors = F)
# 
# ## and now re-install install packages using install.packages()
# install.packages(packs$Package)


### Data cleaning for Gallup World plots

setwd("C:/Users/yujia/OneDrive - The University of Chicago/Desktop/Research/gallup_world")

library(haven)
library(readr)
library(tidyverse)
# install.packages("countrycode")
library(countrycode)
# install.packages("lessR")
library(lessR)
library(labelled)


### Read in all of the data
#Change path files to connect to google drive
# gini <- read.csv("gini_copy.csv")
# gdp <- read.csv("gdp_copy.csv")
wv5 <- read_dta("wv5.dta")
wv6 <- read_sav("WV6.sav")
wv7 <- read_dta("wv7.dta")

hofstede <- read.csv("hofstede.csv")

country_level_models <- read_csv("country_level_models2.csv")

gallup_world <- read_rds("gallup_world_clean.rds")



### Clean GDP and GINI data and count unique countries
# old gdp and gini data were deleted; probably no need to clean anymore
# colnames(gini) <- gini[4,]
# gini <- gini[-c(1:4),] %>% select(-c(2:4)) %>% select(c(1,30:50))
# rownames(gini) <- NULL
# print(gini)
# 
# colnames(gdp) <- gdp[4,]
# gdp <- gdp[-c(1:4),] %>% select(-c(2:4)) %>% select(c(1,30:50))
# rownames(gdp) <- NULL
# print(gdp)
# 
# #rename country name column
# colnames(gini)[1] <- 'country'
# colnames(gdp)[1] <- 'country'
# 
# #write csv
# write_csv(gini, "gini_clean.csv")
# write_csv(gdp, "gdp_clean.csv")
# 
# print(gini)
# print(gdp)

gdp <- read.csv("gdp.csv")
gini <- read.csv("gini_index.csv")

#save country names to check against other datasets
gini_country <- gini$country %>% as.data.frame()
gdp_country <- gdp$country %>% as.data.frame()



### Count WVS wave 5, 6 and 7 unique countries

## convert country code into country name
#check how many unique country codes there are
length(unique(wv5$V2A)) #59 unique values

length(unique(wv6$COW))
length(unique(wv6$C_COW_ALPHA))
length(unique(wv6$B_COUNTRY_ALPHA))
length(unique(wv6$V2)) # all show 60 unique values

# length(unique(wv7$cntrycow))
# length(unique(wv7$cntry_AN))
# length(unique(wv7$cntry)) #81

length(unique(wv7$C_COW_ALPHA))
length(unique(wv7$B_COUNTRY_ALPHA))
length(unique(wv7$B_COUNTRY))

#There should be 60 countries in WVS wave 6 and 57 in wave 7. 59 countries in wave 5.
#If we want the survey to be from the same years as the gdp and gini values in gallup world: wave 5



### Clean WVS (wave 6 and 7 for now)
#I used the package countrycode to do the country abbreviation/code conversion. Only WVS wave 6 and 7 needed this.

## convert country code to country name
#for both waves: using COW code will produce the wrong results; iso-3 character/# gives the correct number of countries
wv6$country <- countrycode(sourcevar = wv6$B_COUNTRY_ALPHA, origin = 'iso3c', 
                           destination = 'country.name')
wv6 <- wv6 %>% select(country, everything()) #make 'country' the first column
unique(wv6$country) %>% length()


wv7$country <- countrycode(sourcevar = wv7$B_COUNTRY, origin = 'iso3n', 
                           destination = 'country.name')
wv7 <- wv7 %>% select(country, everything())
unique(wv7$country) %>% length()

## delete columns that we don't need
wv6
wv6 <- select(wv6, -c(2:8))
wv7
wv7 <- select(wv7, -c(2:36))

head(wv6)
head(wv7)

write_csv(wv6, "wv6_clean.csv")
write_csv(wv7, "wv7_clean.csv")

#save country names again
wv6_country <- unique(wv6$country) %>% as.data.frame()
colnames(wv6_country) <- "wv6"
wv7_country <- unique(wv7$country) %>% as.data.frame()
colnames(wv7_country) <- "wv7"



### Hofstede
#Looks neat, but should we join Canada and Canada French or leave them as separate?

hofstede <- select(hofstede, -c(1))
hofstede
write_csv(hofstede, "hofstede_clean.csv")

#select power distance
#hofstede_pdi <- select(hofstede, c(country,pdi))

#save country names
hofstede_country <- hofstede$country %>% as.data.frame()
colnames(hofstede_country) <- "hofstede"



### Country level models
head(country_level_models)

#save country names
country_level_models_country <- unique(country_level_models$country) %>% as.data.frame()
colnames(country_level_models_country) <- "country level models"
country_level_models_country



### Select useful variables for WVS (wave 7 for now)
gc()
#select variables that are important to social status and well-being
wv7_vars <- wv7 %>%
  select(country, c(Q48,Q50,Q51,Q52,Q53,Q56,Q70,Q71,Q106,Q109,Q110,Q112,Q150,Q164)) %>%
  dplyr::select(country, Q48,Q50,Q51,Q52,Q53,Q56,Q70,Q71,Q106,Q109,Q110,Q112,Q150,Q164) %>%
  filter_at(
    vars(
      country, Q48,Q50,Q51,Q52,Q53,Q56,Q70,Q71,Q106,Q109,Q110,Q112,Q150,Q164
    ),
    all_vars(!is.na(.))
  )



### Check country names of datasets
#check how many countries have the same names against wv7
mapply(function(x, y) sum(x %in% y), hofstede_country, country_level_models)



### Join datasets and filter NAs for final_df
#*Note: "Canada" does not include French speaking regions for Hofstede
gdp_gini <- left_join(gdp, gini, by = "country")

a <- left_join(country_level_models,hofstede)
b <- left_join(a,gdp_gini)

#summarize wv7 into one country per row
wv7_vars <- wv7_vars %>% group_by(country) %>% summarise_each(funs(mean))

#join b with wv7_vars
c <- left_join(b,wv7_vars)
c

final_df <- c

final_df
#need to check overlapping countries between country_level_models and WVS




#joining the other way around
# n <- left_join(wv7_vars,b)

#joining data this way deletes too many columns
# final_df <- final_df %>%
#   dplyr::select(everything()) %>%
#   filter_at(
#     vars(
#       everything()
#     ),
#     all_vars(!is.na(.))
#   )
#probably shouldn't filter out all the NAs here - lose too much
#correlation between comp_income and secondary var
#final col: number of countries we have data for