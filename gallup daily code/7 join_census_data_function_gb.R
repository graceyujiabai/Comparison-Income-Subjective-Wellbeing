join_census_data <- function(df, year){
  
  year = ifelse(year < 2010, 2010, year)
  
  ## general census data ##
  
  census_data_path <- "census data/"
  
  census_county <-
    read_csv(paste0(census_data_path, "census_ses_county_wide_", year, ".csv")) %>% 
    dplyr::select(
      fips_code,
      median_income_county = median_income,
      median_monthly_housing_cost_county = median_monthly_housing_cost,
      median_home_value_county = median_home_value,
      # pov_status_below_per_county = pov_status_below_per,
      gini_county = gini,
      unweighted_pop_county = unweighted_population,
      total_pop_county = total_population
      # ea_bach_or_higher_per_county = ea_bach_or_higher_per
    )
  
  df <-
    df %>% 
    left_join(
      census_county,
      by = c("fips_code")
    )
  
  
  ## economic mobility ##
  
  # mobility <- 
  #   read_csv(paste0(machine_path, "research/projects/secondary_data/mobility/chetty_mobility.csv")) %>% 
  #   dplyr::select(
  #     fips_code = `County FIPS 2000`,
  #     mob25_all = `Hhold Inc. All Kids p25`,
  #     mob75_all = `Hhold Inc. All Kids p75`,
  #     mob25_ga = `Hhold Inc. Gend. Avg. p25`,
  #     mob75_ga = `Hhold Inc. Gend. Avg. p75`
  #   ) %>% 
  #   mutate(
  #     fips_code = as.character(ifelse(nchar(fips_code) == 4, paste0("0", fips_code), fips_code))
  #   )
  # 
  # df <-
  #   df %>% 
  #   left_join(
  #     mobility
  #   )
  # 
  
  ## Land Area ##
  
  county_land_area <- 
    read_csv(paste0("county_land_area.csv"))
  
  df <-
    df %>% 
    left_join(
      county_land_area %>% 
        dplyr::select(
          fips_code, 
          land_area_2010 
        ),
      by = "fips_code"
    )
  
  return(df)
}
