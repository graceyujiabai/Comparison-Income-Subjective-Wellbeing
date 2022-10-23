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
