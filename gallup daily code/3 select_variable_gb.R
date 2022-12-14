select_variable <- function(df){
  df <- data.frame(df %>% 
                     select("subid",
                            "year",
                            "income_summary",
                            "scale_income",
                            "raw_income",
                            "education",
                            "sex",
                            "age",
                            "scale_age",
                            "age_dec",
                            "race",
                            "employment_all",
                            "employment10_fac",
                            "married",
                            "children",
                            "adults",
                            "std_living",
                            "econ",
                            "general_health",
                            "HWB17", #financial well-being
                            "HWB5", #financial well-being
                            "HWB20", #purpose well-being
                            "little_pleasure",
                            "active_prod",
                            "ladder_now",
                            "ladder_5yrs",
                            "WP148", #economy getting better: should be derived
                            "enjoyment",
                            "worry",
                            "stress",
                            "fruits_veggies",
                            "exercise",
                            "eat_healthy",
                            "smoke",
                            "bmi",
                            "obese",
                            "hbp",
                            "cholesterol",
                            "diabetes",
                            "depression",
                            "heart_attack",
                            "cancer",
                            "height",
                            "want_to_wrk",
                            "hrs_wrk_wk",
                            "looking_for_wrk",
                            "wks_looking_for_wrk",
                            "COMB_WEIGHT",
                            "zipcode",
                            "census_region",
                            "msa",
                            "fips_code"))
  return(df)
}
