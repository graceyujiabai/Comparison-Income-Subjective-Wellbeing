munge_data <- function(df) {
  df <- 
    df %>% 
    mutate(
      subid = MOTHERLODE_ID,
      year = YEAR)
    
  df <- 
    df %>% 
    mutate(
      #employment
      WP46 = ifelse(year %in% c(2008:2009), WP46, NA),
      WP10200 = ifelse(year %in% c(2009:2017), WP10200, NA),
      WP10202 = ifelse(year %in% c(2009:2017), WP10202, NA),
      WP9081 = ifelse(year %in% c(2009), WP9081, NA),
      WP8859 = ifelse(year %in% c(2009), WP8859, NA),
      #purpose well-being
      HWB19 = ifelse(year %in% c(2014:2017), HWB19, NA),
      HWB1 = ifelse(year %in% c(2014:2017), HWB1, NA),
      HWB20 = ifelse(year %in% c(2014:2017), HWB20, NA),
      HWB11 = ifelse(year %in% c(2014:2017), HWB11, NA),
      HWB2 = ifelse(year %in% c(2014:2017), HWB2, NA),
      #community well-being
      HWB21 = ifelse(year %in% c(2014:2016), HWB21, NA),
      WP83 = ifelse(year %in% c(2014:2016), WP83, NA),
      HWB9 = ifelse(year %in% c(2014:2016), HWB9, NA),
      HWB22 = ifelse(year %in% c(2014:2016), HWB22, NA),
      HWB23 = ifelse(year %in% c(2014:2016), HWB23, NA),
      HWB18 = ifelse(year %in% c(2014:2016), HWB18, NA),
      HWB10 = ifelse(year %in% c(2014:2016), HWB10, NA),
      #financial well-being
      WP40 = ifelse(year %in% c(2014:2017), WP40, NA),
      HWB5 = ifelse(year %in% c(2014:2017), HWB5, NA),
      HWB6 = ifelse(year %in% c(2014:2017), HWB6, NA),
      M1 = ifelse(year %in% c(2014:2016), M1, NA),
      HWB17 = ifelse(year %in% c(2014:2017), HWB17, NA),
      #social well-being
      HWB14 = ifelse(year %in% c(2014:2017), HWB14, NA),
      HWB15 = ifelse(year %in% c(2014:2017), HWB15, NA),
      HWB3 = ifelse(year %in% c(2014:2017), HWB3, NA),
      HWB4 = ifelse(year %in% c(2014:2017), HWB4, NA),
      #other employment vars
      want_to_wrk = ifelse(year %in% c(2010:2016), WP10229, NA),
      hrs_wrk_wk = ifelse(year %in% c(2010:2016), WP10215, NA),
      looking_for_wrk = ifelse(year %in% c(2013:2016), WP10208, NA),
      wks_looking_for_wrk = ifelse(year %in% c(2013:2016), WP10983, NA)
      )
  #return(df)
  #I ran the above chunk separately first, and it worked for munge_data(gallup daily data_year.SAV)
  
  df <-
    df %>% 
    mutate(
      # Demographics
      income_summary = ifelse(INCOME_SUMMARY < 11, INCOME_SUMMARY, NA),
      raw_income = 
        case_when(
          income_summary == 1 ~ 360,
          income_summary == 2 ~ 3360,
          income_summary == 3 ~ 9000,
          income_summary == 4 ~ 18000,
          income_summary == 5 ~ 30000,
          income_summary == 6 ~ 42000,
          income_summary == 7 ~ 54000,
          income_summary == 8 ~ 75000,
          income_summary == 9 ~ 105000,
          income_summary == 10 ~ 120000,
        ),
      scale_income = 
        scale(case_when(
          income_summary == 1 ~ 360,
          income_summary == 2 ~ 3360,
          income_summary == 3 ~ 9000,
          income_summary == 4 ~ 18000,
          income_summary == 5 ~ 30000,
          income_summary == 6 ~ 42000,
          income_summary == 7 ~ 54000,
          income_summary == 8 ~ 75000,
          income_summary == 9 ~ 105000,
          income_summary == 10 ~ 120000,
        )) %>% as.numeric,
      
      education = ifelse(EDUCATION < 7, EDUCATION, NA),
      education_fac = as.factor(education),
      
      sex = ifelse(SC7 %in% c(1),0,1),
      
      age = ifelse(WP1220 < 100 & WP1220 != 0, WP1220, NA),
      scale_age = scale(ifelse(WP1220 < 100 & WP1220 != 0, WP1220, NA)) %>% as.numeric,
      age_dec = round((ifelse(WP1220 < 100 & WP1220 != 0, WP1220, NA))/10)*10, #round to nearest decade
      
      race = as.factor(RACE),
      
      children = H17,
      adults = D9,
      
      married = 
        as.factor(ifelse(WP1223 == 6 | WP1223 == 7, NA, WP1223)),
      
      employment10_fac = as.factor(ifelse(year >= 2010, as.factor(EMPLOYMENT2010), NA)),
      # 1 "Employed Full Time (Employer)"
      # 2 "Employed Full Time (Self)"
      # 3 "Employed Part Time, Do Not Want Full Time"
      # 4 "Unemployed"
      # 5 "Employed Part Time, Want Full Time"
      # NA "Not in Work Force".
      employment_all =
        as.factor(
          ifelse(
            year %in% c(2008:2009) & !is.na(WP46) & WP46 == 1,
            1,
            ifelse(
              year %in% c(2008:2009) & !is.na(WP46) & WP46 == 2,
              0,
              ifelse(
                year %in% c(2009:2017) & !is.na(WP10200) & WP10200 == 1,
                1,
                ifelse(
                  year %in% c(2009:2017) & !is.na(WP10200) & WP10200 == 2 & WP10202 == 1,
                  1,
                  ifelse(
                    year %in% c(2009:2017) & !is.na(WP10200) & WP10200 == 2 & WP10202 == 2,
                    0,
                    ifelse(
                      year == 2009 & !is.na(WP9081) & WP9081 == 1,
                      1,
                      ifelse(
                        year == 2009 & !is.na(WP9081) & WP9081 == 2,
                        0,
                        ifelse(
                          year == 2009 & !is.na(WP8859) & WP8859 == 1,
                          1,
                          ifelse(
                            year == 2009 & !is.na(WP8859) & WP8859 == 2,
                            0,
                            NA
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
      
      #Psychological
      std_living = WP30,
      econ = M30,
      comp_satis_std_liv = ifelse(year %in% c(2014:2017) & HWB17 %in% c(1:5), HWB17, NA),
      enough_money = ifelse(year %in% c(2014:2017) & HWB5 %in% c(1:5), HWB5, NA),
      goals = ifelse(year %in% c(2014:2017)  & HWB20 %in% c(1:5), HWB20, NA),
      little_pleasure = ifelse(year %in% c(2014:2017), H45, NA),
      active_prod = ifelse(year %in% c(2014:2017), HWB7, NA),
      #drugs_relax = ifelse(year %in% c(2014:2016), H46, NA),
      
      ladder_now = ifelse(year %in% c(2008:2017) & WP16 %in% c(1:10), WP16, ifelse(year %in% c(2018), LAD1, NA)),
      ladder_5yrs = ifelse(year %in% c(2008:2017) & WP18 %in% c(1:10), WP18, ifelse(year %in% c(2018), LAD2, NA)),
      cl_diff = ladder_5yrs - ladder_now,
      
      economy_getting_better = 4 - WP148,
      enjoyment = WP67,
      worry = WP69,
      #sadness = ifelse(year %in% c(2017, 2018), NA, WP70),
      stress = WP71,
      
      
      # Health behaviors  
      fruits_veggies = as.numeric(ifelse(H12B < 8, H12B, NA)),
      exercise = as.numeric(ifelse(H12A < 8, H12A, NA)),
      eat_healthy = as.numeric(M16 == 1, 1, ifelse(M16 == 2, 0, NA)),
      smoke = as.numeric(H11 == 1, 1, ifelse(H11 == 2, 0, NA)),
      #num_alc = ifelse(year %in% c(2014:2016), ALCO1, NA),
      
      
      # Health outcomes
      bmi = as.numeric(BMI),
      obese = as.factor(OBESE),
      general_health = H36,
      hbp = H4A,
      cholesterol = H4B,
      diabetes = H4C,
      depression = H4D,
      heart_attack = H4E,
      #asthma = H4F,
      cancer = H4G,
      height = HEIGHT,
      
      
      # Geography
      zipcode = as.factor(ZIPCODE),
      census_region = as.factor(ZIPCENSUSREGION),
      msa = as.factor(MSACODE),
      fips_code = as.character(ifelse(year %in% c(2008:2012), fips_code, as.character(FIPS_CODE))),
      COMB_WEIGHT = ifelse(year %in% c(2018), WB_WEIGHT, ifelse(year %in% c(2008:2017), COMB_WEIGHT, NA))
      
    )
  
  df <- mutate_at(df, 
                  vars(education, 
                       #gender, 
                       race,
                       sex,
                       
                           std_living,
                           econ,
                           comp_satis_std_liv ,
                           enough_money,
                           goals,
                           little_pleasure,
                           active_prod,
                       
                           enjoyment, worry, stress,
                       
                           eat_healthy, smoke,
                       
                           obese,
                           general_health,
                           hbp,
                           cholesterol,
                           diabetes,
                           depression,
                           heart_attack,
                           cancer),
                  as.factor)
  
  return(df)
}


