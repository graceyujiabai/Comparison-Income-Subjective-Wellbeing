#Gallup Daily dempgraphic income median prediction

calculate_median_income_sar <- function(df){
  med_inc_gallup <-
    df %>% 
    dplyr::select(
      raw_income,
      age,
      sex,
      race,
      COMB_WEIGHT
    ) %>% 
    filter_all(all_vars(!is.na(.))) %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    group_by(
      age_dec,
      sex,
      race
    ) %>% 
    summarise(
      mean_income_demo_sar = matrixStats::weightedMean(raw_income, w = COMB_WEIGHT, na.rm = TRUE),
      median_income_demo_sar = matrixStats::weightedMedian(raw_income, w = COMB_WEIGHT, na.rm = TRUE),
      gini_demo_sar = reldist::gini(raw_income, w = COMB_WEIGHT)
    ) %>% 
    ungroup()
  
  df <-
    df %>% 
    mutate(
      age_dec = round(age, digits = -1)
    ) %>% 
    left_join(
      med_inc_gallup,
      by = c("sex", "age_dec", "race")
    )
  
  return(df)
}
