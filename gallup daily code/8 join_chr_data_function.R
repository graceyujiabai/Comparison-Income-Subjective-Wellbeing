join_chr_data <- function(df, year){
  
  year = ifelse(year < 2010, 2010, year)
  
  chr <-
    read_csv(paste0("chr/analytic_data", year, ".csv"), 
             skip = 1) %>% 
    dplyr::slice(2:nrow(.))
  
  
  chr <-
    chr %>% 
    mutate(
      fips_code = as.character(ifelse(nchar(chr$fipscode) == 4, paste0("0", chr$fipscode), chr$fipscode)),
      physicians_scale = scale(v004_rawvalue),
      dentists_scale = scale(ifelse(year %in% c(2012:2020), `v088_rawvalue`, NA)),
      therapists_scale = scale(ifelse(year %in% c(2013:2020), `v062_rawvalue`, NA))
    ) %>% 
    mutate_at(
      vars(
        physicians_scale:therapists_scale
      ),
      as.numeric
    )
  
  
  df <-
    df %>% 
    left_join(
      chr %>% 
        dplyr::select(
          fips_code,
          physicians_scale:therapists_scale
        ),
      by = "fips_code"
    )
  
  return(df)
  
  
}
