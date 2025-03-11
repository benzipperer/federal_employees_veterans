download_ipums_acs = function(download_date) {
  
  vars = c(
    "STATEFIP", "PUMA",
    "SPLOC", "SEX", "AGE", "RACE", "HISPAN", "VETSTAT", "EDUC",
    "EMPSTAT", "CLASSWKR", "IND"
  )
    
  extract = define_extract_micro(
    collection = "usa",
    description = "5 year ACS for federal employee counts",
    samples = "us2023c",
    variables = vars
  )
    
  dl_ipums_micro(extract) |> 
    clean_names()
}
clean_acs_micro = function(acs_raw_ipums) {
  all_data = acs_raw_ipums |>
    mutate(
      usps = if_else(ind == 6370, 1, 0),
      veteran = if_else(vetstat == 2, 1, 0),
      armed_forces = if_else(empstatd %in% 14:15, 1, 0)
    ) |> 
    mutate(state_fips = as.numeric(statefip), .keep = "unused")
  
  sp_data = all_data |> 
    filter(sploc > 0) |> 
    select(
      serial, 
      pernum = sploc, 
      spouse_veteran = veteran, 
      spouse_af = armed_forces
    ) |> 
    mutate(spouse_status = 1)
  
  sp_joined_data = all_data |> 
    left_join(sp_data, by = c("serial", "pernum")) |> 
    replace_na(list(spouse_veteran = 0, spouse_status = 0, spouse_af = 0))
}
