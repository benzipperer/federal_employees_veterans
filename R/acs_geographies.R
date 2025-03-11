filter_to_federal = function(data) {
  data |> 
    filter(
      # with job at work
      empstatd == 10, 
      # federal
      classwkrd == 25
    )
}

create_acs_cd = function(acs_national, puma_cd_map_file) {
  puma_cd_map = puma_cd_map_file |> 
    mutate(across(state|cd118|puma22, as.numeric)) |> 
    select(
      state_fips = state,
      puma = puma22,
      cd118,
      afact
    )
  
  acs_national |> 
    filter_to_federal() |> 
    left_join(
      puma_cd_map, 
      by = c("state_fips", "puma"), 
      relationship = "many-to-many"
    ) |> 
    # adjust weights for multiple obs due to puma <-> CD mapping
    mutate(weight = perwt * afact) 
    
}