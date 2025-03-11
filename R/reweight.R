create_fedscope_targets = function(fedscope_targets_raw) {
  fedscope_targets_raw |> 
    mutate(level = case_match(
      group_value,
      "0_24" ~ 1,
      "25_34" ~ 2,
      "35_44" ~ 3,
      "45_54" ~ 4,
      "55_64" ~ 5,
      "65_99" ~ 6,
      "lths" ~ 1,
      "hs" ~ 2,
      "sc" ~ 3,
      "ba" ~ 4,
      "adv" ~ 5,
      "male" ~ 0,
      "female" ~ 1,
      "nonveteran" ~ 0,
      "veteran" ~ 1,
      "white" ~ 1,
      "black" ~ 2,
      "hispanic" ~ 3,
      "aapi" ~ 4,
      "multiple" ~ 5
    )) |> 
    mutate(group = paste0(group, "_group")) |> 
    mutate(proportion = value / sum(value), .by = group) |> 
    select(variable = group, level, proportion)
}


reweight_acs_data_fedscope = function(acs_data, fedscope_targets) {
  max_weight_value = 60
  
  data = acs_data |> 
    filter(usps == 0) |> 
    mutate(age_group = case_when(
      age < 25 ~ 1,
      age >= 25 & age < 35 ~ 2,
      age >= 35 & age < 45 ~ 3,
      age >= 45 & age < 55 ~ 4,
      age >= 55 & age < 65 ~ 5,
      age >= 65 ~ 6
    )) |> 
    mutate(educ_group = case_match(
      educd,
      0:61 ~ 1,
      62:64 ~ 2,
      65:99 ~ 3,
      100:101 ~ 4,
      114:116 ~ 5,
    )) |> 
    mutate(race_group = case_match(
      race, 
      1 ~ 1, 
      2 ~ 2, 
      3:6 ~ 4, 
      7:9 ~ 5
    )) |> 
    mutate(race_group = if_else(hispan > 0, 3, race_group)) |> 
    mutate(
      gender_group = case_match(sex, 1 ~ 0, 2 ~ 1),
      veteran_group = veteran,
      weight_norm = weight / mean(weight)
    )

  data |>
    harvest(
      target = fedscope_targets,
      start_weights = data$weight_norm,
      max_weight = max_weight_value
    ) |> 
    verify(weights < max_weight_value) |> 
    rename(weight_fedscope = weights)
    
}

reweight_acs_data_ces = function(
  acs_cd,
  acs_cd_fedscope,
  ces_fed_excl_usps,
  ces_fed_usps
) {
  
  usps_benchmarked = acs_cd |> 
    filter(usps == 1) |> 
    mutate(weight_ces = weight * ces_fed_usps / sum(weight)) 

  non_usps_benchmarked = acs_cd_fedscope |> 
    verify(usps == 0) |> 
    mutate(
      weight_ces = weight_fedscope * ces_fed_excl_usps / sum(weight_fedscope)
    )
  
  usps_benchmarked |> 
    bind_rows(non_usps_benchmarked) 
  
}