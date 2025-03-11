count_vets = function(data, fed_emp_benchmarks, geo_level) {
  state_names = state |> 
    mutate(state_fips = as.numeric(fips)) |> 
    select(state_fips, state_abb = usps, state_name = state)
  
  
  if (geo_level == "cd") {
    benchmarks = fed_emp_benchmarks |> 
      select(state_fips, cd118 = cd, federal_workers)
    
    geog_levels = c("state_fips", "cd118")
  }
  
  if (geo_level == "state") {
    benchmarks = fed_emp_benchmarks |> 
      select(state_fips, federal_workers)
    
    geog_levels = "state_fips"
  }
  
  if (geo_level == "us") {
    benchmarks = fed_emp_benchmarks |> 
      select(state_fips, federal_workers)
    
    geog_levels = "state_fips"
  }
  
  prep_data = data |> 
    mutate(
      vetspouse = spouse_veteran == 1 | spouse_af == 1,
      vet_vetspouse = vetspouse == 1 | veteran == 1
    )
  
  results = prep_data |> 
    summarize(
      share_vet = weighted.mean(veteran, w = weight_ces),
      share_vetspouse = weighted.mean(vetspouse, w = weight_ces),
      share_vet_vetspouse = weighted.mean(vet_vetspouse, w = weight_ces),
      n = n(),
      n_afact = sum(afact),
      .by = all_of(geog_levels)
    ) |> 
    inner_join(benchmarks, by = geog_levels) |> 
    mutate(
      veteran = share_vet * federal_workers,
      veteran_spouse = share_vetspouse * federal_workers,
      veteran_veteran_spouse = share_vet_vetspouse * federal_workers,
    ) |> 
    mutate(small_sample = n < 400) |> 
    mutate(across(
      federal_workers|veteran|veteran_spouse|veteran_veteran_spouse,
      ~ label_comma(accuracy = 1)(round(.x / 100) * 100),
      .names = "pretty_{.col}"
    )) |> 
    mutate(
      pretty_veteran = if_else(veteran < 100, NA, pretty_veteran),
      pretty_veteran_veteran_spouse = if_else(veteran_veteran_spouse < 100, NA, pretty_veteran_veteran_spouse),
    ) |> 
    mutate(across(
      matches("pretty_veteran"),
      ~ if_else(small_sample, NA, .x)
    ))
  
  if (geo_level == "cd") {
    redistricting_exceptions = c(
      "Alabama",
      "Georgia",
      "Louisiana",
      "North Carolina",
      "New York"
    )
    
    final_results = results |> 
      inner_join(state_names, by = "state_fips") |> 
      # drop redistricting exceptions
      filter(!state_name %in% redistricting_exceptions) |> 
      # deal with naming of DC and single state CDs
      mutate(cd118 = if_else(cd118 %in% c(0, 98), 1, cd118)) |> 
      mutate(cd_name = paste(state_name, "-", cd118)) |> 
      arrange(state_name, cd118) |> 
      select(
        cd_name, 
        federal_workers = pretty_federal_workers,
        veteran = pretty_veteran, 
        veteran_veteran_spouse = pretty_veteran_veteran_spouse
      )
  }
  
  if (geo_level == "state") {
    final_results = results |> 
      inner_join(state_names, by = "state_fips") |> 
      arrange(state_name) |> 
      select(
        state_name, 
        federal_workers = pretty_federal_workers,
        veteran = pretty_veteran, 
        veteran_veteran_spouse = pretty_veteran_veteran_spouse
      )
  }
  
  if (geo_level == "us") {
    spouse_val = prep_data |> 
      summarize(
        share_vet_vetspouse = weighted.mean(vet_vetspouse, w = weight_ces),
        .by = all_of(geog_levels)
      ) |>
      inner_join(benchmarks, by = geog_levels) |> 
      mutate(
        veteran_veteran_spouse = share_vet_vetspouse * federal_workers,
      ) |> 
      summarize(sum(veteran_veteran_spouse)) |> 
      pull()
    
    spouse_pop = prep_data |> 
      filter(vet_vetspouse == 1) |> 
      mutate(race_group = case_match(
        race, 
        1 ~ "White", 
        2 ~ "Black", 
        3:6 ~ "Asian American Pacific Islander", 
        7:9 ~ "Multiple"
      )) |> 
      mutate(race_group = if_else(hispan > 0, "Hispanic", race_group)) |> 
      mutate(gender_group = case_match(sex, 1 ~ "Male", 2 ~ "Female"))
    
    race_counts = spouse_pop |> 
      count(race_group, wt = weight_ces) |> 
      mutate(
        value = n / sum(n) * spouse_val,
        value = label_comma(accuracy = 1)(round(value / 100) * 100)
      ) |> 
      mutate(name = "veteran_veteran_spouse") |> 
      select(name, group = race_group, value) |> 
      arrange(group)
    
    gender_counts = spouse_pop |> 
      count(gender_group, wt = weight_ces) |> 
      mutate(
        value = n / sum(n) * spouse_val,
        value = label_comma(accuracy = 1)(round(value / 100) * 100)
      ) |> 
      mutate(name = "veteran_veteran_spouse") |> 
      select(name, group = gender_group, value) |> 
      arrange(group)
    
    final_results = results |> 
      summarize(across(
        federal_workers|veteran|veteran_spouse|veteran_veteran_spouse,
        sum
      )) |> 
      mutate(across(
        everything(),
        ~ label_comma(accuracy = 1)(round(.x / 100) * 100)
      )) |> 
      pivot_longer(everything()) |> 
      mutate(group = "Overall") |> 
      bind_rows(race_counts, gender_counts)  |> 
      mutate(name = case_match(
        name,
        "federal_workers" ~ "All federal employees",
        "veteran" ~ "Veterans",
        "veteran_spouse" ~ "Spouses of veterans/military",
        "veteran_veteran_spouse" ~ "Veterans or spouses of veterans/military"
      ))
  }
  
  final_results

}

make_tables = function(vet_counts_us, vet_counts_state, vet_counts_cd) {
  overall_table_file = "data_outputs/table_1.csv"
  state_table_file = "data_outputs/table_2.csv"
  cd_table_file = "data_outputs/table_3.csv"
  
  vet_counts_us |> 
    filter(group == "Overall") |> 
    select(name, value) |> 
    write_csv(overall_table_file)
  
  vet_counts_state |> 
    rename(
      `Overall` = federal_workers,
      `Veterans` = veteran,
      `Veterans and veteran or military spouses` = veteran_veteran_spouse
    ) |> 
    write_csv(state_table_file)
  
  vet_counts_cd |> 
    rename(
      `Overall` = federal_workers,
      `Veterans` = veteran,
      `Veterans and veteran or military spouses` = veteran_veteran_spouse
    ) |> 
    mutate(across(-cd_name, ~ if_else(is.na(.x), "-", .x))) |> 
    write_csv(cd_table_file)
  
  c(overall_table_file, state_table_file, cd_table_file)
}


create_acs_release = function(data, file) {
  data |> 
    write_parquet(file)
  
  file
}

show_table = function(csv) {
  read_csv(csv, col_types = "ccc") |> 
    tinytable::tt() 
} 
  