## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

download_date = "1 March 2025"

tar_assign({

  # federal gov benchmarks
  # from BLS series CES9091100001, value Jan 2025
  ces_fed_excl_usps = 2419500 |> tar_target()
  # from BLS series CES9091912001, value Jan 2025
  ces_fed_usps = 604800 |> tar_target()

  acs_raw_ipums = download_ipums_acs(download_date) |> 
    tar_parquet()
  
  puma_cd_map_file = "data_inputs/geocorr2022_puma2020_cd118.csv" |> 
    tar_file_read(read_csv(!!.x, show_col_types = FALSE))
  
  fedscope_targets_raw = "data_inputs/fedscope_sep2024.csv" |> 
    tar_file_read(read_csv(!!.x, show_col_types = F))
  
  fed_emp_benchmark_cd = "data_inputs/federal_workers_cd.csv" |> 
    tar_file_read(read_csv(!!.x, show_col_types = F))
  
  fed_emp_benchmark_state = "data_inputs/federal_workers_state.csv" |> 
    tar_file_read(read_csv(!!.x, show_col_types = F))
  
  fedscope_targets = create_fedscope_targets(fedscope_targets_raw) |> 
    tar_target()
  
  acs_national = clean_acs_micro(acs_raw_ipums) |> 
    tar_parquet()
  
  acs_cd = create_acs_cd(acs_national, puma_cd_map_file) |> 
    tar_parquet()
  
  acs_cd_fedscope = reweight_acs_data_fedscope(acs_cd, fedscope_targets) |> 
    tar_parquet()
  
  acs_cd_final = reweight_acs_data_ces(
    acs_cd,
    acs_cd_fedscope,
    ces_fed_excl_usps,
    ces_fed_usps
  ) |> 
    tar_parquet()
  
  acs_release_file = create_acs_release(
    acs_cd_final,
    "release/acs_cd_reweighted.parquet"
  ) |> 
    tar_file()
  
  vet_counts_cd = count_vets(
    acs_cd_final,
    fed_emp_benchmark_cd,
    "cd"
  ) |> 
    tar_target()
  
  vet_counts_state = count_vets(
    acs_cd_final,
    fed_emp_benchmark_state,
    "state"
  ) |> 
    tar_target()
  
  vet_counts_us = count_vets(
    acs_cd_final,
    fed_emp_benchmark_state,
    "us"
  ) |> 
    tar_target()
  
  tables = make_tables(vet_counts_us, vet_counts_state, vet_counts_cd) |> 
    tar_file()

})
