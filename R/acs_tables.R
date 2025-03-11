clean_acs_tables = function(acs_tables_raw) {
  data = acs_tables_raw |> 
    mutate(state_fips = as.numeric(state))
  
  state_data = data |> 
    filter(geo_id == "state") |> 
    select(geo_id, state_fips, value)
  
  cd_data = data |> 
    filter(
      geo_id == "cd",
      congressional_district != "ZZ"
    ) |> 
    mutate(cd118 = as.numeric(congressional_district)) |> 
    select(geo_id, state_fips, cd118, value)
  
  county_data = data |> 
    filter(geo_id == "county") |> 
    mutate(county_fips = as.numeric(paste0(state, county))) |> 
    select(geo_id, state_fips, county_fips, value)
  
  metro_data = data |> 
    filter(geo_id == "metro") |> 
    mutate(metro_fips = as.numeric(
      metropolitan_statistical_area_micropolitan_statistical_area
    )) |> 
    select(geo_id, metro_fips, value)
  
  bind_rows(
    state_data,
    cd_data,
    county_data,
    metro_data
  )
  
}




download_acs_data = function(download_date) {
  
  geogs = c("state", "cd", "county", "metro")
  
  map(geogs, getCensus_geog) |> 
    list_rbind() |> 
    rename(value = S2408_C01_008E) |> 
    select(-starts_with("S2408"))
}

getCensus_geog = function(geog) {
  
  if (geog == "state") census_geo_id = "state"
  if (geog == "county") census_geo_id = "county"
  if (geog == "cd") census_geo_id = "congressional district"
  if (geog == "metro") census_geo_id = "metropolitan statistical area/micropolitan statistical area"
  
  getCensus(
    name = "acs/acs5/subject", 
    vintage = 2023, 
    vars = "group(S2408)", 
    region = paste0(census_geo_id, ":*")
  ) |> 
    as_tibble() |> 
    mutate(geo_id = geog)
}

# to view variables
# variables = listCensusMetadata(name = "acs/acs5/subject", vintage = 2023, type = "variables", group = "S2408")
# variables |> as_tibble()