library(dplyr)
library(sf)

aggregate_data <- function(router = "rio", res = 7) {
  
  # read census tract data (population and income) and aggregate to an sf object
  
  population_data <- readr::read_csv("./data/Censo2010_Basico_RJ.csv") %>% 
    select(code_tract = Cod_setor, population = V002)
  
  income_data <- readr::read_csv("./data/Censo2010_PessoaRenda_RJ.csv") %>% 
    select(code_tract = Cod_setor, total_income = V022)
  
  census_tract_data <- geobr::read_census_tract(3304557) %>% 
    select(code_tract) %>% 
    mutate(code_tract = as.numeric(as.character(code_tract))) %>% 
    left_join(population_data, by = "code_tract") %>% 
    left_join(income_data, by = "code_tract") %>% 
    mutate(
      population = ifelse(is.na(population), 0, population),
      total_income = as.numeric(ifelse(total_income == "X" | is.na(total_income) | population == 0, 0, total_income))
    ) %>% 
    select(-code_tract) %>% 
    st_transform(5880) %>% 
    st_make_valid()
  
  # read traffic analysis zone data
  
  opportunities_data <- readr::read_rds("./data/empregos_taz.rds") %>% 
    select(opportunities = empregos) %>% 
    st_transform(5880)
  
  # read given resolution hexagonal grid and aggregate data to it
  
  router_folder <- paste0("./data/", router, "_res_", res)
  
  hex_grid <- readr::read_rds(paste0(router_folder, "/grid_raw.rds"))
  hex_grid_crs <- st_crs(hex_grid)
  
  hex_grid <- hex_grid %>% st_transform(5880)
  
  hex_grid_with_census <- st_interpolate_aw(census_tract_data, hex_grid, extensive = TRUE)
  hex_grid_with_opportunities <- st_interpolate_aw(opportunities_data, hex_grid, extensive = TRUE)
  
  hex_grid_with_data <- hex_grid_with_census %>% 
    st_drop_geometry() %>% 
    left_join(hex_grid_with_opportunities, by = "Group.1") %>% 
    rename(id = Group.1) %>% 
    st_as_sf() %>% 
    st_set_crs(5880) %>% 
    st_transform(hex_grid_crs)
  
  grid_data_path <- paste0(router_folder, "/grid_with_data.rds")
  
  readr::write_rds(hex_grid_with_data, grid_data_path)
  
}