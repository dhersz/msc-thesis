library(dplyr)
library(sf)

aggregate_data <- function(res = 7) {
  
  opportunities_data <- readr::read_csv("./data/estabelecimentos.csv") %>%
    rename(code_tract = CD_GEOCODI, opportunities = Total)
  
  population_data <- readr::read_csv("./data/Censo2010_Basico_RJ.csv") %>% 
    select(code_tract = Cod_setor, population = V002)
  
  income_data <- readr::read_csv("./data/Censo2010_PessoaRenda_RJ.csv") %>% 
    select(code_tract = Cod_setor, total_income = V022)
  
  # aggregate data to census tracts shapes
  census_tract_data <- geobr::read_census_tract(3304557) %>% 
    select(code_tract) %>% 
    mutate(code_tract = as.numeric(as.character(code_tract))) %>% 
    left_join(opportunities_data, by = "code_tract") %>% 
    left_join(population_data, by = "code_tract") %>% 
    left_join(income_data, by = "code_tract") %>% 
    mutate(
      opportunities = ifelse(is.na(opportunities), 0, opportunities),
      population = ifelse(is.na(population), 0, population),
      total_income = as.numeric(ifelse(total_income == "X" | is.na(total_income) | population == 0, 0, total_income))
    ) %>% 
    select(-code_tract) %>% 
    st_transform(5880) %>% 
    st_make_valid()
  
  hex_grid <- readr::read_rds(stringr::str_c("./data/rio_h3_grid_res_", res, ".rds"))
  hex_grid_crs <- st_crs(hex_grid)
  
  hex_grid <- hex_grid %>% st_transform(5880)
  
  hex_grid_with_data <- st_interpolate_aw(census_tract_data, hex_grid, extensive = TRUE) %>% 
    rename(id = Group.1) %>% 
    st_transform(hex_grid_crs)
  
  readr::write_rds(hex_grid_with_data, stringr::str_c("./data/rio_h3_grid_res_", res, "_with_data.rds"))
  
}