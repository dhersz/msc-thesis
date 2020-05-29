source("./code/setup.R")
source("./code/setup_local.R")

census_tract_to_grid <- function(census_tract_info = NULL, muni = 3304557, res = 6) {
  
  if (is.null(census_tract_info)) {
    
    filepath <- "./data/rio_census_tract_with_info.rds"
    
    if (file.exists(filepath)) census_tract_info <- read_rds(filepath)
    else census_tract_info <- generate_census_tract_info(muni)
      
  }
  
  census_tract_info <- census_tract_info %>%
    select(-code_tract) %>% 
    st_transform(5880) %>% 
    st_make_valid()
  
  h3_grid <- read_rds(str_c("./data/rio_h3_grid_res_", res, ".rds")) %>% 
    st_transform(5880)
  
  # throws a warning, not sure why
  aggregated_to_grid <- st_interpolate_aw(census_tract_info, h3_grid, extensive = TRUE) %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    rename(id = Group.1)
  
  aggregated_to_grid
  
}