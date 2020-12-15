library(ggplot2)
library(data.table)
source("./code/06_analyse_results.R")

generate_maps <- function(grid_name = "grid_with_data_aop", 
                          router = "rio_no_inter", 
                          res = 8) {
  
  # * read and prepare data -------------------------------------------------
  
  
  router_folder     <- paste0("./data/", router, "_res_", res)
  study_area_folder <- paste0(router_folder, "/study_area")
  
  grid_data <- readr::read_rds(paste0(router_folder, "/", grid_name, ".rds"))
  
  rapid_transit_stations <- extract_rapid_transit(router)
  
  return(rapid_transit_stations)
  
}