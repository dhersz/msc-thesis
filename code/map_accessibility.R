map_accessibility <- function(accessibility, BU = TRUE) {
  
  grid_cells <- read_rds("./data/rio_h3_grid_res_6.rds") %>% 
    rowid_to_column("id") %>% 
    left_join(accessibility, by = "id")
  
  rio_contour <- grid_cells %>% st_union()
  
  tm_shape(grid_cells) +
    tm_fill(col = "accessibility", title = "Oportunidades") +
  tm_shape(rio_contour) +
    tm_borders() +
  tm_layout(title = ifelse(BU, "Com BU", "Sem BU"), legend.outside = TRUE)
  
}