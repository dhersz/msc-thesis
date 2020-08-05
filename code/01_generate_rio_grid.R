library(dplyr)
library(sf)

generate_hex_grid <- function(res = 7) {
  
  rio_municipality <- readr::read_rds("./data/rio_municipality.rds")
  
  # when the resolution is below 9 no hexagons have their centroids on Paquetá, hence the island is not actually represented
  # to prevent that, the hexagons are generated covering the entire bounding box of rio, and then are intersected with rio's shape
  # even when accounting that, Paquetá is not represented in resolution 6 or below due to the increased size of hexagons
  
  if (res < 9) {
    
    rio_crs <- st_crs(rio_municipality)
    
    rio_bbox <- st_bbox(rio_municipality)
    xmin <- rio_bbox["xmin"]
    xmax <- rio_bbox["xmax"]
    ymin <- rio_bbox["ymin"]
    ymax <- rio_bbox["ymax"]
    
    shape_bbox <- st_polygon(list(rbind(c(xmin, ymax), c(xmax, ymax), c(xmax, ymin), c(xmin, ymin), c(xmin, ymax)))) %>% 
      st_sfc() %>% 
      st_set_crs(rio_crs) %>% 
      st_as_sf()
    
    # st_intersection assumes its input is in a projected crs
    # since data downloaded from geobr comes with crs 4674 (geographic), the crs is temporarily transformed to 5880 (projected)
    
    rio_municipality <- rio_municipality %>% st_transform(5880)
    
    hex_ids <- h3jsr::polyfill(shape_bbox, res = res, simple = FALSE)
    
    hex_grid <- unlist(hex_ids$h3_polyfillers) %>% 
      h3jsr::h3_to_polygon(simple = FALSE) %>% 
      st_transform(5880) %>% 
      st_intersection(rio_municipality) %>% 
      st_transform(rio_crs) %>% 
      dplyr::select(h3_address, geometry)
    
  } else {
    
    hex_ids <- h3jsr::polyfill(rio_municipality, res = res, simple = FALSE)
    
    hex_grid <- unlist(hex_ids$h3_polyfillers) %>%
      h3jsr::h3_to_polygon(simple = FALSE) %>%
      dplyr::select(h3_address, geometry)
    
  }
  
  readr::write_rds(hex_grid, stringr::str_c("./data/rio_h3_grid_res_", res, ".rds"))
  
}