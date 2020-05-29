calculate_accessibility <- function(itineraries = NULL, aggregated_info = NULL, BU = TRUE, travel_time_threshold = 90, cost_threshold = 7.5) {
  
  if (is.null(itineraries)) {
    
    itin_details_filepath <- "./data/itin_details_h3_res_6.rds"
    
    if (file.exists(itin_details_filepath)) itineraries <- read_rds(itin_details_filepath)
    
    else {
      origin <- read_rds("./data/rio_h3_grid_res_6.rds") %>% 
        st_transform(5880) %>% 
        st_centroid(of_largest_polygon = TRUE) %>% 
        st_transform(4674) %>% 
        st_coordinates() %>% 
        as_tibble() %>% 
        rowid_to_column("id") %>% 
        rename(long = X, lat = Y)
      
      dest <- origin
      
      itineraries <- itin_get(origin, dest, param = list(numItineraries = 3), router = "router") %>% 
        itin_details() %>% 
        itin_select_unique()
    }
    
  }
  
  if (is.null(aggregated_info)) aggregated_info <- read_rds("./data/grid_res_6_with_info.rds")
  
  aggregated_opp <- aggregated_info %>% select(id, establishments)
  
  inside_accessibility <- aggregated_opp %>% rename(opp_inside = establishments)
  
  outside_accessibility <- itineraries %>% 
    bind_cols(tibble(leg_cost = itin_calculate_fare(., BU = BU))) %>%
    mutate(travel_time = as.numeric(as.duration(itin_end_time - itin_start_time), "minutes")) %>% 
    group_by(origin_id, dest_id, it_id) %>% 
    summarise(
      travel_time = mean(travel_time),
      cost = sum(leg_cost)
    ) %>% 
    left_join(aggregated_opp, by = c("dest_id" = "id")) %>% 
    filter(travel_time <= travel_time_threshold, cost <= cost_threshold) %>% 
    group_by(origin_id, dest_id) %>% 
    slice(1) %>% 
    group_by(origin_id) %>% 
    summarise(opp_outside = sum(establishments)) %>% 
    rename(id = origin_id) %>% 
    ungroup()
  
  accessibility <- inside_accessibility %>% 
    left_join(outside_accessibility, by = "id") %>% 
    mutate(accessibility = ifelse(is.na(opp_outside), opp_inside, opp_inside + opp_outside)) %>% 
    select(-opp_inside, -opp_outside)
  
  accessibility
  
}