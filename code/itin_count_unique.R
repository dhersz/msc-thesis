itin_count_unique <- function(itineraries) {
  
  df <- itineraries %>%
    filter(is.na(error_id)) %>% 
    select(-c(error_id, error_msg, itin_start_time, itin_end_time, leg_start_time, leg_end_time)) %>% 
    group_by(origin_id, dest_id, it_id) %>% 
    nest() %>% 
    rename(legs = data) %>% 
    ungroup() %>% 
    group_by(origin_id, dest_id) %>% 
    nest() %>% 
    rename(itineraries = data) %>% 
    ungroup() %>% 
    mutate(
      total_itin = map_dbl(itineraries, function(i) nrow(i)),
      unique_itin = map_dbl(itineraries, function(i) length(unique(i$legs)))
    ) %>% 
    select(-itineraries)
  
  df
  
}