itin_count_dup <- function(itineraries) {
  
  df <- itineraries %>%
    filter(is.na(error_id)) %>% 
    select(-c(error_id, error_msg)) %>% 
    mutate(first_leg_start_time = ifelse(leg_id == 1, leg_start_time, 0)) %>% 
    group_by(origin_id, dest_id, it_id, itin_start_time, itin_end_time) %>% 
    mutate(first_leg_start_time = lubridate::as_datetime(sum(first_leg_start_time))) %>% 
    ungroup() %>% 
    select(-itin_start_time) %>% 
    group_by(origin_id, dest_id, it_id, first_leg_start_time, itin_end_time) %>% 
    nest() %>% 
    ungroup() %>%
    mutate(
      duration = lubridate::as.duration(itin_end_time - first_leg_start_time)
    ) %>%
    group_by(origin_id, dest_id) %>%
    summarise(
      total_itin = n(),
      distinct_itin = n_distinct(duration)
    )
  
  df
}