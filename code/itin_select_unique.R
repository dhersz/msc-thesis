itin_select_unique <- function(itineraries) {
  
  # sometimes OTP returns multiple identical itineraries for each OD pair, but at different times
  # this issue gets more relevant when you increase the numItineraries parameter sent in request
  
  # this function selects only unique itineraries for each O-D pair
  # the legs' start and end time are disregarded, and the route, distance and mode are compared
  
  # in this case, leg_start_time and leg_end_time are thrown away
  # doing this makes the function much faster. keeping these columns imply in creating a temporary
  # column with map() in order to use a copy of 'data' with no legs' start and end time to compare
  # itineraries, which is very slow
  #### e.g. mutate(temp = map(data, function(i) select(i, -leg_start_time, -leg_end_time)))
  
  df <- itineraries %>%
    
    # filter out errors, select relevant columns and nest each itinerary's legs details in a df
    filter(is.na(error_id)) %>% 
    select(-error_id, -error_msg, -leg_start_time, -leg_end_time) %>% 
    group_by(origin_id, dest_id, it_id, itin_start_time, itin_end_time) %>% 
    nest() %>% 
    
    # filter out non unique itineraries (within each OD pair)
    group_by(origin_id, dest_id) %>% 
    filter(!duplicated(data)) %>% 
    
    # format df back to how it used to be
    ungroup() %>% 
    unnest(data)
  
  df
  
}