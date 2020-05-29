source("./code/setup.R")
source("./code/setup_local.R")

main_func <- function() {
  origin <- head(readr::read_csv("centroids_rio.csv"), 500)
  #dest <- tail(readr::read_csv("centroids_rio.csv"), 1)
  dest <- tibble::tibble(id = 1, lat = -22.906453, long = -43.179156) # carioca
  
  v <- itin_get(
    origin,
    dest,
    param = list(mode = "TRANSIT",
                 date = 01-01-2020,
                 time = "08:00am",
                 arriveBy = FALSE,
                 maxWalkDistance = 2000,
                 numItineraries = 3),
    router = "router"
  )
  
  # v
    
  itin_details(v)
}

workflow <- function() {
  
  itineraries <- read_rds("./data/itin_details_h3_res_6.rds")
  aggregated_info <- read_rds("./data/grid_res_6_with_info.rds")
  
  access_bu <- calculate_accessibility(itineraries, aggregated_info, BU = TRUE, travel_time_threshold = 90, cost_threshold = 7.5)
  access_no_bu <- calculate_accessibility(itineraries, aggregated_info, BU = FALSE, travel_time_threshold = 90, cost_threshold = 7.5)
  
  map_no_bu <- map_accessibility(access_no_bu, BU = FALSE)
  map_bu <- map_accessibility(access_bu, BU = TRUE)
  
  gini_no_bu <- gini_index(access_no_bu, aggregated_info)
  gini_bu <- gini_index(access_bu, aggregated_info)
  
  print(tmap_arrange(map_no_bu, map_bu))
  
  cowplot::plot_grid(lorenz_curve(access_bu, aggregated_info, gini_bu, BU = TRUE), lorenz_curve(access_no_bu, aggregated_info, gini_no_bu, BU = FALSE))

  # costs <- c(4, 5, 6, 7, 8, 9, 10)
  # travel_times <- c(30, 40, 50, 60, 70, 80, 90)
  # message <- vector("character", length = length(costs) * length(travel_times))
  # 
  # c <- 1
  # for(i in seq_along(costs)) {
  #   
  #   for(j in seq_along(travel_times)) {
  #     
  #     access_bu <- calculate_accessibility(itineraries, aggregated_info, BU = TRUE, travel_time_threshold = travel_times[j], cost_threshold = costs[i])
  #     access_no_bu <- calculate_accessibility(itineraries, aggregated_info, BU = FALSE, travel_time_threshold = travel_times[j], cost_threshold = costs[i])
  #     
  #     gini_bu <- gini_index(access_bu, aggregated_info)
  #     gini_no_bu <- gini_index(access_no_bu, aggregated_info)
  #     
  #     message[c] <- str_c("Accessibility at R$ ", costs[i], " and ", travel_times[j], " minutes:\nGini with BU: ", gini_bu, "\nGini without BU: ", gini_no_bu, "\n\n")
  #     
  #     c <- c + 1
  #     
  #   }
  #   
  # }
  
}

generate_sample <- function(itineraries_with_costs = NULL, n = 100, routes_info = NULL) {
  
  if(is.null(routes_info)) routes_info <- read_csv("./data/routes_info_rio.csv")
  
  if(is.null(itineraries_with_costs)) {
    itineraries_with_costs <- read_rds("./data/itin_details_h3_res_6.rds") %>% 
      mutate(
        routeId = str_extract(routeId, "\\d+$"),
        routeId = ifelse(is.na(routeId), "0", routeId),
        routeId = as.integer(routeId)
      ) %>% 
      left_join(routes_info, by = c("routeId" = "route_id")) %>% 
      select(-c(fare_id, route_short_name, route_long_name, price_bu, price)) %>% 
      rename(leg_type = type) %>% 
      bind_cols(tibble(leg_cost_bu = itin_calculate_fare(.))) %>%
      bind_cols(tibble(leg_cost_no_bu = itin_calculate_fare(., BU = FALSE)))
  }
  
  sampled <- itineraries_with_costs %>% 
    select(-itin_start_time, -itin_end_time) %>% 
    group_by(origin_id, dest_id, it_id) %>% 
    nest() %>% 
    ungroup() %>% 
    slice(sample(1:nrow(.), n))
  
  sampled
  
}