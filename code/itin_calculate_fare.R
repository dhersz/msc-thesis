itin_calculate_fare <- function(itineraries, routes_info = NULL, fare_schema = NULL, BU = TRUE, debug = FALSE) {
  
  # reads route_info e fare_schema dataframes, in case they haven't been set
  
  if(is.null(routes_info)) routes_info <- read_csv("./data/routes_info_rio.csv")
  if(is.null(fare_schema)) fare_schema <- read_csv("./data/fare_schema_rio.csv")
  
  # if BU argument has been set to FALSE the function calculates the fare as if there isn't any 
  # type of discounts.
  # to do so, the special fares are filtered out of the fare_schema dataframe
  
  if(!BU) fare_schema <- fare_schema %>% filter(with_card == FALSE)
  
  itineraries <- itineraries %>% 
    mutate(
      routeId = str_extract(routeId, "\\d+$"),
      routeId = ifelse(is.na(routeId), "0", routeId),
      routeId = as.integer(routeId)
    ) %>% 
    left_join(routes_info, by = c("routeId" = "route_id")) %>% 
    select(-c(distance, mode, routeId, fare_id, route_short_name, route_long_name))
  
  int_fare <- set_names(fare_schema$fare, nm = fare_schema$int_id)
  int_with_card <- set_names(fare_schema$with_card, nm = fare_schema$int_id)
  
  leg_cost <- vector("double", length = nrow(itineraries))
  
  for (i in 1:nrow(itineraries)) {
    
    if (itineraries$leg_id[i] == 1) {
      last_moto <- ""
      price_last_moto <- 0
      has_integrated <- FALSE
      had_special <- FALSE
    }
    
    leg_id <- str_c(last_moto, "&", itineraries$type[i])
    
    if (is.na(int_fare[leg_id])) {
      
      if (BU && !had_special && !has_integrated && itineraries$type[i] %in% c("barca", "onibus_intermunicipal") && itineraries$price_bu[i] < itineraries$price[i]) {
        leg_cost[i] <- itineraries$price_bu[i]
        had_special <- TRUE
      }
      else leg_cost[i] <- itineraries$price[i]
      
      if (itineraries$type[i] != "caminhada" && had_special && !has_integrated && last_moto %in% c("barca", "onibus_intermunicipal")) {
        has_integrated <- TRUE
      }
      
    } else {
      
      if (!int_with_card[leg_id]) leg_cost[i] <- int_fare[leg_id] - itineraries$price[i]
      else if (has_integrated) leg_cost[i] <- itineraries$price[i]
      else {
        leg_cost[i] <- int_fare[leg_id] - price_last_moto
        has_integrated <- TRUE
      }
      
    }
    
    if (itineraries$type[i] != "caminhada" && (is.na(int_fare[leg_id]) || int_with_card[leg_id])){
      last_moto <- itineraries$type[i]
      price_last_moto <- leg_cost[i]
    }
    
    if (debug) {
      cat(str_c("\nLeg ", itineraries$leg_id[i],
                  "\nlast_moto = ", last_moto,
                  "\nprice_last_moto = ", price_last_moto,
                  "\nhas_integrated = ", has_integrated,
                  "\nhad_special = ", had_special, "\n"))
    }
    
  }
  
  leg_cost
  
}