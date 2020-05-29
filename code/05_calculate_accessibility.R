library(dplyr)
library(sf)

calculate_accessibility <- function(res = 7) {
  
  itineraries_details <- readr::read_rds(stringr::str_c("./data/itineraries_details_res_", res, ".rds"))
  
  routes_info <- generate_routes_info()
  fare_schema <- generate_fare_schema()
  
  itineraries_details <- itineraries_details %>% 
    mutate(
      leg_cost_BU = calculate_fare(., routes_info, fare_schema, BU = TRUE),
      leg_cost_no_BU = calculate_fare(., routes_info, fare_schema, BU = FALSE)
    )
  
  itineraries_details
  
}

generate_routes_info <- function() {
  
  fetranspor_info <- raw_routes_info("fetranspor")
  supervia_info <- raw_routes_info("supervia")
  
  integration_metro_bus <- c(513, 603, 608, 605, 609, 209, 611, 614, 616, 913, 876)
  
  routes_info <- bind_rows(fetranspor_info, supervia_info) %>% 
    mutate(
      route_short_name = ifelse(is.na(route_short_name), "", route_short_name),
      type = ifelse(fare_id == 32069624 | fare_id == 34127545, "onibus_municipal", ""),
      type = ifelse(route_short_name %in% as.character(integration_metro_bus), "onibus_metro", type),
      type = ifelse(stringr::str_detect(route_long_name, "Ramal"), "trem", type),
      type = ifelse(stringr::str_detect(route_short_name, "BRT"), "brt", type),
      type = ifelse(stringr::str_detect(route_short_name, "VLT"), "vlt", type),
      type = ifelse(stringr::str_detect(route_short_name, "L\\d") | stringr::str_detect(route_long_name, "Metrô na"), "metro", type),
      type = ifelse(stringr::str_detect(route_short_name, "^2\\d{3}$") | route_short_name == "LECD 25", "frescao_municipal", type),
      type = ifelse(stringr::str_detect(route_short_name, "^\\d{3}[B-Z]$") | stringr::str_detect(route_short_name, "^4\\d{2}A$") | stringr::str_detect(route_short_name, "TB$"), "onibus_intermunicipal", type),
      type = ifelse(stringr::str_detect(route_short_name, "^\\d{4}[B-Z]$"), "frescao_intermunicipal", type),
      type = ifelse(fare_id == 13794408, "barca", type),
      type = ifelse(type == "", "outros", type),
      
      # temp only (I hope)
      # adjusting prices to make sure their logic matches with what is actually true
      
      price = ifelse(route_id == 19209271 | route_id == 19132687, 10, price), # two random intermunicipal bus lines with no fare
      price = ifelse(route_id == 19016860, 3.85, price), # 495L
      price = ifelse(route_id == 12862118, 3.6, price), # 603
      price = ifelse(route_id == 18928473, 4, price), # 716L
      price = ifelse(fare_id == 34127393, 3.6, price), # brt prices = municipal_bus prices
      price = ifelse(fare_id == 52433953, 3.5, price), # vlt prices lower than bus
      price = ifelse(fare_id == 125, 4.2, price), # train price higher than subway
      
      # these types of routes below have special prices when using the BU
      
      price_bu = ifelse(type == "barca", 6, price),
      price_bu = ifelse(type == "onibus_intermunicipal" & price > 7.4, 7.4, price)
    ) %>% 
    bind_rows(tibble::tibble(route_id = 0, fare_id = 0, route_short_name = "caminhada", route_long_name = "caminhada", price = 0, type = "caminhada", price_bu = 0))
  
  routes_info
  
}

raw_routes_info <- function(holder) {
  
  rio <- geobr::read_municipality(3304557) %>% st_transform(5880)
  
  zip_filepath <- stringr::str_c("./otp/graphs/rio/gtfs_", holder, ".zip")
  
  required_files <- c("fare_attributes.txt", "fare_rules.txt", "routes.txt", "shapes.txt", "trips.txt")
  
  files_list <- setNames(
    purrr::map(1:5, function(i) readr::read_csv(unz(zip_filepath, required_files[i]))),
    required_files
  )
  
  fare_attributes <- files_list[["fare_attributes.txt"]]
  fare_rules <- files_list[["fare_rules.txt"]]
  routes <- files_list[["routes.txt"]]
  shapes <- files_list[["shapes.txt"]]
  trips <- files_list[["trips.txt"]]
  
  routes_info <- trips %>% 
    trips_treatment() %>% 
    left_join(routes_treatment(routes), by = "route_id") %>% 
    left_join(fare_rules_treatment(fare_rules, holder, routes_treatment(routes)), by = "route_id") %>% 
    left_join(fare_attributes_treatment(fare_attributes), by = "fare_id") %>% 
    left_join(shapes_treatment(shapes, 4674), by = "shape_id") %>% 
    st_as_sf() %>% 
    st_transform(5880) %>% 
    filter(st_intersects(., rio, sparse = FALSE)) %>% 
    st_drop_geometry() %>% 
    distinct(route_id, fare_id, route_short_name, route_long_name, price)
  
  routes_info
  
}

trips_treatment <- function(trips) {
  
  trips_treated <- trips %>% distinct(route_id, shape_id)
  
  trips_treated
  
}

routes_treatment <- function(routes) {
  
  routes_treated <- routes %>% 
    select(route_id, route_short_name, route_long_name)
  
  routes_treated
  
}

fare_rules_treatment <- function(fare_rules, style, routes_treated = NA) {
  
  # "fetranspor" style refers to each route having its own values
  # else ("supervia") means its based on the stations you drop in and off the system
  # origin/destination_id = 77 refers to Guapimirim branch, not used here, so set to have the same value as the ~default~
  # origin/destination_id = 79 refers to Teleférico do Alemão, which costs 1 BRL (fare_id = 127)
  # origin/destination_id = 78 refers to the ~usual~ SuperVia stations and is our ~default~ (4.6) (fare_id = 125)
  
  if (style == "fetranspor") {
    fare_rules_treated <- fare_rules %>% 
      select(fare_id, route_id)
  } else {
    fare_rules_treated <- routes_treated %>% 
      mutate(fare_id = ifelse(route_id == 247, 127, 125)) %>% 
      select(fare_id, route_id)
  }
  
  fare_rules_treated
  
}

fare_attributes_treatment <- function(fare_attributes) {
  
  fare_attributes_treated <- fare_attributes %>% 
    select(fare_id, price)
  
  fare_attributes_treated
  
}

shapes_treatment <- function(shapes, crs = 4674) {
  
  shapes <- shapes %>% 
    group_by(shape_id) %>% 
    summarise(coord_matrix = list(matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2)))
  
  sfc <- st_multilinestring(shapes$coord_matrix) %>%
    st_sfc() %>%
    st_cast("LINESTRING")

  shapes <- shapes %>%
    select(-coord_matrix) %>%
    st_sf(geom = sfc) %>%
    st_set_crs(crs)
  
  shapes
  
}

generate_fare_schema <- function() {
  
  # fares and fare schema can be consulted at https://www.cartaoriocard.com.br/rcc/institucional/tarifas
  # need_bu column is TRUE when the fare is only valid when using BU. else, there is full integration (you
  # don't need to leave the station to hop in another route), so the integration fare equals the mode's
  # price, with no further additional
  
  fare_schema <- tibble::tribble(
    ~leg_1,                  ~leg_2,                  ~fare, ~need_bu,
    "brt",                   "brt",                   3.6,   FALSE,
    "metro",                 "metro",                 4.1,   FALSE,
    "trem",                  "trem",                  4.2,   FALSE,
    "onibus_municipal",      "onibus_municipal",      3.6,   TRUE,
    "onibus_municipal",      "brt",                   3.6,   TRUE,
    "brt",                   "onibus_municipal",      3.6,   TRUE,
    "onibus_municipal",      "vlt",                   3.6,   TRUE,
    "vlt",                   "onibus_municipal",      3.6,   TRUE,
    "onibus_municipal",      "onibus_metro",          3.6,   TRUE,
    "onibus_metro",          "onibus_municipal",      3.6,   TRUE,
    "vlt",                   "vlt",                   3.5,   TRUE,
    "onibus_metro",          "metro",                 6.05,  TRUE,
    "metro",                 "onibus_metro",          6.05,  TRUE,
    "onibus_intermunicipal", "onibus_municipal",      7.4,   TRUE,
    "onibus_municipal",      "onibus_intermunicipal", 7.4,   TRUE,
    "onibus_intermunicipal", "barca",                 7.4,   TRUE,
    "barca",                 "onibus_intermunicipal", 7.4,   TRUE,
    "brt",                   "metro",                 6.8,   TRUE,
    "metro",                 "brt",                   6.8,   TRUE,
    "trem",                  "metro",                 7.4,   TRUE,
    "metro",                 "trem",                  7.4,   TRUE,
    "vlt",                   "onibus_intermunicipal", 7.4,   TRUE,
    "onibus_intermunicipal", "vlt",                   7.4,   TRUE,
    "vlt",                   "barca",                 7.4,   TRUE,
    "barca",                 "vlt",                   7.4,   TRUE,
    "onibus_intermunicipal", "metro",                 7.4,   TRUE,
    "metro",                 "onibus_intermunicipal", 7.4,   TRUE,
    "onibus_intermunicipal", "trem",                  7.4,   TRUE,
    "trem",                  "onibus_intermunicipal", 7.4,   TRUE)
  
  fare_schema <- fare_schema %>% mutate(int_id = stringr::str_c(leg_1, "&", leg_2))
  
  fare_schema
  
}

calculate_fare <- function(itineraries_details, routes_info, fare_schema, BU = TRUE, debug = FALSE) {
  
  # if BU argument has been set to FALSE the function calculates the fare as if there isn't any 
  # type of discounts, so the special fares are filtered out of the fare_schema dataframe
  
  if(!BU) fare_schema <- fare_schema %>% filter(need_bu == FALSE)
  
  # format route_id in the itineraries_details to fit the fare_schema style, join the dataframes and select relevant columns
  
  legs <- itineraries_details %>% 
    mutate(
      route_id = stringr::str_extract(route_id, "\\d+$"),
      route_id = ifelse(is.na(route_id), "0", route_id),
      route_id = as.integer(route_id)
    ) %>% 
    left_join(routes_info, by = "route_id") %>% 
    select(c(leg_id, route_id, route, price, price_bu, type))
  
  # create vector to lookup if fare needs BU to happen and its price (using vectors makes it much faster than dataframes)
  
  int_fare <- rlang::set_names(fare_schema$fare, nm = fare_schema$int_id)
  int_need_bu <- rlang::set_names(fare_schema$need_bu, nm = fare_schema$int_id)

  # initialise vector to store legs' cost
  
  n <- nrow(legs)
  leg_cost <- vector("double", length = n)

  # looping through each leg
  
  for (i in 1:n) {
    
    # leg costs are based on the mode of the leg, the previous motorised leg, whether a integration
    # that required BU has happenned before and whether a specially priced mode (either intermunicipal
    # bus or ferry) has been taken before
    # the variables within the if below are used as auxiliaries to check for theses conditions. every
    # first leg of a itinerary they need to be ~restarted~ so the previous itinerary doesn't affect the
    # current one

    if (legs$leg_id[i] == 1) {
      last_moto <- ""
      price_last_moto <- 0
      has_integrated <- FALSE
      had_special <- FALSE
    }

    # leg_id is used to lookup for integrations and its prices in the vectors set previously
    
    leg_id <- stringr::str_c(last_moto, "&", legs$type[i])

    # if int_fare[leg_id] is NA the current leg doesn't have any kind of integration with the previous
    # also it may mean that it refers to a walking leg
    # an exception to this rule is when the current leg refers to an intermunicipal bus or a ferry. these
    # modes may have a special price when using the BU. if its original price is higher than the special
    # price, then the user is charged the special price (in the routes_info dataframe the price_bu column
    # indicates the special price)
    # now, let's say that after the ~special leg~ a user embarks another motorised leg, but this new
    # current leg doesn't have any kind of integration with its previous leg (the special). then the
    # system accounts the special price grant as an integration itself, and no further integrations
    # are accounted. if, however, the new current leg integrates with the special, the fare discount is granted
    
    # note that if BU is set to false all the integrations that require the BU have been filtered out,
    # so the majority of the legs (with the exception of those related to modes with full integration)
    # fall within this if and have their full prices accounted for
    
    if (is.na(int_fare[leg_id])) {

      # checking for the conditions for the special price leg.
      # if met, account the special price and assign a variable to indicate it
      # else, account the full normal price
      
      if (BU && !had_special && !has_integrated && legs$type[i] %in% c("barca", "onibus_intermunicipal") && legs$price_bu[i] < legs$price[i]) {
        leg_cost[i] <- legs$price_bu[i]
        had_special <- TRUE
      }
      else leg_cost[i] <- legs$price[i]

      # check for the case where a specially priced leg is accounted as an integration
      
      if (legs$type[i] != "caminhada" && had_special && !has_integrated && last_moto %in% c("barca", "onibus_intermunicipal")) {
        has_integrated <- TRUE
      }

    }
    
    else {
      
      # if a integration exists, it can fall within 3 categories:
      # * the integration doesn't require BU, so the current leg price is 0 (equivalent to the first
      # expression since int_fare[leg_id] and legs$price[i] are the same)
      # * an integration has happenned before already, in which case the full leg price is charged
      # * an integration has not occurred, in which case the excess of the integration fare compared to
      # the last motorised leg price is charged

      if (!int_need_bu[leg_id]) leg_cost[i] <- int_fare[leg_id] - legs$price[i]
      else if (has_integrated) leg_cost[i] <- legs$price[i]
      else {
        leg_cost[i] <- int_fare[leg_id] - price_last_moto
        has_integrated <- TRUE
      }

    }
    
    # if the current leg mode is not walk, save it as the last motorised
    # also checks if an full integration has happenned. if so, doesn't update the variables, so the price
    # of the last motorised mode is the original price of the mode, and not 0 (the second leg price).
    # this is important to ensure that integrations with these modes work alright.
    # e.g. suppose we have brt-> brt -> metro.
    # the correct leg cost is be (brt_price) -> 0 -> (metro_brt_integration - brt_price)
    # if the variables were updated we'd have (brt_price) -> 0 -> (metro_brt_integration)

    if (legs$type[i] != "caminhada" && (is.na(int_fare[leg_id]) || int_need_bu[leg_id])){
      last_moto <- legs$type[i]
      price_last_moto <- leg_cost[i]
    }

    if (debug) {
      cat(stringr::str_c("\nLeg ", legs$leg_id[i],
                "\nlast_moto = ", last_moto,
                "\nprice_last_moto = ", price_last_moto,
                "\nhas_integrated = ", has_integrated,
                "\nhad_special = ", had_special, "\n"))
    }

  }

  leg_cost
  
}