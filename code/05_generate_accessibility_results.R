library(dplyr)
library(sf)
library(data.table)

generate_accessibility_results <- function(dep_time = NULL,
                                           grid_data_path = NULL,
                                           n_cores = 4,
                                           res = 7,
                                           router = "rio") {

  # * read data -------------------------------------------------------------
  
  
  routes_info <- setDT(generate_routes_info(router))
  fare_schema <- generate_fare_schema()
  
  router_folder <- paste0("./data/", router, "_res_", res)
  
  # read grid_data - when grid_data_path is null defaults to a specific path
  
  if (is.null(grid_data_path)) {
    
    grid_data_path <- paste0(router_folder, "/grid_with_data.rds")
    
  }
  
  grid_data <- readr::read_rds(grid_data_path) %>% st_drop_geometry() %>% setDT()
  
  # store itineraries paths in a character vector which will be followed along
  # to calculate accessibility

  itineraries_folder <- paste0(router_folder, "/itineraries")

  if (!is.null(dep_time)) {

    dep_time <- gsub(":", "", dep_time)

    itineraries_paths <- paste0(itineraries_folder, "/itineraries_", dep_time, ".rds")

  } else {

    itineraries <- list.files(itineraries_folder)

    itineraries_paths <- paste0(itineraries_folder, "/", itineraries)

  }

  # loop through each itineraries' path, calculate accessibility and save it in
  # separate folder
  
  for (i in seq_along(itineraries_paths)) {
    
    itineraries_details <- data.table::setDT(readr::read_rds(itineraries_paths[i]))
    
    costs <- calculate_costs(itineraries_details, routes_info, fare_schema, n_cores)
    
    
    # * calculate accessibility -----------------------------------------------
    
    
    # establish costs thresholds for accessibility calculation
    # monetary cost thresholds are specified as percentages of the minimum wage
    
    minimum_wage_percentages <- c(0.20, 0.25, 0.30, 0.35, 0.40, 10)
    travel_time_thresholds   <- c(30, 60, 90, 120)
    
    # store results in a data.table. each iterarion of pwalk adds a column to it
    # total accessibility is the sum of opportunities within the origin itself
    # (column 'inside') and the opportunities of hexagons within reach
    
    minimum_wage <- 1045
    
    iterator <- expand.grid(
      tt = travel_time_thresholds,
      mw = minimum_wage_percentages,
      wt = c("with", "without"),
      stringsAsFactors = FALSE
    )
    
    accessibility <-  rbindlist(purrr::pmap(iterator, function(tt, mw, wt) {
      
      total_opportunities <- grid_data[, .(id, inside = opportunities)]
      
      desired_cost <- paste0("cost_", wt, "_BU")
      
      # the monetary cost threshold equals the amount of money spent per transit
      # trip that, after a monthful of trips, sums up to the specified percentage
      # of minimum wage, considering a 22 days monthly journey of work and 2 
      # trips per day
      
      mc <- mw * minimum_wage / (22 * 2)
      
      outside <- costs[travel_time <= tt & get(desired_cost) <= mc]
      outside <- outside[outside[, .I[1], keyby = .(orig_id, dest_id)]$V1
                         ][grid_data, on = c(dest_id = "id"), outside := i.opportunities
                           ][, .(outside = sum(outside)), keyby = orig_id]
      
      total_opportunities[outside, on = c(id = "orig_id"), outside := i.outside
                          ][, total := rowSums(.SD, na.rm = TRUE), .SDcols = c("inside", "outside")]
      
      data.table(
        id               = total_opportunities$id,
        bilhete_unico    = wt,
        travel_time      = tt,
        min_wage_percent = mw,
        accessibility    = total_opportunities$total
      )
      
    }))
    
    # save accessibility results in a separate folder
    
    accessibility_folder <- paste0(router_folder, "/accessibility")
    
    if (!file.exists(accessibility_folder)) dir.create(accessibility_folder)
    
    dep_time <- stringr::str_extract(itineraries_paths[i], "\\d{4}(am|pm)")
    
    fwrite(accessibility, paste0(accessibility_folder, "/accessibility_", dep_time, ".csv"))
    
  }
  
}


calculate_costs <- function(itineraries_details, routes_info, fare_schema, n_cores) {
  
  # join routes_info based on route_id, then nest legs details from each
  # origin into a data.table
  
  legs <- data.table::copy(itineraries_details
  )[, route_id := as.integer(fifelse(is.na(route_id), "0", stringr::str_extract(route_id, "\\d+$")))
    ][routes_info, on = "route_id", `:=`(type = i.type, price = i.price, price_bu = i.price_bu)
      ][, .(orig_id, leg_id, type, price, price_bu)
        ][, .(legs = list(.SD)), by = orig_id]
  
  # calculate the monetary cost of each leg and save it as a data.table
  
  future::plan(future::multisession, workers = n_cores)
  
  leg_cost_with_BU <- furrr::future_map(legs$legs, calculate_fare, fare_schema, BU = TRUE, .progress = TRUE)
  leg_cost_with_BU <- data.table::rbindlist(leg_cost_with_BU)
  data.table::setnames(leg_cost_with_BU, "leg_cost", "leg_cost_with_BU")
  
  leg_cost_without_BU <- furrr::future_map(legs$legs, calculate_fare, fare_schema, BU = FALSE, .progress = TRUE)
  leg_cost_without_BU <- data.table::rbindlist(leg_cost_without_BU)
  data.table::setnames(leg_cost_without_BU, "leg_cost", "leg_cost_without_BU")
  
  future::plan(future::sequential)
  
  # bind monetary cost data.table to itineraries_details and calculate total
  # cost (travel time and monetary) for each itinerary
  
  costs <- cbind(itineraries_details, leg_cost_with_BU, leg_cost_without_BU
  )[, travel_time := as.numeric(lubridate::as.duration(itinerary_end_time - itinerary_start_time), "minutes")
    ][, 
      .(travel_time = mean(travel_time), 
        cost_with_BU = sum(leg_cost_with_BU), 
        cost_without_BU = sum(leg_cost_without_BU)), 
      by = .(orig_id, dest_id, it_id)]
  
  return(costs)
  
}


calculate_fare <- function(legs, fare_schema, BU = TRUE) {
  
  # if BU argument has been set to FALSE the function calculates the fare as if there isn't any 
  # type of discounts, so the special fares are filtered out of the fare_schema dataframe
  
  if (!BU) fare_schema <- fare_schema %>% filter(need_bu == FALSE)
  
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
    
    leg_id <- paste0(last_moto, "&", legs$type[i])
    
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
      
      if (BU && !had_special && !has_integrated && legs$type[i] %chin% c("barca", "onibus_intermunicipal") && legs$price_bu[i] < legs$price[i]) {
        leg_cost[i] <- legs$price_bu[i]
        had_special <- TRUE
      }
      else leg_cost[i] <- legs$price[i]
      
      # check for the case where a specially priced leg is accounted as an integration
      
      if (legs$type[i] != "caminhada" && had_special && !has_integrated && last_moto %chin% c("barca", "onibus_intermunicipal")) {
        has_integrated <- TRUE
      }
      
    }
    
    else {
      
      # if a integration exists, it can fall within 4 categories:
      # * the integration doesn't require BU, so the current leg price is 0 (equivalent to the first
      # expression since int_fare[leg_id] and legs$price[i] are the same)
      # * an integration has happenned before already, in which case the full leg price is charged
      # * an integration has not occurred and the sum of the fares of the two modes is higher than the
      # integration fare, in which case the excess of the integration fare compared to the last motorised
      # leg price is charged
      # * an integration has not occurred and the sum of the fares that compose it is lower than the
      # integration fare (some intermunicipal buses cost less than 4.50, for instance, so the sum of their
      # fares and the municipal buses fares fall below 8.55, the integration cost), in which case the
      # mode fare is charged and the integration is not actually accounted
      
      if (!int_need_bu[leg_id]) leg_cost[i] <- int_fare[leg_id] - legs$price[i]
      
      else if (has_integrated) leg_cost[i] <- legs$price[i]
      
      else {
        
        if (price_last_moto + legs$price[i] > int_fare[leg_id]) {
          leg_cost[i] <- int_fare[leg_id] - price_last_moto
          has_integrated <- TRUE
        }
        
        else leg_cost[i] <- legs$price[i]
        
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
    
  }
  
  list(leg_cost = leg_cost)
  
}


# ROUTES_INFO_RELATED -----------------------------------------------------


generate_routes_info <- function(router) {
  
  # find fetranspor related gtfs file name
  filenames <- list.files(paste0("./otp/graphs/", router))
  fetranspor_name <- filenames[grep("fetranspor", filenames)]
  fetranspor_name <- gsub("gtfs_", "", fetranspor_name)
  fetranspor_name <- gsub(".zip" , "", fetranspor_name)
  
  fetranspor_info <- raw_routes_info(router, fetranspor_name)
  supervia_info <- raw_routes_info(router, "supervia")
  
  # the buses below have a specific fare integration with the subway
  
  integration_metro_bus <- c(513, 603, 608, 605, 609, 209, 611, 614, 616, 913, 876)
  
  # create a type column to be used when calculating the fares
  # update the price column (and price_bu, since some modes have distinct prices with BU) to match the 2020 fares
  
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
    ) %>% 
    update_fares() %>% 
    mutate(
      price = ifelse(route_id == 19209271 | route_id == 19132687, 10, price), # two random intermunicipal bus lines with no fare
      price_bu = ifelse(type == "barca", 6.3, price),
      price_bu = ifelse(type == "onibus_intermunicipal" & price > 8.55, 8.55, price_bu)
    ) %>% 
    bind_rows(tibble::tibble(route_id = 0, fare_id = 0, route_short_name = "caminhada", route_long_name = "caminhada", price = 0, type = "caminhada", price_bu = 0))
  
  routes_info
  
}

raw_routes_info <- function(router, holder, buffer_dist = 0) {
  
  rio <- readr::read_rds("./data/rio_municipality.rds") %>% 
    st_transform(5880) %>% 
    st_buffer(dist = buffer_dist)
  
  zip_filepath <- paste0("./otp/graphs/", router, "/gtfs_", holder, ".zip")
  
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
    #st_transform(5880) %>% 
    #filter(st_intersects(., rio, sparse = FALSE)) %>% 
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
  
  if (grepl("fetranspor", style)) {
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

update_fares <- function(routes_info) {
  
  # both gtfs' fares are outdated. this function updates them according to the 2020 fares
  # intermunicipal buses' fares were updated individually and compiled into a csv (values in http://www.detro.rj.gov.br/uploads/2020/AnexoPortariaDETRO-PRES.1513.pdf)
  # 'outros' category was left as is since it would require to research into many cities' different databases/laws and they barely affect the final result 
  
  intermunicipal_fares <- readr::read_csv("./data/tarifas_intermunicipais_2020.csv") %>% 
    select(-c(fare_id, route_short_name, route_long_name)) %>% 
    rename(intermunicipal_fare = actual_price)
  
  # intermunicipal buses costing less than 4.05 (municipal buses' fare) were also updated to 4.05, even if
  # not in the pdf linked previously, because no intermunicipal bus costs less than a municipal bus 
  
  routes_info <- routes_info %>% 
    left_join(intermunicipal_fares, by ="route_id") %>% 
    mutate(
      actual_price = ifelse(type == "barca", 6.50, NA),
      actual_price = ifelse(type %in% c("brt", "onibus_metro", "onibus_municipal"), 4.05, actual_price),
      actual_price = ifelse(type == "metro", 5.00, actual_price),
      actual_price = ifelse(type == "trem", 4.70, actual_price),
      actual_price = ifelse(type == "vlt", 3.80, actual_price),
      actual_price = ifelse(type %in% c("onibus_intermunicipal", "frescao_intermunicipal"), intermunicipal_fare, actual_price),
      actual_price = ifelse(type == "onibus_intermunicipal" & price < 4.05 & is.na(actual_price), 4.05, actual_price),
      actual_price = ifelse(is.na(actual_price), price, actual_price),
      price = actual_price
    ) %>% 
    select(-intermunicipal_fare, -actual_price)
  
  routes_info
}


# FARE_SCHEMA RELATED -----------------------------------------------------


generate_fare_schema <- function() {
  
  # fares and fare schema can be consulted at https://www.cartaoriocard.com.br/rcc/institucional/tarifas
  # need_bu column is TRUE when the fare is only valid when using BU. else, there is full integration (you
  # don't need to leave the station to hop in another route), so the integration fare equals the mode's
  # price, with no further additional
  
  fare_schema <- tibble::tribble(
    ~leg_1,                  ~leg_2,                  ~fare, ~need_bu,
    "brt",                   "brt",                   4.05,  FALSE,
    "metro",                 "metro",                 5.00,  FALSE,
    "trem",                  "trem",                  4.70,  FALSE,
    "onibus_municipal",      "onibus_municipal",      4.05,  TRUE,
    "onibus_municipal",      "brt",                   4.05,  TRUE,
    "brt",                   "onibus_municipal",      4.05,  TRUE,
    "onibus_municipal",      "vlt",                   4.05,  TRUE,
    "vlt",                   "onibus_municipal",      4.05,  TRUE,
    "onibus_municipal",      "onibus_metro",          4.05,  TRUE,
    "onibus_metro",          "onibus_municipal",      4.05,  TRUE,
    "vlt",                   "vlt",                   3.80,  TRUE,
    "onibus_metro",          "metro",                 6.05,  TRUE,
    "metro",                 "onibus_metro",          6.05,  TRUE,
    "onibus_intermunicipal", "onibus_municipal",      8.55,  TRUE,
    "onibus_municipal",      "onibus_intermunicipal", 8.55,  TRUE,
    "onibus_intermunicipal", "barca",                 8.55,  TRUE,
    "barca",                 "onibus_intermunicipal", 8.55,  TRUE,
    "brt",                   "metro",                 7.10,  TRUE,
    "metro",                 "brt",                   7.10,  TRUE,
    "trem",                  "metro",                 8.55,  TRUE,
    "metro",                 "trem",                  8.55,  TRUE,
    "vlt",                   "onibus_intermunicipal", 8.55,  TRUE,
    "onibus_intermunicipal", "vlt",                   8.55,  TRUE,
    "vlt",                   "barca",                 8.55,  TRUE,
    "barca",                 "vlt",                   8.55,  TRUE,
    "onibus_intermunicipal", "metro",                 8.55,  TRUE,
    "metro",                 "onibus_intermunicipal", 8.55,  TRUE,
    "onibus_intermunicipal", "trem",                  8.55,  TRUE,
    "trem",                  "onibus_intermunicipal", 8.55,  TRUE)
  
  fare_schema <- fare_schema %>% mutate(int_id = stringr::str_c(leg_1, "&", leg_2))
  
  fare_schema
  
}


# MISC --------------------------------------------------------------------


smallest_cost <- function(id, 
                          n_cores, 
                          grid_data, 
                          dep_time = "07:00am", 
                          router = "rio", 
                          res = 8) {
  
  routes_info <- setDT(generate_routes_info(router))
  fare_schema <- generate_fare_schema()
  
  dep_time <- gsub(":", "", dep_time)
  
  itineraries_path <- paste0(
    "./data/", router, "_res_", res, 
    "/itineraries/itineraries_", dep_time, ".rds"
  )
      
  itineraries_details <- data.table::setDT(readr::read_rds(itineraries_path))
  itineraries_details <- itineraries_details[orig_id == id]
  
  costs <- calculate_costs(itineraries_details, routes_info, fare_schema, n_cores)
  setkey(costs, cost_with_BU)
  
  costs <- costs[costs[, .I[1], by = .(dest_id)]$V1]
  
  grid_data <- grid_data %>% 
    left_join(costs, by = c("id" = "dest_id"))
      
}
