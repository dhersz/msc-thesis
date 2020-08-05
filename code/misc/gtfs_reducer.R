generate_smaller_gtfs <- function(holder = "fetranspor") {
  
  # path to the original big gtfs file
  
  original_gtfs_filepath <- stringr::str_c("./otp/graphs/rio_old/gtfs_", holder, ".zip")
  
  # extract the id of routes within a given distance from rio municipality
  
  lines_within_rio_mun <- raw_routes_info(holder, buffer_dist = 1000)
  
  relevant_route_ids <- lines_within_rio_mun$route_id
  
  ########################################################################
  # edit each gtfs file to keep only the entries related to these routes #
  ########################################################################
  
  if(!file.exists("./otp/graphs/rio/gtfs_fetranspor_reduced")) dir.create("./otp/graphs/rio/gtfs_fetranspor_reduced")
  
  # routes.txt
  
  readr::read_csv(unz(original_gtfs_filepath, "routes.txt")) %>% 
    filter(route_id %in% relevant_route_ids) %>% 
    readr::write_csv("./otp/graphs/rio/gtfs_fetranspor_reduced/routes.txt")
  
  # fare_rules.txt
  
  fare_rules <- readr::read_csv(unz(original_gtfs_filepath, "fare_rules.txt")) %>% 
    filter(route_id %in% relevant_route_ids)
  
  readr::write_csv(fare_rules, "./otp/graphs/rio/gtfs_fetranspor_reduced/fare_rules.txt")
  
  relevant_fare_ids <- unique(fare_rules$fare_id)
  
  # fare_attributes.txt
  
  readr::read_csv(unz(original_gtfs_filepath, "fare_attributes.txt")) %>% 
    filter(fare_id %in% relevant_fare_ids) %>% 
    readr::write_csv("./otp/graphs/rio/gtfs_fetranspor_reduced/fare_attributes.txt")
  
  # trips.txt
  
  trips <- readr::read_csv(unz(original_gtfs_filepath, "trips.txt")) %>% 
    filter(route_id %in% relevant_route_ids)
  
  readr::write_csv(trips, "./otp/graphs/rio/gtfs_fetranspor_reduced/trips.txt")
  
  relevant_service_ids <- unique(trips$service_id)
  relevant_trip_ids <- unique(trips$trip_id)
  relevant_shape_ids <- unique(trips$shape_id)
  
  # calendar.txt
  
  readr::read_csv(unz(original_gtfs_filepath, "calendar.txt")) %>% 
    filter(service_id %in% relevant_service_ids) %>% 
    readr::write_csv("./otp/graphs/rio/gtfs_fetranspor_reduced/calendar.txt")
  
  # calendar_dates.txt
  
  readr::read_csv(unz(original_gtfs_filepath, "calendar_dates.txt")) %>% 
    filter(service_id %in% relevant_service_ids) %>% 
    readr::write_csv("./otp/graphs/rio/gtfs_fetranspor_reduced/calendar_dates.txt")
  
  # frequencies.txt
  
  readr::read_csv(unz(original_gtfs_filepath, "frequencies.txt"), col_types = "cccdl") %>% 
    filter(trip_id %in% relevant_trip_ids) %>% 
    readr::write_csv("./otp/graphs/rio/gtfs_fetranspor_reduced/frequencies.txt")
  
  # shapes.txt
  
  readr::read_csv(unz(original_gtfs_filepath, "shapes.txt")) %>% 
    filter(shape_id %in% relevant_shape_ids) %>% 
    readr::write_csv("./otp/graphs/rio/gtfs_fetranspor_reduced/shapes.txt")
  
  # stop_times.txt
  
  stop_times <- readr::read_csv(unz(original_gtfs_filepath, "stop_times.txt"), col_types = "cccddcccc") %>% 
    filter(trip_id %in% relevant_trip_ids)
  
  readr::write_csv(stop_times, "./otp/graphs/rio/gtfs_fetranspor_reduced/stop_times.txt")
  
  relevant_stop_ids <- unique(stop_times$stop_id)
  
  # stops.txt
  
  readr::read_csv(unz(original_gtfs_filepath, "stops.txt")) %>% 
    filter(stop_id %in% relevant_stop_ids) %>% 
    readr::write_csv("./otp/graphs/rio/gtfs_fetranspor_reduced/stops.txt")
  
  # agency.txt and feed_info.txt go unchanged
  
  readr::read_csv(unz(original_gtfs_filepath, "agency.txt")) %>% 
    readr::write_csv("./otp/graphs/rio/gtfs_fetranspor_reduced/agency.txt")
  
  readr::read_csv(unz(original_gtfs_filepath, "feed_info.txt")) %>% 
    readr::write_csv("./otp/graphs/rio/gtfs_fetranspor_reduced/feed_info.txt")
  
  ######################
  # now zip everything #
  ######################
  
  original_wd <- getwd()
  
  setwd("./otp/graphs/rio/gtfs_fetranspor_reduced/")
  
  zip(stringr::str_c(original_wd, "/otp/graphs/rio/gtfs_fetranspor_reduced"), files = list.files())
  
  setwd(original_wd)
  
  unlink("./otp/graphs/rio/gtfs_fetranspor_reduced", recursive = TRUE)
  
}

raw_routes_info <- function(holder, buffer_dist = 0) {
  
  rio <- readr::read_rds("./data/rio_municipality.rds") %>% 
    st_transform(5880) %>% 
    st_buffer(dist = buffer_dist)
  
  zip_filepath <- stringr::str_c("./otp/graphs/rio_old/gtfs_", holder, ".zip")
  
  required_files <- c("routes.txt", "shapes.txt", "trips.txt")
  
  files_list <- setNames(
    purrr::map(required_files, function(i) readr::read_csv(unz(zip_filepath, i))),
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
    left_join(shapes_treatment(shapes, 4674), by = "shape_id") %>% 
    st_as_sf() %>% 
    st_transform(5880) %>% 
    filter(st_intersects(., rio, sparse = FALSE)) %>% 
    st_drop_geometry() %>% 
    distinct(route_id, route_short_name, route_long_name)
  
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

shapes_treatment <- function(shapes, crs = 4674) {
  
  shapes <- shapes %>% 
    group_by(shape_id) %>% 
    summarise(coord_matrix = list(matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2)), .groups = "drop")
  
  sfc <- st_multilinestring(shapes$coord_matrix) %>%
    st_sfc() %>%
    st_cast("LINESTRING")
  
  shapes <- shapes %>%
    select(-coord_matrix) %>%
    st_sf(geom = sfc) %>%
    st_set_crs(crs)
  
  shapes
  
}