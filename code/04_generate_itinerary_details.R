library(dplyr)
library(sf)

generate_itinerary_details <- function(res = 7) {
  
  parameters <- list(
    mode = "TRANSIT,WALK",
    date = "01-01-2020",
    time = "08:00am",
    arriveBy = "FALSE",
    maxWalkDistance = "2000",
    numItineraries = "3"
  )
  
  itineraries_list <- get_transit_itineraries(parameters, res)
  
  desired_leg_details <- c("startTime","endTime", "distance", "mode", "routeId", "route")
  
  itineraries_details <- extract_itinerary_details(itineraries_list, desired_leg_details)
  
  readr::write_rds(itineraries_details, stringr::str_c("./data/itineraries_details_res_", res, ".rds"))
  
}

get_transit_itineraries <- function(parameters, res = 7) {
  
  rio_centroids_coordinates <- readr::read_rds(stringr::str_c("./data/rio_h3_grid_res_", res, "_with_data.rds")) %>%
    filter(opportunities != 0 | population != 0) %>% 
    st_transform(5880) %>% 
    st_centroid() %>% 
    st_transform(4674) %>% 
    st_coordinates() %>% 
    tibble::as_tibble() %>% 
    tibble::rowid_to_column("id") %>% 
    mutate(lat_lon = stringr::str_c(Y, ",", X)) %>% 
    select(-X, -Y) %>% 
    head(10)
  
  # opentripplanner::otp_setup(otp = "./otp/otp.jar", dir = "./otp", memory = 2048, router = "rio")
  
  n <- nrow(rio_centroids_coordinates)
  
  future::plan(future::multiprocess)
  
  responses <- furrr::future_map(1:n, make_request, rio_centroids_coordinates, parameters, .progress = TRUE) %>% 
    purrr::flatten()
  
  # opentripplanner::otp_stop(warn = FALSE)
  
  responses
  
}

make_request <- function(x, od_points, parameters) {
  
  orig <- od_points[x, ]
  parameters$fromPlace <- orig$lat_lon
  
  n <- nrow(od_points)
  response_list <- vector("list", length = n)
  
  for (i in 1:n) {
    
    parameters$toPlace <- od_points[i, ]$lat_lon
    
    request_url <- httr::parse_url("http://localhost:8080/otp/routers/rio/plan/")
    request_url$query <- parameters
    request_url <- httr::build_url(request_url)
    
    res <- httr::GET(request_url) %>% httr::content(as = "text", encoding = "UTF-8") %>% jsonlite::fromJSON()
    res$identification <- list(orig_id = orig$id, dest_id = od_points[i, ]$id)
    
    response_list[[i]] <- res
    
  }
  
  response_list
  
}

extract_itinerary_details <- function(itineraries_list, leg_details) {
  
  n <- length(itineraries_list)
  
  future::plan(future::multiprocess, workers = future::availableCores() - 1)
  
  furrr::future_map_dfr(itineraries_list, itinerary_details_to_df, leg_details) %>% 
    select(
      all_of(names(.)[! names(.) %in% c("leg_id", leg_details)]),
      all_of(c("leg_id", leg_details))
    ) %>%
    rename(leg_start_time = startTime, leg_end_time = endTime, route_id = routeId) %>% 
    mutate_at(
      vars(ends_with("time")),
      list(~ lubridate::as_datetime(as.double(.) / 1000, tz = "America/Sao_Paulo"))
    ) %>% 
    select_unique_itineraries()
  
}

itinerary_details_to_df <- function(itineraries_list, leg_details) {
  
  orig_id <- itineraries_list$identification$orig_id
  dest_id <- itineraries_list$identification$dest_id
  
  if (!is.null(itineraries_list$error)) {
    
    error_id <- itineraries_list$error$id
    error_msg <- itineraries_list$error$msg

    itineraries_details <- tibble::tibble(orig_id = orig_id, dest_id = dest_id,
                                          error_id = error_id, error_msg = error_msg)
    
    return(itineraries_details)
    
  }
  
  itinerary_start_time <- itineraries_list$plan$date
  
  itineraries <- itineraries_list$plan$itineraries
  n <- nrow(itineraries)
  
  itineraries_details <- NULL
  
  for (i in 1:n) {
    
    itinerary_end_time <- itineraries$endTime[[i]]
    
    legs <- itineraries$legs[[i]]
    nl <- nrow(legs)
    
    legs$leg_id <- 1:nl
    if (is.null(legs$routeId)) legs$routeId <- NA
    
    legs <- legs[c("leg_id", leg_details)] %>% 
      bind_cols(
        tibble(orig_id = rep(orig_id, nl),
               dest_id = rep(dest_id, nl),
               it_id = rep(i, nl),
               itinerary_start_time = rep(itinerary_start_time, nl),
               itinerary_end_time = rep(itinerary_end_time, nl)
        )
      )
    
    itineraries_details <- bind_rows(itineraries_details, legs)
    
  }
  
  itineraries_details
  
}

select_unique_itineraries <- function(itineraries_details) {
  
  # sometimes OTP returns multiple identical itineraries for each OD pair, but at different times
  # this issue gets more relevant when you increase the numItineraries parameter sent in request
  
  # this function selects only unique itineraries for each O-D pair
  # the legs' start and end time are disregarded, and the route, distance and mode are compared
  
  # in this case, leg_start_time and leg_end_time are thrown away
  # doing this makes the function much faster. keeping these columns imply in creating a temporary
  # column with map() in order to use a copy of 'data' with no legs' start and end time to compare
  # itineraries, which is very slow
  #### e.g. mutate(temp = map(data, function(i) select(i, -leg_start_time, -leg_end_time)))
  
  itineraries_details <- itineraries_details %>%
    
    # filter out errors, select relevant columns and nest each itinerary's legs details in a df
    filter(is.na(error_id)) %>% 
    select(-error_id, -error_msg, -leg_start_time, -leg_end_time) %>% 
    group_by(orig_id, dest_id, it_id, itinerary_start_time, itinerary_end_time) %>% 
    tidyr::nest() %>% 
    
    # filter out non unique itineraries (within each OD pair)
    group_by(orig_id, dest_id) %>% 
    filter(!duplicated(data)) %>% 
    
    # format back to how it used to be
    ungroup() %>% 
    tidyr::unnest(data)
  
  itineraries_details
  
}



get_transit_itineraries2 <- function(parameters, res = 7) {
  
  rio_centroids_coordinates <- readr::read_rds(stringr::str_c("./data/rio_h3_grid_res_", res, "_with_data.rds")) %>%
    filter(opportunities != 0 | population != 0) %>% 
    st_transform(5880) %>% 
    st_centroid() %>% 
    st_transform(4674) %>% 
    st_coordinates() %>% 
    tibble::as_tibble() %>% 
    tibble::rowid_to_column("id") %>% 
    mutate(lat_lon = stringr::str_c(Y, ",", X)) %>% 
    select(-X, -Y) %>% 
    head(10)
  
  # opentripplanner::otp_setup(otp = "./otp/otp.jar", dir = "./otp", memory = 2048, router = "rio")
  
  n <- nrow(rio_centroids_coordinates)
  
  responses <- purrr::map(1:n, make_request, rio_centroids_coordinates, parameters) %>% 
    purrr::flatten()
  
  # opentripplanner::otp_stop(warn = FALSE)
  
  responses
  
}











get_transit_itineraries3 <- function(parameters, res = 7) {
  
  rio_centroids_coordinates <- readr::read_rds(stringr::str_c("./data/rio_h3_grid_res_", res, "_with_data.rds")) %>%
    filter(opportunities != 0 | population != 0) %>% 
    st_transform(5880) %>% 
    st_centroid() %>% 
    st_transform(4674) %>% 
    st_coordinates() %>% 
    tibble::as_tibble() %>% 
    tibble::rowid_to_column("id") %>% 
    mutate(lat_lon = stringr::str_c(Y, ",", X)) %>% 
    select(-X, -Y) %>% 
    head(10)
  
  # opentripplanner::otp_setup(otp = "./otp/otp.jar", dir = "./otp", memory = 2048, router = "rio")
  
  n <- nrow(rio_centroids_coordinates)
  
  responses <- make_request3(rio_centroids_coordinates, parameters)
  
  # opentripplanner::otp_stop(warn = FALSE)
  
  responses
  
}

make_request3 <- function(od_points, parameters) {
  
  n <- nrow(od_points)
  
  urls <- vector("character", length = n * n)
  identification <- vector("list", length = n * n)
  
  c <- 1
  for (i in 1:n) {
    
    for(j in 1:n) {
    
      parameters$fromPlace <- od_points[i, ]$lat_lon
      
      parameters$toPlace <- od_points[j, ]$lat_lon
      
      request_url <- httr::parse_url("http://localhost:8080/otp/routers/rio/plan/")
      request_url$query <- parameters
      request_url <- httr::build_url(request_url)
      
      urls[[c]] <- request_url
      
      identification[[c]]$origin_id <- od_points[i, ]$id
      identification[[c]]$dest_id <- od_points[j, ]$id
      
      c <- c + 1
    
    }
    
  }
  
  requests <- crul::Async$new(urls = urls)
  
  response_list <- requests$get() %>% 
    lapply(function(z) z$parse("UTF-8")) %>% 
    lapply(function(z) jsonlite::fromJSON(z))
  
  for (i in seq_along(response_list)) {
    response_list[[i]]$identification <- identification[[i]]
  }
  
  response_list
  
}