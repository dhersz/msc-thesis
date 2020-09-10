library(dplyr)
library(sf)

# before using these functions, specifically get_transit_itineraries, it's
# important to start the OTP server. to do so, one could use:
#    opentripplanner::otp_setup(otp = "./otp/otp.jar", dir = "./otp", memory = 2048, router = "rio")
#    opentripplanner::otp_stop(warn = FALSE)
# but unfortunately that wasn't really working reliably, sometimes java would
# crash, not sure why.

generate_itinerary_details <- function(dyn = FALSE,
                                       dep_time = "08:00am",
                                       groups_size = 1,
                                       n_instances = 1,
                                       n_cores = 3L,
                                       res = 7,
                                       router = "rio") {

  # set data.table options
  
  old_dt_threads <- data.table::getDTthreads()
  on.exit(data.table::setDTthreads(threads = old_dt_threads))
  
  data.table::setDTthreads(threads = n_cores)
  
  # list of parameters sent to the OTP api
  parameters <- list(
    mode = "TRANSIT,WALK",
    date = "01-08-2020",
    time = dep_time,
    arriveBy = "FALSE",
    numItineraries = "10"
    # ,maxWalkDistance = "2000"
    # ,maxHours = "1"
    # ,useRequestedDateTimeInMaxHours = "TRUE"
  )
  
  # replace ':' in dep_time to later use it in the files names
  dep_time <- gsub(":", "", dep_time)

  # set of leg details important for accessibility analysis
  leg_details <- c("startTime","endTime", "distance", "mode", "routeId", "route")

  # calculate the centroids coordinates of the grid with resolution 'res'.
  # cells with no opportunities and population are cleaned out of the dataframe
  # to speed up the requests, since they won't affect accessibility.

  clean_grid <- readr::read_rds(paste0("./data/rio_h3_grid_res_", res, "_with_data.rds")) %>%
    filter(opportunities != 0 | population != 0)
  
  if (res == 8) {
    
    clean_grid <- readr::read_rds("../../data/acesso_oport/hex_agregados/2019/hex_agregado_rio_08_2019.rds") %>% 
      filter(empregos_total != 0 | pop_total != 0) %>% 
      rename(id = id_hex)
    
  }

  clean_grid_cell_ids <- clean_grid$id

  centroids_coordinates <- clean_grid %>%
    st_transform(5880) %>%
    st_centroid() %>%
    st_transform(4674) %>%
    st_coordinates() %>%
    tibble::as_tibble() %>%
    tibble::add_column(id = clean_grid_cell_ids) %>%
    mutate(lat_lon = paste0(Y, ",", X)) %>%
    select(-X, -Y) %>% 
    cbind(numeric_id = 1:nrow(.))

  # calculate groups of centroids to be sent in bashes to each core

  num_ids <- centroids_coordinates$numeric_id

  origin_groups <- split(num_ids, ceiling(num_ids / groups_size))
  
  names(origin_groups) <- lapply(
    origin_groups, 
    function(i) {
      ifelse(
        length(i) == 1, 
        as.character(i), 
        paste0(i[1], "_to_", i[length(i)])
      )
    }
  )

  # centroids_coordinates is sent to a function which calculates routes between
  # an origin (or a group of) and all possible destinations, then processes the
  # data from a list into a dataframe and saves it in a temporary folder.

  if (!file.exists("./data/temp")) dir.create("./data/temp")

  future::plan(future::multisession, workers = n_cores)

  invisible(
    furrr::future_map(
      1:length(origin_groups),
      get_transit_itineraries,
      origin_groups,
      centroids_coordinates,
      parameters,
      leg_details,
      n_instances,
      n_cores,
      dyn,
      dep_time,
      res,
      .progress = TRUE
    )
  )

  # read each dataframe from the temporary folder into a list and bind
  # everything together. then tidy the resulting dataframe (format columns,
  # arrange rows, etc) and save it in a separate folder

  # _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  # IMPORTANT: tidy_itineraries() removes errors rows _/
  # _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/

  folder_path <- paste0("./data/", router, "_res_", res)
  if (!file.exists(folder_path)) dir.create(folder_path)
  
  subfolder_path <- paste0("./data/", router, "_res_", res, "/itineraries")
  if (!file.exists(subfolder_path)) dir.create(subfolder_path)
  
  temp_files_path <- paste0("./data/temp/itineraries_details_orig_",
                            names(origin_groups),"_res_", res, "_", dep_time,
                            ".rds")
  
  furrr::future_map(temp_files_path, readr::read_rds) %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidy_itineraries(leg_details, res) %>%
    readr::write_rds(paste0(subfolder_path, "/itineraries_", dep_time, ".rds"), compress = "gz")

  # close multisession workers by switching plan
  future::plan(future::sequential)

  # delete the temporary folder
  # unlink("./data/temp", recursive = TRUE)

}



get_transit_itineraries <- function(x, 
                                    origin_groups, 
                                    od_points, 
                                    parameters, 
                                    leg_details, 
                                    n_instances, 
                                    n_cores, 
                                    dyn, 
                                    dep_time, 
                                    res) {

  # this function calls the two most important functions in this whole ensemble.
  # make_request sends requests to the OTP API and returns its responses.
  # extract_itinerary_details takes these responses and processes their data
  # from a list into a dataframe.
  # this dataframe is then saved inside a temporary folder that contains the
  # itineraries details from each single origin of the origin groups to all
  # destinations.

  origin_group <- origin_groups[x]
  
  filepath <- paste0("./data/temp/itineraries_details_orig_",
                     names(origin_group),"_res_", res, "_", dep_time, ".rds")

  make_request(x, origin_group, od_points, parameters, n_instances, dyn) %>%
    purrr::map_dfr(extract_itinerary_details, leg_details) %>%
    readr::write_rds(filepath, compress = "gz")

  origin_group

}



make_request <- function(x,
                         origin_group,
                         od_points,
                         parameters,
                         n_instances,
                         dyn) {
  
  origins_pool <- od_points[origin_group[[1]], ]

  request_url <- httr::parse_url(paste0("http://localhost:", 8090 + x %% n_instances, "/otp/routers/rio/plan/"))

  # iterate through origins_pool and use each entry as an origin

  response_list <- vector("list", length = nrow(origins_pool) * nrow(od_points))

  cont <- 1
  for (i in 1:nrow(origins_pool)) {

    # use origin lat_lon as the fromPlace parameter sent to the API

    parameters$fromPlace <- origins_pool[i, ]$lat_lon

    # iterate through each centroid and use them as a destination
    # process it from a json to a list and add an identification nested list

    for (j in 1:nrow(od_points)) {
      
      if (dyn) request_url <- httr::parse_url(paste0("http://localhost:", 8090 + cont %% n_instances, "/otp/routers/rio/plan/"))

      parameters$toPlace <- od_points[j, ]$lat_lon

      request_url <- httr::modify_url(request_url, query = parameters)

      res <- httr::GET(request_url) %>% httr::content(as = "text", encoding = "UTF-8") %>% jsonlite::fromJSON()
      res$identification <- list(orig_id = origins_pool[i, ]$id, dest_id = od_points[j, ]$id)

      response_list[[cont]] <- res

      cont <- cont + 1

    }

  }

  response_list
  
}



extract_itinerary_details <- function(itineraries_list, leg_details) {

  # save origin and destination id for easier cell identification later on

  orig_id <- itineraries_list$identification$orig_id
  dest_id <- itineraries_list$identification$dest_id

  # check if request has thrown an error - if positive return a tibble with error id and message

  if (!is.null(itineraries_list$error)) {

    error_id <- itineraries_list$error$id
    error_msg <- itineraries_list$error$msg

    itineraries_details <- data.frame(orig_id = orig_id, dest_id = dest_id, error_id = error_id, error_msg = error_msg)

    return(itineraries_details)

  }

  # if the request hasn't thrown an error, an itinerary has successfully been calculated

  # the clampInitialWait API parameter is not working properly, not sure why. sometimes it clamps all the wait, sometimes it doesn't.
  # so the itinerary start time is saved as the time sent as the departure time set as the parameter, and not the first leg start time

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

    legs <- legs[c("leg_id", leg_details)]
    legs <- cbind(legs, orig_id = orig_id, dest_id = dest_id, it_id = i, itinerary_start_time = itinerary_start_time, itinerary_end_time = itinerary_end_time)

    itineraries_details <- bind_rows(itineraries_details, legs)

  }

  itineraries_details

}



tidy_itineraries <- function(itineraries_details, leg_details, res){

  # select itinerary-level columns and place to the left of leg-level columns
  # convert names from camelCase to snake_case
  # convert time columns from epoch to datetime
  # select unique itineraries

  itineraries_details <- itineraries_details %>%
    select(
      all_of(names(.)[! names(.) %in% c("leg_id", leg_details)]),
      all_of(c("leg_id", leg_details))
    ) %>%
    rename(leg_start_time = startTime, leg_end_time = endTime, route_id = routeId) %>%
    mutate_at(
      vars(ends_with("time")),
      list(~ lubridate::as_datetime(as.double(.) / 1000, tz = "America/Sao_Paulo"))
    ) %>%
    select_unique_itineraries(res)

  itineraries_details

}



select_unique_itineraries <- function(itineraries_details, res) {

  # sometimes OTP returns multiple identical itineraries for each OD pair, but at different times
  # this issue gets more relevant when you increase the numItineraries parameter sent in request

  # this function selects only unique itineraries for each O-D pair
  # the legs' start and end time are disregarded, and the route, distance and mode are compared

  # in this case, leg_start_time and leg_end_time are thrown away
  # doing this makes the function much faster. keeping these columns imply in creating a temporary
  # column with map() in order to use a copy of 'data' with no legs' start and end time to compare
  # itineraries, which is very slow
  #### e.g. mutate(temp = map(data, function(i) select(i, -leg_start_time, -leg_end_time)))

  # first save errors in another file

  errors <- itineraries_details %>%
    filter(!is.na(error_id)) %>%
    select(orig_id, dest_id, error_id, error_msg) %>%
    readr::write_rds(stringr::str_c("./data/itineraries_details_res_", res, "_errors.rds"))

  # filter out errors, select relevant columns and nest each itinerary's legs details in a df

  itineraries_details <- itineraries_details %>%
    filter(is.na(error_id)) %>%
    select(-error_id, -error_msg, -leg_start_time, -leg_end_time) %>%
    group_by(orig_id, dest_id, it_id, itinerary_start_time, itinerary_end_time) %>%
    tidyr::nest()

  # filter out non unique itineraries (within each OD pair) and format back to how it used to be

  itineraries_details <- itineraries_details %>%
    group_by(orig_id, dest_id) %>%
    filter(!duplicated(data)) %>%
    ungroup() %>%
    tidyr::unnest(data)

  itineraries_details

}



setup_otp <- function(n_instances) {

  future::plan(future::multisession, workers = n_instances)

  invisible(furrr::future_map(1:n_instances,
    function(i) {
      system2("java8",
        args = c("-Xmx4G",
                 "-jar", "otp/otp.jar",
                 "--server", "--graphs", "otp/graphs", "--router", "rio",
                 "--port", as.character(8090 + i-1),
                 "--securePort", as.character(8900 + i-1)),
        wait = FALSE)
    })
  )

  future::plan(future::sequential)

}



stop_otp <- function() {
  
  system("Taskkill /IM java8.exe /F", intern = TRUE)
  
}



timer <- function(dyn, dep_time, groups_size, n_instances, n_cores, res) {

  tictoc::tic()
  generate_itinerary_details(dyn, dep_time, groups_size, n_instances, n_cores, res)
  elapsed <-  tictoc::toc()

  elapsed$toc - elapsed$tic

}



times_dataset_builder <- function(n_list = c(1, 5, 10, 15)){

  n_instances <- 12
  n_cores <- 18
  res <- 7
  dyn <- 0
  dep_time <- "08:00am"
  # group_size <- 1
  
  test_repetitions <- 1
  
  setup_otp(n_instances)

  Sys.sleep(60)

  df <- data.frame(
    # n_instances = rep(n_list, times = test_repetitions),
    # dyn = rep(c(0, 1), times = test_repetitions),
    group_size = rep(n_list, times = test_repetitions),
    test_no = rep(1:test_repetitions, each = length(n_list)),
    running_time = rep(NA, times = length(n_list) * test_repetitions)
  )

  for (i in 1:nrow(df)) {

    case <- df[i, ]

    df$running_time[i] = timer(dyn = dyn, dep_time = dep_time, groups_size = case$group_size, n_instances = n_instances, n_cores = n_cores, res = res)

    readr::write_rds(df, "./data/times_dataset_temp.rds")

  }

  readr::write_rds(df, "./data/times_dataset_group_size_res_7.rds")

  file.remove("./data/times_dataset_temp.rds")

  stop_otp()

}



# NOTHING IS BEING USED BEYOND HERE ---------------------------------------

# #
# # I HAVE TO CLEAN THIS LATER #
# #
#
#
# generate_itinerary_details_alt <- function(n_cores = 3L, res = 7) {
#
#   # list of parameters sent to the OTP api
#
#   parameters <- list(
#     mode = "TRANSIT,WALK",
#     date = "01-08-2020",
#     time = "08:00am",
#     arriveBy = "FALSE",
#     maxWalkDistance = "2000",
#     numItineraries = "10"
#   )
#
#   # set of leg characteristics important for accessibility analysis
#
#   leg_details <- c("startTime","endTime", "distance", "mode", "routeId", "route")
#
#   # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#   # very important to change the number of cores accordingly to what is available #
#   # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#
#   itineraries_list <- get_transit_itineraries3(parameters, res)
#   #itineraries_list <- get_transit_itineraries_alt(parameters, leg_details, n_cores, res)
#
#   # readr::write_rds(itineraries_list, "./data/temp_itineraries_list.rds")
#
#   itineraries_details <- extract_itinerary_details_alt(itineraries_list, leg_details, n_cores)
#
#   # file.remove("./data/temp_itineraries_list.rds")
#
#   readr::write_rds(itineraries_details, stringr::str_c("./data/itineraries_details_res_", res, ".rds"))
#
# }
#
# get_transit_itineraries_alt <- function(parameters, leg_details, n_cores, res) {
#
#   # calculate the centroids coordinates of the grid with resolution 'res'
#   # cells with no opportunities and population are cleaned out of the dataframe to speed up the requests, since they won't affect accessibility
#
#   clean_grid <- readr::read_rds(stringr::str_c("./data/rio_h3_grid_res_", res, "_with_data.rds")) %>%
#     filter(opportunities != 0 | population != 0)
#
#   clean_grid_cell_ids <- clean_grid$id
#
#   centroids_coordinates <- clean_grid %>%
#     st_transform(5880) %>%
#     st_centroid() %>%
#     st_transform(4674) %>%
#     st_coordinates() %>%
#     tibble::as_tibble() %>%
#     tibble::add_column(id = clean_grid_cell_ids) %>%
#     mutate(lat_lon = stringr::str_c(Y, ",", X)) %>%
#     select(-X, -Y) %>%
#     head(10)
#
#   # centroids_coordinates is sent to a function which calculates a route between a specific origin and all possible destinations,
#   # then processes the data from a list into a dataframe and saves the final dataframe in a temporary folder
#   # STILL A WORK IN PROGRESS, NOT WHAT IS SHOWN BELOW
#
#   if(!file.exists("./data/temp")) dir.create("./data/temp")
#
#   future::plan(future::multisession, workers = n_cores)
#
#   n <- nrow(centroids_coordinates)
#   responses <- furrr::future_map(1:n, make_request_alt, centroids_coordinates, parameters, .progress = TRUE) %>%
#     purrr::flatten()
#
#   responses
#
# }
#
# make_request_alt <- function(x, od_points, parameters) {
#
#   orig <- od_points[x, ]
#   parameters$fromPlace <- orig$lat_lon
#
#   n <- nrow(od_points)
#
#   urls <- vector("character", length = n)
#   identification <- vector("list", length = n)
#
#   c <- 1
#   for (i in 1:n) {
#
#     parameters$toPlace <- od_points[i, ]$lat_lon
#
#     request_url <- httr::parse_url("http://localhost:8080/otp/routers/rio/plan/")
#     request_url$query <- parameters
#     request_url <- httr::build_url(request_url)
#
#     urls[[i]] <- request_url
#
#     identification[[i]]$orig_id <- orig$id
#     identification[[i]]$dest_id <- od_points[i, ]$id
#
#   }
#
#   requests <- crul::Async$new(urls = urls)
#
#   response_list <- requests$get() %>%
#     lapply(function(z) z$parse("UTF-8")) %>%
#     lapply(function(z) jsonlite::fromJSON(z))
#
#   for (i in seq_along(response_list)) {
#     response_list[[i]]$identification <- identification[[i]]
#   }
#
#   response_list
#
# }
#
# extract_itinerary_details_alt <- function(itineraries_list, leg_details, n_cores) {
#
#   future::plan(future::multisession, workers = n_cores)
#
#   furrr::future_map_dfr(itineraries_list, itinerary_details_to_df, leg_details) %>%
#     select(
#       all_of(names(.)[! names(.) %in% c("leg_id", leg_details)]),
#       all_of(c("leg_id", leg_details))
#     ) %>%
#     rename(leg_start_time = startTime, leg_end_time = endTime, route_id = routeId) %>%
#     mutate_at(
#       vars(ends_with("time")),
#       list(~ lubridate::as_datetime(as.double(.) / 1000, tz = "America/Sao_Paulo"))
#     ) %>%
#     select_unique_itineraries()
#
# }
#
# itinerary_details_to_df <- function(itineraries_list, leg_details) {
#
#   # save origin and destination id for easier cell identification later on
#
#   orig_id <- itineraries_list$identification$orig_id
#   dest_id <- itineraries_list$identification$dest_id
#
#   # check if request has thrown an error - if positive return a tibble with error id and message
#
#   if (!is.null(itineraries_list$error)) {
#
#     error_id <- itineraries_list$error$id
#     error_msg <- itineraries_list$error$msg
#
#     itineraries_details <- tibble::tibble(orig_id = orig_id, dest_id = dest_id,
#                                           error_id = error_id, error_msg = error_msg)
#
#     return(itineraries_details)
#
#   }
#
#   # if the request hasn't thrown an error, an itinerary has successfully been calculated
#
#   # the clampInitialWait API parameter is not working properly, not sure why. sometimes it clamps all the wait, sometimes it doesn't.
#   # so the itinerary start time is saved as the time sent as the departure time set as the parameter, and not the first leg start time
#
#   itinerary_start_time <- itineraries_list$plan$date
#
#   itineraries <- itineraries_list$plan$itineraries
#   n <- nrow(itineraries)
#
#   itineraries_details <- NULL
#
#   for (i in 1:n) {
#
#     itinerary_end_time <- itineraries$endTime[[i]]
#
#     legs <- itineraries$legs[[i]]
#     nl <- nrow(legs)
#
#     legs$leg_id <- 1:nl
#     if (is.null(legs$routeId)) legs$routeId <- NA
#
#     legs <- legs[c("leg_id", leg_details)] %>%
#       bind_cols(
#         tibble(orig_id = rep(orig_id, nl),
#                dest_id = rep(dest_id, nl),
#                it_id = rep(i, nl),
#                itinerary_start_time = rep(itinerary_start_time, nl),
#                itinerary_end_time = rep(itinerary_end_time, nl)
#         )
#       )
#
#     itineraries_details <- bind_rows(itineraries_details, legs)
#
#   }
#
#   itineraries_details
#
# }
#
#
#
# # ANOTHER APPROACH # THIS ONE IS ACTUALLY FASTER, BUT MUCH MORE MEMORY HUNGRY
# #################### not sure why, but it returns much less itineraries than the method being used right now...
#
#
# get_transit_itineraries3 <- function(parameters, res) {
#
#   clean_grid <- readr::read_rds(stringr::str_c("./data/rio_h3_grid_res_", res, "_with_data.rds")) %>%
#     filter(opportunities != 0 | population != 0)
#
#   clean_grid_cell_ids <- clean_grid$id
#
#   centroids_coordinates <- clean_grid %>%
#     st_transform(5880) %>%
#     st_centroid() %>%
#     st_transform(4674) %>%
#     st_coordinates() %>%
#     tibble::as_tibble() %>%
#     tibble::add_column(id = clean_grid_cell_ids) %>%
#     mutate(lat_lon = stringr::str_c(Y, ",", X)) %>%
#     select(-X, -Y) %>%
#     head(10)
#
#   responses <- make_request3(centroids_coordinates, parameters)
#
#   responses
#
# }
#
# make_request3 <- function(od_points, parameters) {
#
#   n <- nrow(od_points)
#
#   urls <- vector("character", length = n * n)
#   identification <- vector("list", length = n * n)
#
#   c <- 1
#   for (i in 1:n) {
#
#     for(j in 1:n) {
#
#       parameters$fromPlace <- od_points[i, ]$lat_lon
#
#       parameters$toPlace <- od_points[j, ]$lat_lon
#
#       request_url <- httr::parse_url(stringr::str_c("http://localhost:", 8080, "/otp/routers/rio/plan/"))
#       request_url$query <- parameters
#       request_url <- httr::build_url(request_url)
#
#       urls[[c]] <- request_url
#
#       identification[[c]]$orig_id <- od_points[i, ]$id
#       identification[[c]]$dest_id <- od_points[j, ]$id
#
#       c <- c + 1
#
#     }
#
#   }
#
#   requests <- crul::Async$new(urls = urls)
#
#   response_list <- requests$get() %>%
#     lapply(function(z) z$parse("UTF-8")) %>%
#     lapply(function(z) jsonlite::fromJSON(z))
#
#   for (i in seq_along(response_list)) {
#     response_list[[i]]$identification <- identification[[i]]
#   }
#
#   response_list
#
# }