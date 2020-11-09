library(dplyr)
library(sf)

# before using these functions, specifically get_transit_itineraries, it's
# important to start the OTP server. to do so, one could use:
#    opentripplanner::otp_setup(otp = "./otp/otp.jar", dir = "./otp", memory = 2048, router = "rio")
#    opentripplanner::otp_stop(warn = FALSE)
# but unfortunately that wasn't really working reliably, sometimes java would
# crash, not sure why.

generate_itinerary_details <- function(dyn = FALSE,
                                       grid_name = "grid_with_data",
                                       dep_time = "08:00am",
                                       groups_size = 1,
                                       n_instances = 1,
                                       n_cores = 3L,
                                       res = 7,
                                       router = "rio",
                                       walking_only = FALSE) {

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
    numItineraries = "20"
    # ,maxWalkDistance = "2000"
    # ,maxHours = "1"
    # ,useRequestedDateTimeInMaxHours = "TRUE"
  )
  
  if (walking_only) parameters$mode <- "WALK"
  
  # replace ':' in dep_time to later use it in the files names
  dep_time <- gsub(":", "", dep_time)

  # set of leg details important for accessibility analysis
  leg_details <- c("startTime","endTime", "distance", "mode", "routeId", "route")

  # calculate the centroids coordinates of the grid with resolution 'res'.
  # cells with no opportunities and population are cleaned out of the dataframe
  # to speed up the requests, since they won't affect accessibility.

  router_folder <- paste0("./data/", router, "_res_", res)
  grid_data_path <- paste0(router_folder, "/", grid_name, ".rds")
  
  clean_grid <- readr::read_rds(grid_data_path) %>%
    filter(opportunities != 0 | population != 0)

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
      router,
      res,
      .progress = TRUE
    )
  )

  # read each dataframe from the temporary folder into a list and bind
  # everything together. then tidy the resulting dataframe (format columns,
  # arrange rows, etc) and save it in a separate folder

  folder_path <- paste0("./data/", router, "_res_", res)
  if (!file.exists(folder_path)) dir.create(folder_path)
  
  subfolder_path <- paste0(folder_path, "/itineraries")
  if (!file.exists(subfolder_path)) dir.create(subfolder_path)
  
  temp_files_path <- paste0("./data/temp/itineraries_details_orig_",
                            names(origin_groups),"_res_", res, "_", dep_time,
                            ".rds")
  
  itineraries_path <- ifelse(
    walking_only,
    paste0(subfolder_path, "/walking_itineraries.rds"),
    paste0(subfolder_path, "/itineraries_", dep_time, ".rds")
  )
  
  furrr::future_map(temp_files_path, readr::read_rds) %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidy_itineraries(leg_details, res, walking_only, subfolder_path, dep_time) %>%
    readr::write_rds(itineraries_path, compress = "gz")

  # close multisession workers by switching plan
  future::plan(future::sequential)

  # delete the temporary folder
  # unlink("./data/temp", recursive = TRUE)
  
  stop_otp()
  invisible(gc())

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
                                    router,
                                    res) {

  # this function calls the two most important functions in this whole ensemble.
  # make_request sends requests to the OTP API and returns its responses.
  # extract_itinerary_details takes these responses and processes their data
  # from a list into a dataframe.
  # this dataframe is then saved inside a temporary folder that contains the
  # itineraries details from each single origin of the origin groups to all
  # destinations.

  origin_group <- origin_groups[x]
  
  filepath <- paste0(
    "./data/temp/itineraries_details_orig_",
    names(origin_group),
    "_res_", 
    res, 
    "_", 
    dep_time, 
    ".rds"
  )

  make_request(x, origin_group, od_points, parameters, n_instances, dyn, router) %>%
    purrr::map_dfr(extract_itinerary_details, leg_details) %>%
    readr::write_rds(filepath, compress = "gz")

  origin_group

}



make_request <- function(x,
                         origin_group,
                         od_points,
                         parameters,
                         n_instances,
                         dyn,
                         router) {
  
  origins_pool <- od_points[origin_group[[1]], ]

  request_url <- httr::parse_url(paste0("http://localhost:", 8090 + x %% n_instances, "/otp/routers/", router, "/plan/"))

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



tidy_itineraries <- function(itineraries_details, leg_details, res, walking_only, subfolder_path, dep_time) {

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
    select_unique_itineraries(res, walking_only, subfolder_path, dep_time)
  
  if (walking_only) {
    
    itineraries_details <- itineraries_details %>% mutate(it_id = 200)
    
  } 

  itineraries_details

}



select_unique_itineraries <- function(itineraries_details, res, walking_only, subfolder_path, dep_time) {

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
  
  errors_path <- ifelse(
    walking_only,
    paste0(subfolder_path, "/walking_itineraries_errors.rds"),
    paste0(subfolder_path, "/itineraries_", dep_time, "_errors.rds")
  )

  errors <- itineraries_details %>%
    filter(! is.na(error_id)) %>%
    select(orig_id, dest_id, error_id, error_msg) %>%
    readr::write_rds(errors_path, compress = "gz")

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



setup_otp <- function(n_instances, router = "rio") {

  future::plan(future::multisession, workers = n_instances)

  invisible(furrr::future_map(1:n_instances,
    function(i) {
      system2("java8",
        args = c("-Xmx4G",
                 "-jar", "otp/otp.jar",
                 "--server", "--graphs", "otp/graphs", "--router", router,
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