generate_itinerary_details_otp <- function(dyn, groups_size = 1, n_instances = 1, n_cores = 3L, res = 7) {
  
  # open connection to OTP
  
  otpcon <- opentripplanner::otp_connect("localhost", "rio")
  
  # remove cells that don't have any opportunities or inhabitants within it
  
  centroids_coordinates <- readr::read_rds(paste0("./data/rio_h3_grid_res_", res, "_with_data.rds")) %>%
    filter(opportunities != 0 | population != 0) %>%
    st_transform(5880) %>%
    st_centroid(of_largest_polygon = TRUE) %>%
    st_transform(4326) %>% 
    select(-c(population, total_income, opportunities))
  
  # calculate groups of centroids. for each group, calculate routes between
  # origins and all destinations and save the result in a temporary folder
  
  total_cells   <- nrow(centroids_coordinates)
  centroids_ids <- centroids_coordinates$id
  
  origin_groups <- split(centroids_ids, ceiling(centroids_ids / groups_size))
  
  dests <- centroids_coordinates[rep(1:total_cells, times = groups_size), ]
  
  if(!file.exists("./data/temp")) dir.create("./data/temp")
  
  invisible(lapply(
    origin_groups,
    function(i) {
      
      group_id <- ifelse(
        length(i) == 1,
        as.character(i),
        paste0(i[1], "_to_", i[length(i)])
      )
      
      origs <- centroids_coordinates[rep(i, each = total_cells), ]
      
      # last group might have less points than groups_size
      if (nrow(origs) < nrow(dests)) dests <- dests[1:nrow(origs)]
      
      opentripplanner::otp_plan(
        otpcon = otpcon,
        fromPlace = origs,
        toPlace   = dests,
        fromID = as.character(origs$id),
        toID   = as.character(dests$id),
        mode = c("WALK", "TRANSIT"),
        date_time = as.POSIXct("08-01-2020 08:00:00", format = "%d-%m-%Y %H:%M:%S"),
        arriveBy = FALSE,
        maxWalkDistance = 2000,
        numItineraries = 10,
        get_geometry = FALSE,
        ncores = n_cores
      ) %>% 
      readr::write_rds(paste0("./data/temp/itineraries_details_orig_", group_id,
                              "_res_", res, ".rds"))
      
      i
      
    }
  ))
  
  
  # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # # very important to change the number of cores according to what is available !
  # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # 
  # # list of parameters sent to the OTP api
  # 
  # parameters <- list(
  #   mode = "TRANSIT,WALK",
  #   date = "01-08-2020",
  #   time = "08:00am",
  #   arriveBy = "FALSE",
  #   numItineraries = "10"
  #   # ,maxWalkDistance = "2000"
  #   # ,maxHours = "1"
  #   # ,useRequestedDateTimeInMaxHours = "TRUE"
  # )
  # 
  # # set of leg details important for accessibility analysis
  # 
  # leg_details <- c("startTime","endTime", "distance", "mode", "routeId", "route")
  # 
  # # centroids_coordinates is sent to a function which calculates routes between an origin (or a group of) and all possible destinations,
  # # then processes the data from a list into a dataframe and saves it in a temporary folder
  # # since there is no parallel equivalent of walk(), invisible() is used with future_map()
  # 
  # if(!file.exists("./data/temp")) dir.create("./data/temp")
  # 
  # # read each dataframe from the temporary folder into a list and bind everything together
  # # then tidy the resulting dataframe (format columns, arrange rows, etc) and save it
  # 
  # # _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  # # IMPORTANT: tidy_itineraries() removes errors rows _/
  # # _/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/
  # 
  # furrr::future_map(paste0("./data/temp/", list.files("./data/temp")), readr::read_rds) %>%
  #   bind_rows() %>%
  #   tidy_itineraries(leg_details, res) %>%
  #   readr::write_rds(paste0("./data/itineraries_details_res_", res, ".rds"))
  # 
  # # close multisession workers by switching plan
  # 
  # future::plan(future::sequential)
  # 
  # # delete the temporary folder
  # 
  # # unlink("./data/temp", recursive = TRUE)
  
}