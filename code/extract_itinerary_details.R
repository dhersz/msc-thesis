extract_itinerary_details <- function(itin,
                                      leg_details = c("startTime", "endTime", "distance",
                                      "mode", "route")) {
  
  # initialise list to store each itinerary across all requests ----------------------------------
  
  # calculate the total number of itineraries
  n_itin <- 0
  for (i in seq_along(itin)) {
    if (!is.null(itin[[i]]$plan)) { # check if a plan has been successfully calculated
      n_itin <- n_itin + nrow(itin[[i]]$plan$itineraries) # if positive, sum up the number of itineraries
    } else {
      n_itin <- n_itin + 1 # if negative, sum one to store error details
    }
  }
  
  # create empty list with length equal to the number of itineraries
  itin_details <- vector("list", length = n_itin)
  
  # # create progress bar to assess progress
  # pb <- progress::progress_bar$new(total = n_itin, format = "(:spin)[:bar]:percent")
  
  # store the details of each itinerary in its respective list -----------------------------------
  # iterate through each request and itinerary
  
  n_itin <- 1 
  for (i in seq_along(itin)) { # iteration through each request
    # identification infos
    origin_id <- itin[[i]]$identification$origin_id
    dest_id <- itin[[i]]$identification$dest_id
    origin_lat <- stringr::str_split(itin[[i]]$requestParameters$fromPlace, ",")[[1]][[1]]
    origin_long <- stringr::str_split(itin[[i]]$requestParameters$fromPlace, ",")[[1]][[2]]
    dest_lat <- stringr::str_split(itin[[i]]$requestParameters$toPlace, ",")[[1]][[1]]
    dest_long <- stringr::str_split(itin[[i]]$requestParameters$toPlace, ",")[[1]][[2]]
    
    # save start time as the time sent as parameter, not when OTP calculates the departure.
    # OTP dismisses "waiting at home", assuming that people know exactly at what time their
    # transport is gonna arrive.
    # many people choose a specific time to leave their homes, which is assumed to be the time
    # sent as the parameter.
    itin_start_time <- itin[[i]]$plan$date
    
    if (!is.null(itin[[i]]$error)) { # check if request has thrown an error
      # if positive, store error id, msg
      
      error_id <- as.character(itin[[i]]$error$id)
      error_msg <- itin[[i]]$error$msg
      
      # create list with identification and error infos then store it in the itineraries list
      itin_details[[n_itin]] <- list(
        "origin_id" = origin_id,
        "origin_lat" = origin_lat,
        "origin_long" = origin_long,
        "dest_id" = dest_id,
        "dest_lat" = dest_lat,
        "dest_long" = dest_long,
        "error_id" = error_id,
        "error_msg" = error_msg
      )
      
      n_itin <- n_itin + 1
      
      # pb$tick()
    } else {
      # if negative, a plan has been successfully calculated and its details are stored
      
      # convert the itineraries list to a tibble for easier manipulation
      df <- tibble::as_tibble(itin[[i]]$plan$itineraries)
      
      for (j in seq.int(nrow(df))) { # iteration through each request's itinerary
        error_id <- "ok"
        
        itin_end_time <- df$endTime[[j]]
        
        # convert the request's legs list to a tibble for easier manipulation
        legs <- tibble::as_tibble(df$legs[[j]])
        
        # select only the desired legs' properties
        legs <- dplyr::select(
          legs,
          any_of(leg_details)
        )
        
        # substitute empty char in route (when mode is walk) to avoid confusion later
        legs <- dplyr::mutate(
          legs,
          route = ifelse(route == "", NA, route) 
        )
        
        # change columns' names from camelCase to snake_case
        legs <- dplyr::rename(
          legs,
          start_time = startTime,
          end_time = endTime
        )
        
        # if the number of legs in an itinerary equals to 1 (i.e when you only need
        # to walk from origin to destination) add a "1" to the end of the columns'
        # names in order to later arrange the order of the columns approppriately
        n_legs <- nrow(df$legs[[j]])
        if (n_legs == 1) {
          legs <- dplyr::rename_all(legs, stringr::str_c, "1")
        }
        
        # convert legs back into a list to store within a list later 
        legs <- unlist(legs)
        
        # create list with identification and itinerary infos then store it in the
        # itineraries list
        itin_details[[n_itin]] <- list(
          "origin_id" = origin_id,
          "origin_lat" = origin_lat,
          "origin_long" = origin_long,
          "dest_id" = dest_id,
          "dest_lat" = dest_lat,
          "dest_long" = dest_long,
          "it_number" = j,
          "error_id" = error_id,
          "itin_start_time" = itin_start_time,
          "itin_end_time" = itin_end_time
        )
        itin_details[[n_itin]] <- append(itin_details[[n_itin]], legs)
        
        n_itin <- n_itin + 1
        
        # pb$tick()
      }
    }
  }
  
  # convert itin_details from a list to a dataframe
  itin_details <- dplyr::bind_rows(itin_details)
  
  # rename legs columns from "column_name1" to "leg_1_column_name"
  itin_details <- dplyr::rename_at(
    itin_details,
    dplyr::vars(matches("\\d$")), # looks for columns' names which ends with digits
    
    # get the matched columns' names and rename based on a regex expression
    # string to be replaced divided in two groups, the first being the text and the second
    # the digits, which are then reordered using backreferences
    list(~ stringr::str_replace(., "([^\\d]*)(\\d+)$", "leg_\\2_\\1"))
  )
  
  # convert times from epoch to datetime
  itin_details <- dplyr::mutate_at(
    itin_details,
    dplyr::vars(dplyr::ends_with("time")),
    list(~ lubridate::as_datetime(as.double(.) / 1000))
  )

  # rearrange columns' order for easier comprehension

  # first save the order of the first columns:
  # identification, errors then general trip infos
  ordered_columns <- c("origin_id", "origin_lat", "origin_long",
                       "dest_id", "dest_lat", "dest_long",
                       "error_id", "error_msg",
                       "it_number", "itin_start_time", "itin_end_time")

  # calculate the max number of legs found
  # parse the numbers in the itin_details header and find the max
  max_legs <- max(
    readr::parse_number(names(itin_details), na = ordered_columns),
    na.rm = TRUE
  )

  # arrange columns in the desired order
  itin_details <- dplyr::select(
    itin_details,
    any_of(ordered_columns),
    starts_with(stringr::str_c("leg_", 1:max_legs)),
    everything() # in case a column has not been accounted in the previous steps
  )
  
  itin_details
}