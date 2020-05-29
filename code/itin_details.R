itin_details <- function(itin, leg_details = c("startTime", "endTime", "distance",
                                                  "mode", "routeId", "route")) {
  
  # initialise list to store each itinerary across all requests
  # calculate the total number of itineraries
  # if a plan has been succesfully calculated, sum the number of itineraries
  # else sum one to store error details
  n_itin <- 0
  for (i in seq_along(itin)) {
    if (!is.null(itin[[i]]$plan)) { 
      n_itin <- n_itin + nrow(itin[[i]]$plan$itineraries)
    } else {
      n_itin <- n_itin + 1
    }
  }
  
  # create empty list with length equal to the number of itineraries
  itin_details <- vector("list", length = n_itin)

  # iterate through each request
  # if a request has sent an error as response, store error details
  # else loop through each response's itinerary
  n_itin <- 1 
  for (i in seq_along(itin)) {
    # identification infos
    origin_id <- itin[[i]]$identification$origin_id
    dest_id <- itin[[i]]$identification$dest_id
    
    # save start time as the time sent as parameter, not when OTP calculates the departure.
    # OTP dismisses "waiting at home", assuming that people know exactly at what time their
    # transport is gonna arrive.
    # many people choose a specific time to leave their homes, which is assumed to be the time
    # sent as the parameter.
    itin_start_time <- itin[[i]]$plan$date
    
    # check if request has thrown an error
    if (!is.null(itin[[i]]$error)) {
      error_id <- itin[[i]]$error$id
      error_msg <- itin[[i]]$error$msg
      
      # store list with identification and error infos in the itineraries' details list
      # set an empty named leg list to later avoid an undesired behaviour of the unnest_longer func
      itin_details[[n_itin]] <- list(
        "origin_id" = origin_id,
        "dest_id" = dest_id,
        "error_id" = error_id,
        "error_msg" = error_msg
      )
      itin_details[[n_itin]]$legs[[1]] <- list("leg_id" = NA)
      
      n_itin <- n_itin + 1
    } else {
      # save the itineraries list in a new object for easier manipulation
      itin_list <- itin[[i]]$plan$itineraries
      
      # looping through each request's itinerary
      for (j in seq.int(nrow(itin_list))) {
        error_id <- NA
        
        itin_end_time <- itin_list$endTime[[j]]
        
        # save the legs list in a new object for easier manipulation
        legs <- itin_list$legs[[j]]
        
        # add an leg identificator element then select desired legs' properties
        legs$leg_id <- c(1:nrow(legs))
        if (is.null(legs$routeId)) legs$routeId <- NA
        legs <- legs[c("leg_id", leg_details)]
        
        # convert legs from a data.frame to a list with adequate structure for later unnesting
        legs <- purrr::transpose(legs)
        
        # store list with identification, itinerary and legs infos in the itineraries list
        itin_details[[n_itin]] <- list(
          "origin_id" = origin_id,
          "dest_id" = dest_id,
          "it_id" = j,
          "error_id" = error_id,
          "itin_start_time" = itin_start_time,
          "itin_end_time" = itin_end_time
        )
        itin_details[[n_itin]]$legs <- legs
        
        n_itin <- n_itin + 1
      }
    }
  }
  
  # convert itin_details from list to tibble
  itin_details <- tibble::tibble(itin_details = itin_details)
  itin_details <- tidyr::unnest_wider(itin_details, itin_details)
  itin_details <- tidyr::unnest_longer(itin_details, legs)
  itin_details <- tidyr::unnest_wider(itin_details, legs)
  
  # rearrange columns' order, placing legs info to the end
  itin_details <- dplyr::select(
    itin_details,
    all_of(names(itin_details)[! names(itin_details) %in% c("leg_id", leg_details)]),
    all_of(c("leg_id", leg_details))
  )

  # rename columns from camelCase to snake_case
  itin_details <- dplyr::rename(
    itin_details,
    leg_start_time = startTime,
    leg_end_time = endTime,
  )

  # convert times from epoch to datetime
  itin_details <- dplyr::mutate_at(
    itin_details,
    dplyr::vars(dplyr::ends_with("time")),
    list(~ lubridate::as_datetime(as.double(.) / 1000))
  )
  
  itin_details
}











######################################




itin_details2 <- function(itin, leg_details = c("startTime", "endTime", "distance",
                                                "mode", "route")) {
  
  # initialise list to store each itinerary across all requests
  # calculate the total number of itineraries
  # if a plan has been succesfully calculated, sum the number of itineraries
  # else sum one to store error details
  n_itin <- 0
  for (i in seq_along(itin)) {
    if (!is.null(itin[[i]]$plan)) { 
      n_itin <- n_itin + nrow(itin[[i]]$plan$itineraries)
    } else {
      n_itin <- n_itin + 1
    }
  }
  
  # create empty list with length equal to the number of itineraries
  itin_details <- vector("list", length = n_itin)
  
  # iterate through each request
  # if a request has sent an error as response, store error details
  # else loop through each response's itinerary
  n_itin <- 1 
  for (i in seq_along(itin)) {
    # identification infos
    origin_id <- itin[[i]]$identification$origin_id
    dest_id <- itin[[i]]$identification$dest_id
    
    # save start time as the time sent as parameter, not when OTP calculates the departure.
    # OTP dismisses "waiting at home", assuming that people know exactly at what time their
    # transport is gonna arrive.
    # many people choose a specific time to leave their homes, which is assumed to be the time
    # sent as the parameter.
    itin_start_time <- itin[[i]]$plan$date
    
    # check if request has thrown an error
    if (!is.null(itin[[i]]$error)) {
      error_id <- itin[[i]]$error$id
      error_msg <- itin[[i]]$error$msg
      
      # store list with identification and error infos in the itineraries' details list
      # set an empty named leg list to later avoid an undesired behaviour of the unnest_longer func
      itin_details[[n_itin]] <- list(
        "origin_id" = origin_id,
        "dest_id" = dest_id,
        "error_id" = error_id,
        "error_msg" = error_msg
      )
      itin_details[[n_itin]]$legs[[1]] <- list("leg_id" = NA)
      
      n_itin <- n_itin + 1
    } else {
      # save the itineraries list in a new object for easier manipulation
      itin_list <- itin[[i]]$plan$itineraries
      
      # looping through each request's itinerary
      for (j in seq.int(nrow(itin_list))) {
        error_id <- NA
        
        itin_end_time <- itin_list$endTime[[j]]
        
        # save the legs list in a new object for easier manipulation
        legs <- itin_list$legs[[j]]
        
        # add an leg identificator element then select desired legs' properties
        legs$leg_id <- c(1:nrow(legs))
        legs$legGeometry <- legs$legGeometry$points
        legs <- legs[c("leg_id", leg_details)]
        
        
        # convert legs from a data.frame to a list with adequate structure for later unnesting
        legs <- purrr::transpose(legs)
        
        # store list with identification, itinerary and legs infos in the itineraries list
        itin_details[[n_itin]] <- list(
          "origin_id" = origin_id,
          "dest_id" = dest_id,
          "it_id" = j,
          "error_id" = error_id,
          "itin_start_time" = itin_start_time,
          "itin_end_time" = itin_end_time
        )
        itin_details[[n_itin]]$legs <- legs
        
        n_itin <- n_itin + 1
      }
    }
  }
  
  # convert itin_details from list to tibble
  itin_details <- tibble::tibble(itin_details = itin_details)
  itin_details <- tidyr::unnest_wider(itin_details, itin_details)
  itin_details <- tidyr::unnest_longer(itin_details, legs)
  itin_details <- tidyr::unnest_wider(itin_details, legs)
  
  # rearrange columns' order, placing legs info to the end
  itin_details <- dplyr::select(
    itin_details,
    all_of(names(itin_details)[! names(itin_details) %in% c("leg_id", leg_details)]),
    all_of(c("leg_id", leg_details))
  )
  
  # rename columns from camelCase to snake_case
  itin_details <- dplyr::rename(
    itin_details,
    leg_start_time = startTime,
    leg_end_time = endTime,
  )
  
  # convert times from epoch to datetime
  itin_details <- dplyr::mutate_at(
    itin_details,
    dplyr::vars(dplyr::ends_with("time")),
    list(~ lubridate::as_datetime(as.double(.) / 1000))
  )
  
  itin_details
}