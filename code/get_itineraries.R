get_itineraries <- function(origin, dest, hostname = "localhost", port = 8080,
                            router = "default") {
  
  # create router URL to get request from --------------------------------------------------------
  
  router_url = stringr::str_c("http://", hostname, ":", port, "/otp/routers/", router, "/plan")
  
  # initialise list which will store each request with length (number_origins * number_dests) ----
  
  n_origin <- nrow(origin)
  n_dest <- nrow(dest)
  requests_list <- vector("list", length = n_origin * n_dest)
  
  # create progress bar to assess progress -------------------------------------------------------
  
  #total <- n_origin * n_dest
  #pb <- progress::progress_bar$new(total = total, format = "(:spin)[:bar]:percent")
  
  # iterate through each object of origin and dest dataframes and request itineraries between them
  # store request in requests_list along with ids for easy identification -----------------------
  
  for (i in seq.int(n_origin)) {
    # create a string in the format "lat,long" to be used as origin parameter
    fromPlace = stringr::str_c(origin$lat[[i]], ",", origin$long[[i]])
    
    for (j in seq.int(n_dest)) {
      # create a string in the format "lat,long" to be used as destination parameter
      toPlace = stringr::str_c(dest$lat[[j]], ",", dest$long[[j]])
      
      # send request based on the parameters passed, extract content as text and parse as json
      request <- httr::GET(
        router_url,
        query = list(
          fromPlace = fromPlace,
          toPlace = toPlace,
          mode = "TRANSIT,WALK",
          date = "11-25-2018",
          time = "08:00am",
          arriveBy = FALSE,
          maxWalkDistance = 2000#,
          #numItineraries = 10
        )
      )
      request <- httr::content(request, as = "text", encoding = "UTF-8")
      request <- jsonlite::fromJSON(request)
      
      count <- n_dest * (i - 1) + j # which iteration we're at
      requests_list[[count]] <- request # store request in the list containing all requests
      
      # store origin and destination ids in a list to allow for easier identication
      #identification <- list("origin_id" = origin$id[[i]], "dest_id" = dest$id[[j]])
      #requests_list[[count]]$identification <- identification
      
      #pb$tick() # update progress bar
    }
  }
  
  requests_list
}