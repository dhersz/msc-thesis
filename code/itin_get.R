itin_get <- function(origin, dest, param, hostname = "localhost", port = 8080, router = "router") {
  
  router_url = stringr::str_c("http://", hostname, ":", port, "/otp/routers/", router, "/plan")
  
  n_origin <- nrow(origin)
  n_dest <- nrow(dest)
  urls <- vector("character", length = n_origin * n_dest)
  identification <- vector("list", length = n_origin * n_dest)
  
  count <- 1
  for (i in seq.int(n_origin)) {
    # create a string in the format "lat,long" to be used as origin parameter
    fromPlace = stringr::str_c(origin$lat[[i]], ",", origin$long[[i]])
    
    for (j in seq.int(n_dest)) {
      # create a string in the format "lat,long" to be used as destination parameter
      toPlace = stringr::str_c(dest$lat[[j]], ",", dest$long[[j]])
      
      urls[[count]] <- stringr::str_c(
        router_url,
        "?fromPlace=", fromPlace,
        "&toPlace=", toPlace,
        "&mode=TRANSIT,WALK",
        "&date=01-01-2020",
        "&time=08:00am",
        "&arriveBy=FALSE",
        "&maxWalkDistance=2000",
        "&numItineraries=", param[["numItineraries"]]
      )
      
      identification[[count]]$origin_id <- origin$id[[i]]
      identification[[count]]$dest_id <- dest$id[[j]]
      
      count <- count + 1
    }
  }
  
  req <- crul::Async$new(urls = urls)
  
  res <- req$get()
  res <- lapply(res, function(z) z$parse("UTF-8"))
  res <- lapply(res, function(z) jsonlite::fromJSON(z))
  
  for (i in seq_along(res)) {
    res[[i]]$identification <- identification[[i]]
  }

  res
}




itin_get2 <- function(hostname = "localhost", port = 8080, router = "rio") {
  
  router_url = stringr::str_c("http://", hostname, ":", port, "/otp/routers/", router, "/plan")
  
  rio_centroids_coordinates <- readr::read_rds(stringr::str_c("./data/rio_h3_grid_res_7.rds")) %>%
    st_transform(5880) %>% 
    st_centroid() %>% 
    st_transform(4674) %>% 
    st_coordinates() %>% 
    tibble::as_tibble() %>% 
    tibble::rowid_to_column("id") %>%
    rename(lat = Y, long = X) %>% 
    head(10)
  
  origin <- rio_centroids_coordinates
  dest <- origin
  
  n_origin <- nrow(origin)
  n_dest <- nrow(dest)
  urls <- vector("character", length = n_origin * n_dest)
  identification <- vector("list", length = n_origin * n_dest)
  
  count <- 1
  for (i in seq.int(n_origin)) {
    # create a string in the format "lat,long" to be used as origin parameter
    fromPlace = stringr::str_c(origin$lat[[i]], ",", origin$long[[i]])
    
    for (j in seq.int(n_dest)) {
      # create a string in the format "lat,long" to be used as destination parameter
      toPlace = stringr::str_c(dest$lat[[j]], ",", dest$long[[j]])
      
      urls[[count]] <- stringr::str_c(
        router_url,
        "?fromPlace=", fromPlace,
        "&toPlace=", toPlace,
        "&mode=TRANSIT,WALK",
        "&date=01-01-2020",
        "&time=08:00am",
        "&arriveBy=FALSE",
        "&maxWalkDistance=2000",
        "&numItineraries=3"
      )
      
      identification[[count]]$origin_id <- origin$id[[i]]
      identification[[count]]$dest_id <- dest$id[[j]]
      
      count <- count + 1
    }
  }
  
  req <- crul::Async$new(urls = urls)
  
  res <- req$get()
  res <- lapply(res, function(z) z$parse("UTF-8"))
  res <- lapply(res, function(z) jsonlite::fromJSON(z))
  
  for (i in seq_along(res)) {
    res[[i]]$identification <- identification[[i]]
  }
  
  res
}