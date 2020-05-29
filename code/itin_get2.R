itin_get <- function(origin, dest, hostname = "localhost", port = 8080,
                     router = "default") {
  
  router_url = stringr::str_c("http://", hostname, ":", port, "/otp/routers/", router, "/plan")
  
  n_origin <- nrow(origin)
  n_dest <- nrow(dest)
  urls <- vector("character", length = n_origin * n_dest)
  
  for (i in seq.int(n_origin)) {
    # create a string in the format "lat,long" to be used as origin parameter
    fromPlace = stringr::str_c(origin$lat[[i]], ",", origin$long[[i]])
    
    for (j in seq.int(n_dest)) {
      # create a string in the format "lat,long" to be used as destination parameter
      toPlace = stringr::str_c(dest$lat[[j]], ",", dest$long[[j]])
      
      count <- n_dest * (i - 1) + j
      urls[[count]] <- str_c(
        router_url,
        "?fromPlace=", fromPlace,
        "&toPlace=", toPlace,
        "&mode=TRANSIT,WALK",
        "&date=11-25-2018",
        "&time=08:00am",
        "&arriveBy=FALSE",
        "&maxWalkDistance=2000"
      )
    }
  }
  
  req <- Async$new(urls = urls)
  
  res <- req$get()
  res <- lapply(res, function(z) z$parse("UTF-8"))
  res <- lapply(res, function(z) jsonlite::fromJSON(z))
  
  res
}