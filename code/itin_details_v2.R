itin_details_v2 <- function(itin, itin_details = c("endTime", "legs"),
                            leg_details = c("startTime", "endTime", "distance", "mode", "route")) {
  
  details <- tibble::tibble(itin = itin)
  
  details <- tidyr::hoist(
     details,
     itin,
     origin_id = c("identification", "origin_id"),
     dest_id = c("identification", "dest_id"),
     error_id = c("error", "id"),
     error_msg = c("error", "msg"),
     itin_start_time = c("plan", "date"),
     itineraries = c("plan", "itineraries")
  )
   
  details <- dplyr::select(
    details,
    -itin
  )
  
  details <- dplyr::mutate(
    details,
    itineraries = itineraries[it_details]
  )
  
  # details <- tidyr::hoist(
  #   details,
  #   itineraries,
  #   endTime = "endTime",
  #   legs = "legs"
  # )
  # 
  # details <- dplyr::select(
  #   details,
  #   -itineraries
  # )
  
  details
}