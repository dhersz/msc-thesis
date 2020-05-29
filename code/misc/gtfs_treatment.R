source("./code/setup.R")
library(sf)
library(geobr)

shapes_treatment <- function(shapes, crs = 4326) {
  
  shapes <- shapes %>% 
    group_by(shape_id) %>% 
    summarise(
      coord_matrix = list(
        matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2)
      )
    )
    
  sfc <- st_multilinestring(shapes$coord_matrix) %>%
    st_sfc() %>%
    st_cast("LINESTRING")
  
  shapes <- shapes %>% 
    select(-coord_matrix) %>% 
    st_sf(geom = sfc) %>% 
    st_set_crs(crs)
  
  shapes
  
}

calendar_treatment <- function(calendar) {
  
  calendar_treated <- calendar %>% 
    mutate(
      aux = monday * 10^6 + tuesday * 10^5 + wednesday * 10^4 + thursday * 10^3 + friday * 10^2 + saturday * 10 + sunday * 1) %>% 
    mutate(
      working_days = map_chr(aux, function(i) switch(as.character(i), "1" = "sunday", "10" = "saturday", "11" = "weekend", "100" = "friday", "1111100" = "weekday", "1111111" = "every_day"))) %>% 
    select(service_id, working_days)
    
  calendar_treated
  
}

routes_treatment <- function(routes) {
  
  routes_treated <- routes %>% 
    select(route_id, route_short_name, route_long_name)
  
  routes_treated
  
}

trips_treatment <- function(trips) {
  
  trips_treated <- trips %>% 
    distinct(route_id, shape_id)
  
  trips_treated
  
}

fare_attributes_treatment <- function(fare_attributes) {
  
  fare_attributes_treated <- fare_attributes %>% 
    select(fare_id, price)
  
  fare_attributes_treated
  
}

fare_rules_treatment <- function(fare_rules, style = "fetranspor", routes_treated = NA) {
  
  # "fetranspor" style refers to each route having its own values
  # else ("supervia") means its based on the stations you drop in and off the system
  # origin/destination_id = 77 refers to Guapimirim branch, not used here, so set to have the same value as the ~default~
  # origin/destination_id = 79 refers to Teleférico do Alemão, which costs 1 BRL (fare_id = 127)
  # origin/destination_id = 78 refers to the ~usual~ SuperVia stations and is our ~default~ (4.6) (fare_id = 125)
  
  if (style == "fetranspor") {
    fare_rules_treated <- fare_rules %>% 
      select(fare_id, route_id)
  } else {
    fare_rules_treated <- routes_treated %>% 
      mutate(fare_id = ifelse(route_id == 247, 127, 125)) %>% 
      select(fare_id, route_id)
  }
  
  fare_rules_treated
  
}

extract_routes_info <- function(holder = "fetranspor", muni = NA) {
  
  routes <- read_csv(str_c("./data/routes_", holder, ".txt"))
  trips <- read_csv(str_c("./data/trips_", holder, ".txt"))
  fare_attributes <- read_csv(str_c("./data/fare_attributes_", holder, ".txt"))
  fare_rules <- read_csv(str_c("./data/fare_rules_", holder, ".txt"))
  shapes <- read_csv(str_c("./data/shapes_", holder, ".txt"))
  
  routes_info <- trips %>%
    trips_treatment() %>% 
    left_join(
      routes_treatment(routes),
      by = "route_id"
    ) %>% 
    left_join(
      fare_rules_treatment(fare_rules, holder, routes_treatment(routes)),
      by = "route_id"
    ) %>% 
    left_join(
      fare_attributes_treatment(fare_attributes),
      by = "fare_id"
    ) %>% 
    left_join(
      shapes_treatment(shapes, 4674),
      by = "shape_id"
    ) %>% 
    st_as_sf()
  
  if(!is.na(muni)) {
    
    rio <- read_municipality(3304557) %>% 
      st_transform(5880) # %>% 
      # st_buffer(1000)
    
    routes_info <- routes_info %>% 
      st_transform(5880) %>% 
      filter(st_intersects(., rio, sparse = FALSE)) %>% 
      st_drop_geometry()
    
  }
  
  routes_info <- routes_info %>% 
    distinct(route_id, fare_id, route_short_name, route_long_name, price)
  
  routes_info
  
}

workflow <- function() {
  
  f <- extract_routes_info("fetranspor", "rio")
  s <- extract_routes_info("supervia", "rio")
  
  integracao_metro_onibus <- c(513, 603, 608, 605, 609, 209, 611, 614, 616, 913, 876)
  
  combined <- bind_rows(s, f) %>% 
    mutate(
      route_short_name = ifelse(is.na(route_short_name), "", route_short_name),
      type = ifelse(fare_id == 32069624 | fare_id == 34127545, "onibus_municipal", ""),
      type = ifelse(route_short_name %in% as.character(integracao_metro_onibus), "onibus_metro", type),
      type = ifelse(str_detect(route_long_name, "Ramal"), "trem", type),
      type = ifelse(str_detect(route_short_name, "BRT"), "brt", type),
      type = ifelse(str_detect(route_short_name, "VLT"), "vlt", type),
      type = ifelse(str_detect(route_short_name, "L\\d") | str_detect(route_long_name, "Metrô na"), "metro", type),
      type = ifelse(str_detect(route_short_name, "^2\\d{3}$") | route_short_name == "LECD 25", "frescao_municipal", type),
      type = ifelse(str_detect(route_short_name, "^\\d{3}[B-Z]$") | str_detect(route_short_name, "^4\\d{2}A$") | str_detect(route_short_name, "TB$"), "onibus_intermunicipal", type),
      type = ifelse(str_detect(route_short_name, "^\\d{4}[B-Z]$"), "frescao_intermunicipal", type),
      type = ifelse(fare_id == 13794408, "barca", type),
      type = ifelse(type == "", "outros", type),
      
      # temp only (I hope) # adjusting prices to make sure their logic matches with what is actually true
      
      price = ifelse(route_id == 19209271 | route_id == 19132687, 10, price), # two random intermunicipal bus lines with no fare
      price = ifelse(route_id == 19016860, 3.85, price), # 495L
      price = ifelse(route_id == 12862118, 3.6, price), # 603
      price = ifelse(route_id == 18928473, 4, price), # 716L
      price = ifelse(fare_id == 34127393, 3.6, price), # brt prices = municipal_bus prices
      price = ifelse(fare_id == 52433953, 3.5, price), #vlt prices lower than bus
      price = ifelse(fare_id == 125, 4.2, price), # train price higher than subway
      
      price_bu = ifelse(type == "barca", 6, price),
      price_bu = ifelse(type == "onibus_intermunicipal" & price > 7.4, 7.4, price)
      
    )
  
  combined
  
}


