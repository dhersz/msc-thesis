source("./code/setup.R")
source("./code/setup_local.R")
library(sf)
library(googlePolylines)
library(geobr)
library(tmap)
library(h3jsr)

generate_grid_coords <- function(shape = NULL, crs = 4326, res = 7) {
  
  if (is.null(shape)) {
    shape <- read_municipality(3304557) %>% 
      st_transform(5880) %>% 
      st_simplify(dTolerance = 100) %>% 
      st_transform(4674)
  }
  
  bbox <- st_bbox(shape)
  xmin <- bbox["xmin"]
  xmax <- bbox["xmax"]
  ymin <- bbox["ymin"]
  ymax <- bbox["ymax"]
  shape_bbox <- st_polygon(list(rbind(c(xmin, ymax), c(xmax, ymax), c(xmax, ymin), c(xmin, ymin), c(xmin, ymax)))) %>% 
    st_sfc() %>% 
    st_set_crs(crs) %>% 
    st_as_sf()
  
  hex_ids <- polyfill(shape_bbox, res = res, simple = FALSE)
  
  shape <- shape %>% st_transform(5880)
  
  hex_grid <- unlist(hex_ids$h3_polyfillers) %>% 
    h3_to_polygon(simple = FALSE) %>% 
    st_transform(5880) %>% 
    st_intersection(shape) %>% 
    st_transform(4674) %>% 
    select(h3_address, geometry)
  
  hex_grid
  
}

generate_sf <- function(otp_response = NA, res = 7, random = FALSE) {
  
  if (is.list(otp_response)) {
    itinerary_details <- otp_response
  } else {
    origin <- read_rds(str_c("./data/rio_h3_grid_res_", res, ".rds")) %>% 
      st_transform(5880) %>% 
      st_centroid(of_largest_polygon = TRUE) %>% 
      st_transform(4674) %>% 
      st_coordinates() %>% 
      as_tibble() %>% 
      rowid_to_column("id") %>% 
      rename(long = X, lat = Y)
    
    dest <- read_csv("./data/carioca.csv")
    
    itinerary_details <- itin_get(origin, dest, param = list(numItineraries = 3), router = "router")
  }
  
  itinerary_details <- itinerary_details %>% 
    itin_details2(leg_details = c("startTime", "endTime", "distance", "mode", "route", "legGeometry")) %>% 
    itin_select_unique()
  
  if (random) {
    itinerary_details <- itinerary_details %>% 
      mutate(full_id = str_c(origin_id, "_", dest_id, "_", it_id))
    
    unique_itinerary <- unique(itinerary_details$full_id)
    n <- length(unique_itinerary)
    
    set.seed(n)
    random_order <- sample(1:n, n)
    
    itinerary_details <- itinerary_details %>% 
      mutate(new_order = random_order[match(full_id, unique_itinerary)]) %>% 
      arrange(new_order) %>% 
      select(-new_order, -full_id)
  }
  
  leg_geometry <- itinerary_details$legGeometry %>% 
    decode() %>%
    map(function(i) select(i, lon, lat)) %>%
    map(as.matrix) %>% 
    st_multilinestring() %>% 
    st_sfc() %>% 
    st_cast("LINESTRING") %>% 
    st_set_crs(4674) %>% 
    st_transform(5880) %>% 
    st_simplify(dTolerance = 100) %>% 
    st_transform(4674)
  
  itinerary_details <- itinerary_details %>% 
    select(-legGeometry) %>% 
    st_sf(geom = leg_geometry)
  
  itinerary_details
  
}

frames_info <- function(itinerary_details) {
  
  frames_info <- itinerary_details %>% 
    select(mode, route) %>% 
    mutate(
      brt = str_detect(route, "BRT"),
      type = ifelse(mode == "SUBWAY" | mode == "RAIL" | mode == "FERRY" | brt == TRUE, "mid_high_cap", ifelse(mode == "BUS", "low_cap", "active"))
    ) %>% 
    rowid_to_column("id") %>% 
    select(id, type, geom)
    
  frames_info
  
}

map_generator <- function(frames, rj = NULL, rio = NULL, from = 1, to = 100, alpha = 0.04, dpi = 250, width = 2000, height = 1300) {
  
  if (is.null(rio)) {
    rio <- read_municipality(3304557) %>% 
      st_transform(5880) %>% 
      st_simplify(dTolerance = 100) %>% 
      st_transform(4674)
  }
  
  if (is.null(rio)) {
    rj <- read_municipality("RJ") %>% 
      filter(name_muni %in% c("Rio De Janeiro", "Mangaratiba", "Itaguaí", "Seropédica", "Nova Iguaçu",
                              "Mesquita", "Nilópolis", "São João De Meriti", "Duque De Caxias",
                              "São Gonçalo", "Niterói", "Queimados", "Japeri", "Paracambi", "Mendes",
                              "Magé", "Belford Roxo")) %>% 
      st_transform(5880) %>% 
      st_simplify(dTolerance = 100) %>% 
      st_transform(4674)
  }
  
  for (i in from:to) {
    
    temp_frames <- frames %>% filter(id <= i)
  
    routes <- tm_shape(temp_frames) +
      tm_lines(
        col = "type",
        palette = c("darkolivegreen1", "deepskyblue1", "indianred1"),
        lwd = 2,
        alpha = alpha
      )
    
    full_map <-
      tm_shape(rj, bbox = st_bbox(rio)) + tm_borders(col = "#081a25", lwd = 2) +
      tm_shape(rio) + tm_borders(col = "#1C2A33", lwd = 2) + tm_fill(col = "#031826") +
      routes +
      tm_layout(
        title = "Transporte público pro centro da cidade do Rio",
        title.color = "#4F5D66",
        title.bg.color = "#021019",
        title.size = 1.5,
        title.position = c("center", "top"),
        title.fontface = 2,
        frame = FALSE,
        legend.show = FALSE,
        bg.color = "#021019",
        asp = width/height,
        attr.outside = FALSE
      ) +
      tm_credits("Daniel Herszenhut: @daniel.hersz", col = "#35434C", position = c("left", "bottom"), size = 1, alpha = 0.5)
    
    tmap_save(full_map, str_c("./img/frame_", str_pad(i, 4, pad = "0"), ".png"), width = width, height = height, dpi = dpi)
    
  }
  
}

create_movie <- function(filename, path = "./img", fps = 24, duration = 60, width = 2000, height = 1300) {
  
  # needs ffmpeg installed
  # instructions: https://video.stackexchange.com/questions/20495/how-do-i-set-up-and-use-ffmpeg-in-windows
  
  files_dir <- str_replace(path, ".", getwd())
  files <- list.files(files_dir)
  n <- length(files)
  
  full_filename <- str_c(getwd(), "/", filename)
  if (file.exists(full_filename)) file.remove(full_filename)

  call_msg <- str_c(
    "ffmpeg",
    " -f lavfi -i nullsrc=s=", width, "x", height, ":d=", duration, ":r=", fps,
    " -framerate ", fps,
    " -i ", files_dir, "/frame_%04d.png",
    " -filter_complex \"[0:v][1:v]overlay[video]\" -map \"[video]\" -shortest",
    " -pix_fmt yuv420p ", full_filename
    )
  
  shell(call_msg, mustWork = TRUE)
  
  print(str_c("Movie saved to ", full_filename))
  
}

projetinho <- function(res = 7, alpha = 0.04, dpi = 250, width = 2000, height = 1300) {
  
  otp_response <- read_rds(str_c("./data/otp_response_pt_map_", res, ".rds"))
  itinerary_details <- generate_sf(otp_response, res, FALSE)
  frames <- frames_info(itinerary_details)
  n <- nrow(frames)
  rj <- read_rds("./data/shape_rio_state.rds")
  rio <- read_rds("./data/shape_rio_muni.rds")
  map_generator(frames, rj, rio, from = n, to = n, alpha = alpha, dpi = dpi, width = width, height = height)
    
}