library(ggplot2)
library(data.table)
library(dplyr)
library(sf)
source("./code/06_analyse_results.R")

generate_maps <- function(grid_name = "grid_with_data_aop", 
                          router = "rio_no_inter", 
                          res = 8) {
  
  # * read and prepare data -------------------------------------------------
  
  router_folder     <- paste0("./data/", router, "_res_", res)
  
  study_area_folder <- paste0(router_folder, "/study_area")
  if (!dir.exists(study_area_folder)) dir.create(study_area_folder)
  
  grid_data <- readr::read_rds(paste0(router_folder, "/", grid_name, ".rds")) %>% 
    mutate(
      pop_density = as.numeric((population / 1000) / units::set_units(st_area(geometry), "km^2")),
      job_density = as.numeric((opportunities / 1000) / units::set_units(st_area(geometry), "km^2")),
      avg_income  = (total_income / population) / 1000
    )
  
  #return(grid_data)
  
  rapid_transit_stations <- extract_rapid_transit(router)

  # * maps ------------------------------------------------------------------
  
  # dimensions and resolution
  
  dpi <- 600
  min_width <- 7
  max_width <- 13 
  dim_unit <- "cm"
  
  # auxiliary sf objects
  
  rj_state   <- readr::read_rds("./data/rj_state.rds")
  rio_border <- readr::read_rds("./data/rio_municipality.rds")
  
  # expanded border to manually extend plots' bounding box
  
  expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(3000) %>% 
    st_transform(st_crs(rio_border))
  
  # object to ensure scale bar is attached to them bottom (but no too much) of plots
  
  less_expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(1500) %>% 
    st_transform(st_crs(rio_border))
  
  scalebar_data <- rio_border
  scalebar_data_bbox <- st_bbox(scalebar_data)
  scalebar_data_bbox["ymin"] <- st_bbox(less_expanded_rio_border)["ymin"]
  attr(st_geometry(scalebar_data), "bbox") <- scalebar_data_bbox
  
  # object to ensure north bar is attached to the top-left of the plot
  
  north_data <- rio_border
  north_data_bbox <- st_bbox(north_data)
  north_data_bbox["xmin"] <- st_bbox(expanded_rio_border)["xmin"]
  attr(st_geometry(north_data), "bbox") <- north_data_bbox
  
  # trim grid data for pretty plots - suppress st_intersection warnings
  
  trimmed_grid_geometry <- st_intersection(
    st_geometry(grid_data) %>% st_transform(5880), 
    st_geometry(rio_border) %>% st_transform(5880)
  ) %>% 
    st_transform(st_crs(rio_border))
  
  trimmed_grid_data <- mutate(grid_data, geometry = trimmed_grid_geometry)
  
  # auxiliary ggplot theme
  
  theme_maps <- theme(
    axis.title = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank(), 
    panel.grid = element_blank(),
    strip.text.x = element_text(size = 13), 
    strip.text.y = element_text(size = 13),
    strip.background = element_rect(fill = NA),
    panel.background = element_rect(fill = "#aadaff"),
    legend.direction = "horizontal",
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "#aadaff"),
    legend.position = c(0.99, 0),
    legend.title = element_text(size = 10, hjust = 1),
    legend.text = element_text(size = 10),
    plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
  )
  
  # default scale bar
  
  scalebar_maps <- ggsn::scalebar(
    data = scalebar_data, 
    dist = 10, 
    dist_unit = "km",
    location = "bottomleft", 
    transform = TRUE, 
    model = "WGS84",
    border.size = 0.3, 
    st.size = 3,
    st.dist = 0.03
  )
  
  # default north symbol
  
  north_maps <- ggsn::north(
    data = north_data,
    location = "topleft",
    scale = 0.15,
    symbol = 4
  )
  
  # default legend guides
  
  guides_maps <- guides(
    fill = guide_colorbar(title.position = "left", title.vjust = 1),
    linetype = guide_legend(title.position = "top", title.hjust = 0)
  )
  
  # maps bounding box
  
  xlim <- c(st_bbox(rio_border)[1], st_bbox(rio_border)[3])
  ylim <- c(st_bbox(expanded_rio_border)[2], st_bbox(rio_border)[4])
  
  # * * pop and jobs spatial dist -------------------------------------------

  # population

  pp <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(data = trimmed_grid_data, aes(fill = pop_density), color = NA) +
    geom_sf(data = rio_border, fill = NA, color = "gray60", size = 0.3) +
    scale_fill_gradient(
      name = "Population\n(1000/km²)",
      low = "#efeeec",
      high = "firebrick3"
    ) +
    coord_sf(xlim = xlim, ylim = ylim) +
    north_maps +
    theme() +
    guides_maps +
    theme_maps

  # average income

  pi <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(data = trimmed_grid_data, aes(fill = avg_income), color = NA) +
    geom_sf(data = rio_border, fill = NA, color = "gray60", size = 0.3) +
    scale_fill_gradient(
      name = "Income per cap.\n(1000 R$)",
      low = "#efeeec",
      high = "firebrick3",
      na.value = "#efeeec"
    ) +
    coord_sf(xlim = xlim, ylim = ylim) +
    theme() +
    guides_maps +
    theme_maps

  # jobs

  pj <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(data = trimmed_grid_data, aes(fill = job_density), color = NA) +
    geom_sf(data = rio_border, fill = NA, color = "gray60", size = 0.3) +
    scale_fill_gradient(
      name = "Jobs\n(1000/km²)",
      low = "#efeeec",
      high = "firebrick3"
    ) +
    coord_sf(xlim = xlim, ylim = ylim) +
    scalebar_maps +
    theme() +
    guides_maps +
    theme_maps

  # joined together

  pf <- cowplot::plot_grid(pp, pi, pj, ncol = 1)

  # save final result

  filename <- paste0(study_area_folder, "/pop_avginc_job.tiff")

  ggsave(
    filename,
    plot = pf,
    device = "tiff",
    width = max_width,
    height = 22.2,
    units = dim_unit,
    dpi = dpi
  )

  # * * transit distribution ------------------------------------------------

  gtfs_fet_path <- paste0("./otp/graphs/", router, "/gtfs_fetranspor_fsub_ninter_nfresc_nout.zip")
  gtfs_sup_path <- paste0("./otp/graphs/", router, "/gtfs_supervia.zip")
  
  gtfs_fet <- gtfstools::read_gtfs(gtfs_fet_path, c("trips", "shapes", "routes"))
  gtfs_sup <- gtfstools::read_gtfs(gtfs_sup_path, c("trips", "shapes", "routes"))
  
  # subway shapes - pavuna and uruguai
  
  subway_trips  <- c("1202_1188_I_1", "15353_16478_I_alt")
  subway_shapes <- gtfstools::get_trip_geometry(
    gtfs_fet, 
    trip_id = subway_trips, 
    file = "shapes", 
    crs = st_crs(rio_border)
  )
  subway_shapes$mode <- "subway"
  
  # rail shapes
  
  rail_routes <- gtfs_sup$routes[route_type == 2]$route_id
  rail_trips  <- gtfs_sup$trips[route_id %chin% rail_routes & direction_id == 0]
  rail_trips  <- rail_trips[rail_trips[, .I[1], keyby = .(trip_headsign)]$V1]
  rail_trips  <- rail_trips[! trip_headsign %chin% c("Campo Grande", "Gramacho")]$trip_id
  rail_shapes <- gtfstools::get_trip_geometry(
    gtfs_sup, 
    trip_id = rail_trips, 
    file = "shapes", 
    crs = st_crs(rio_border)
  )
  rail_shapes$mode <- "rail"
  
  # brt shapes
  
  brt_routes <- gtfs_fet$routes[grepl("BRT", route_short_name)]$route_id
  brt_trips  <- gtfs_fet$trips[route_id %chin% brt_routes & direction_id == 1]
  brt_trips  <- brt_trips[brt_trips[, .I[1], keyby = .(route_id)]$V1]$trip_id
  brt_shapes <- gtfstools::get_trip_geometry(
    gtfs_fet, 
    trip_id = brt_trips, 
    file = "shapes", 
    crs = st_crs(rio_border)
  )
  brt_shapes$mode <- "brt"
  
  # municipal buses shapes
  
  bus_routes <- gtfs_fet$routes[route_type == 3 & !grepl("BRT", route_short_name)]$route_id
  bus_trips  <- gtfs_fet$trips[route_id %chin% bus_routes & direction_id == 1]
  bus_trips  <- bus_trips[bus_trips[, .I[1], keyby = .(route_id)]$V1]$trip_id
  bus_shapes <- gtfstools::get_trip_geometry(
    gtfs_fet, 
    trip_id = bus_trips, 
    file = "shapes", 
    crs = st_crs(rio_border)
  )
  bus_shapes$mode <- "bus"
  
  # bind shapes together
  
  transit_shapes <- rbind(bus_shapes, brt_shapes, rail_shapes, subway_shapes)
  transit_shapes$mode <- factor(
    transit_shapes$mode,
    labels = c("BRT", "Bus", "Rail", "Subway")
  )
  
  # plot settings
  
  pt <- ggplot() + 
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(data = transit_shapes, aes(linetype = mode, color = mode, size = mode)) +
    geom_sf(
      data = subset(transit_shapes, mode != "Bus"), 
      aes(linetype = mode, color = mode, size = mode)
    ) +
    geom_sf(data = rio_border, fill = NA, color = "gray60", size = 0.3) +
    scale_linetype_manual(
      name = "Mode",
      values = c("solid", "solid", "twodash", "dashed")
    ) +
    scale_color_manual(
      name = "Mode", 
      values = c("firebrick3", "gray85", "royalblue4", "seagreen4")
    ) +
    scale_size_manual(name = "Mode", values = c(0.5, 0.3, 0.5, 0.5)) +
    coord_sf(xlim = xlim, ylim = ylim) +
    north_maps +
    scalebar_maps +
    guides_maps +
    theme_maps
  
  # save final result

  filename <- paste0(study_area_folder, "/transit_dist.tiff")

  ggsave(
    filename,
    plot = pt,
    device = "tiff",
    width = max_width,
    height = 7.4,
    units = dim_unit,
    dpi = dpi
  )
     
}