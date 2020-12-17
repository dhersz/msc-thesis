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
    plot.margin = margin(b = 0)
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
    fill = guide_colorbar(title.position = "left", title.vjust = 1)
  )
  
  # * * pop and jobs spatial dist -------------------------------------------

  xlim <- c(st_bbox(rio_border)[1], st_bbox(rio_border)[3])
  ylim <- c(st_bbox(expanded_rio_border)[2], st_bbox(rio_border)[4])
  
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
  
}