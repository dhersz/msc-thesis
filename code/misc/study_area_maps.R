library(ggplot2)
library(data.table)
library(dplyr)
library(sf)
source("./code/06_analyse_results.R")

generate_maps <- function(grid_name = "grid_with_data_aop", 
                          router = "rio_no_inter", 
                          res = 8,
                          which = c("all", "aps", "pop_job", "transit"),
                          lang = c("en", "pt")) {
  
  # * read and prepare data -------------------------------------------------
  
  router_folder <- paste0("./data/", router, "_res_", res)
  
  study_area_folder <- paste0(router_folder, "/study_area")
  if (!dir.exists(study_area_folder)) dir.create(study_area_folder)
  
  grid_data <- readr::read_rds(paste0(router_folder, "/", grid_name, ".rds")) %>% 
    mutate(
      pop_density = as.numeric((population / 1000) / units::set_units(st_area(geometry), "km^2")),
      job_density = as.numeric((opportunities / 1000) / units::set_units(st_area(geometry), "km^2")),
      avg_income  = (total_income / population) / 1000
    )
  
  lang <- lang[1]
  labels <- generate_labels(lang)
  
  # * maps ------------------------------------------------------------------
  
  # dimensions and resolution
  
  dpi <- 600
  min_width <- 7
  max_width <- 13 
  dim_unit <- "cm"
  
  # auxiliary sf objects
  
  rj_state   <- readr::read_rds("./data/rj_state.rds")
  rio_border <- readr::read_rds("./data/rio_municipality.rds")
  parques <- readr::read_rds("./data/parques_naturais.rds")
  rio_aps <- readr::read_rds("./data/rio_aps.rds")
  
  # transit shapes, to be used on transit and pop_job maps
  
  if (which[1] %in% c("all", "pop_job", "transit"))
      transit_shapes <- generate_transit_shapes(
        router, 
        crs = st_crs(rio_border)
      )
  
  # expanded border to manually extend plots' bounding box
  # 'more_expanded_rio_border' is used in the first facet of pop_jobs
  
  expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(3000) %>% 
    st_transform(st_crs(rio_border))
  
  more_expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(9000) %>% 
    st_transform(st_crs(rio_border))
  
  # object to ensure scale bar is attached to the bottom (but no too much) of 
  # plots
  # scalebar_pop_job is used only on pop_job
  
  less_expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(1500) %>% 
    st_transform(st_crs(rio_border))
  
  scalebar_data <- rio_border
  scalebar_data_bbox <- st_bbox(scalebar_data)
  scalebar_data_bbox["ymin"] <- st_bbox(less_expanded_rio_border)["ymin"]
  attr(st_geometry(scalebar_data), "bbox") <- scalebar_data_bbox
  
  abit_more_expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(7500) %>% 
    st_transform(st_crs(rio_border))
  
  scalebar_popjob_data <- rio_border
  scalebar_popjob_data_bbox <- st_bbox(scalebar_popjob_data)
  scalebar_popjob_data_bbox["ymin"] <- st_bbox(abit_more_expanded_rio_border)["ymin"]
  attr(st_geometry(scalebar_popjob_data), "bbox") <- scalebar_popjob_data_bbox
  
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
  
  # scale bars
  
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
  
  scalebar_popjob <- ggsn::scalebar(
    data = scalebar_popjob_data, 
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
  ylim_lower <- c(st_bbox(more_expanded_rio_border)[2], st_bbox(rio_border)[4])

  # * * areas de planejamento (APs, planning areas) --------------------------

  if (which[1] == "all" | which[1] == "aps") {
    
    parques_trimmed <- sf::st_intersection(
      sf::st_transform(parques, 5880),
      sf::st_transform(rio_aps, 5880)
    )
    parques_trimmed <- sf::st_transform(parques_trimmed, 4674)
    
    pa <- ggplot() +
      geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
      geom_sf(
        data = parques_trimmed,
        aes(fill = "Parques"),
        color = NA,
        size = 0.3
      ) +
      geom_sf(data = rio_aps, aes(color = "APs"), fill = NA, size = 0.3) +
      geom_sf_text(data = rio_aps, aes(label = ap_code)) +
      coord_sf(xlim = xlim, ylim = ylim) +
      north_maps +
      scalebar_maps +
      theme_maps +
      scale_color_manual(
        name = "",
        values = c("APs" = "gray60"), 
        labels = labels$aps$aps
      ) +
      scale_fill_manual(
        name = "",
        values = c("Parques" = "gray85"),
        labels = labels$aps$parks
      ) +
      guides(
        fill = guide_legend(override.aes = list(color = "#efeeec")),
        color = guide_legend(override.aes = list(fill = "#efeeec"))
      ) +
      theme(legend.spacing.y = unit(0, "pt"))
    
    # save final result
    
    filename <- paste0(study_area_folder, "/", lang, "/aps.tiff")
    
    ggsave(
      filename,
      plot = pa,
      device = "tiff",
      width = max_width,
      height = 7.4,
      units = dim_unit,
      dpi = dpi
    )
    
  }
  
  # * * pop and jobs spatial dist -------------------------------------------

  if (which[1] == "all" | which[1] == "pop_job") {
    
    # geometries that will be used on all three "facets"
    # if lang == "en" the plots are geared towards the paper, and the
    # conditional geom shows the transit distribution
    # if lang == "pt" the plots are geared towards the thesis, and the
    # conditional goem shows the planning areas
    # the rio_border is also included here because the transit layer should be
    # on top of it, but the ap layer should be below it
    
    if (lang == "en") {
      
      conditional_geom <- list(
        geom_sf(data = rio_border, fill = NA, color = "gray60", size = 0.3),
        geom_sf(
          data = subset(transit_shapes, mode %in% c("BRT", "Rail", "Subway")),
          aes(linetype = mode),
          color = "gray50",
          size = 0.5
        ),
        scale_linetype_manual(
          name = "Transit\ncorridor",
          values = c("solid", "twodash", "dashed")
        )
      )
      
      conditional_guide <- list(
        guides(
          fill = guide_colorbar(
            title.position = "left", 
            title.vjust = 1, 
            order = 1
          ),
          linetype = guide_legend(
            title.position = "left", 
            title.vjust = 1, 
            order = 2
          )
        )
      )
      
    } else if (lang == "pt") {
      
      conditional_geom <- list(
        geom_sf(data = rio_aps, aes(color = "APs"), fill = NA, size = 0.3),
        geom_sf(data = rio_border, fill = NA, color = "gray60", size = 0.3),
        scale_color_manual(
          name = "",
          values = c("APs" = "gray65"), 
          labels = "Áreas de planejamento"
        )
      )
      
      conditional_guide <- list(
        guides(
          color = guide_legend(
            title.position = "left",
            title.vjust = 1, 
            order = 1,
            override.aes = list(fill = "#efeeec")
          ),
          fill = guide_colorbar(
            title.position = "left", 
            title.vjust = 1, 
            order = 2
          )
        )
      )
      
    }
    
    # population
  
    pp <- ggplot() +
      geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
      geom_sf(data = trimmed_grid_data, aes(fill = pop_density), color = NA) +
      scale_fill_gradient(
        name = labels$pop_job$pop_density,
        low = "#efeeec",
        high = "firebrick3"
      ) +
      conditional_geom +
      conditional_guide +
      coord_sf(xlim = xlim, ylim = ylim_lower) +
      north_maps +
      scalebar_popjob +
      theme_maps +
      theme(
        legend.box.just = "right",
        legend.spacing.y = unit(0, "pt"),
        legend.text = element_text(margin = margin(t = 2))
      )
  
    # average income
  
    pi <- ggplot() +
      geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
      geom_sf(data = trimmed_grid_data, aes(fill = avg_income), color = NA) +
      geom_sf(data = rio_border, fill = NA, color = "gray60", size = 0.3) +
      conditional_geom +
      scale_fill_gradient(
        name = labels$pop_job$income_per_cap,
        low = "#efeeec",
        high = "firebrick3",
        na.value = "#efeeec"
      ) +
      conditional_geom +
      coord_sf(xlim = xlim, ylim = ylim) +
      theme() +
      guides(
        fill = guide_colorbar(title.position = "left", title.vjust = 1),
        linetype = "none",
        color = "none"
      ) +
      theme_maps
  
    # jobs
  
    pj <- ggplot() +
      geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
      geom_sf(data = trimmed_grid_data, aes(fill = job_density), color = NA) +
      geom_sf(data = rio_border, fill = NA, color = "gray60", size = 0.3) +
      scale_fill_gradient(
        name = labels$pop_job$job_density,
        low = "#efeeec",
        high = "firebrick3",
        trans = "pseudo_log",
        breaks = c(0, 10, 20, 60)
      ) +
      conditional_geom +
      coord_sf(xlim = xlim, ylim = ylim) +
      theme() +
      guides(
        fill = guide_colorbar(title.position = "left", title.vjust = 1),
        linetype = "none",
        color = "none"
      ) +
      theme_maps
  
    # add labels
    
    pp <- cowplot::ggdraw(pp) + cowplot::draw_plot_label("A", fontface = "bold")
    
    pi <- cowplot::ggdraw(pi) + cowplot::draw_plot_label("B", fontface = "bold")
    
    pj <- cowplot::ggdraw(pj) + cowplot::draw_plot_label("C", fontface = "bold")
    
    # joined together
  
    pf <- cowplot::plot_grid(pp, pi, pj, ncol = 1, rel_heights = c(1.145, 1, 1))
  
    # save final result
  
    filename <- paste0(study_area_folder, "/", lang, "/pop_avginc_job.png")
  
    ggsave(
      filename,
      plot = pf,
      width = max_width,
      height = 23.2,
      units = dim_unit,
      dpi = dpi
    )
    
  }

  # * * transit distribution ------------------------------------------------

  if (which[1] == "all" | which[1] == "transit") {
  
    # plot settings
  
    pt <- ggplot() +
      geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
      geom_sf(data = transit_shapes, aes(linetype = mode, color = mode, size = mode)) +
      geom_sf(
        data = subset(transit_shapes, (mode != "Bus" & mode != "VLT")),
        aes(linetype = mode, color = mode, size = mode)
      ) +
      geom_sf(data = rio_border, fill = NA, color = "gray60", size = 0.3) +
      scale_linetype_manual(
        name = labels$transit$legend_title,
        values = c("solid", "solid", "dotted", "twodash", "dashed", "solid"),
        labels = labels$transit$legend_lable
      ) +
      scale_color_manual(
        name = labels$transit$legend_title,
        values = c("firebrick3", "gray85", "sienna3", "royalblue4", "seagreen4", "dodgerblue3"),
        labels = labels$transit$legend_lable
      ) +
      scale_size_manual(
        name = labels$transit$legend_title, 
        values = c(0.5, 0.3, 0.5, 0.5, 0.5, 0.5),
        labels = labels$transit$legend_lable
      ) +
      coord_sf(xlim = xlim, ylim = ylim) +
      north_maps +
      scalebar_maps +
      guides_maps +
      theme_maps +
      theme(
        legend.text = element_text(size = 9),
        legend.margin = margin(r = 5.5, b = 5.5)
      )
  
    # save final result
    
    filename <- paste0(study_area_folder, "/", lang, "/transit_dist.tiff")
  
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
     
}

generate_transit_shapes <- function(router = "rio_no_inter", crs) {
  
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
    crs = crs
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
    crs = crs
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
    crs = crs
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
    crs = crs
  )
  bus_shapes$mode <- "bus"
  
  # vlt shapes
  
  vlt_routes <- gtfs_fet$routes[grepl("VLT", route_short_name)]$route_id
  vlt_trips  <- gtfs_fet$trips[route_id %chin% vlt_routes]$trip_id
  vlt_shapes <- gtfstools::get_trip_geometry(
    gtfs_fet,
    trip_id = vlt_trips,
    file = "shapes",
    crs = crs
  )
  vlt_shapes$mode <- "vlt"
  
  # ferry shapes
  
  ferry_routes <- gtfs_fet$routes[route_type == 4]$route_id
  ferry_trips  <- gtfs_fet$trips[route_id %chin% ferry_routes & direction_id == 1]$trip_id
  ferry_shapes <- gtfstools::get_trip_geometry(
    gtfs_fet,
    trip_id = ferry_trips,
    file = "shapes",
    crs = crs
  )
  ferry_shapes$mode <- "ferry"
  
  # bind shapes together
  
  transit_shapes <- rbind(
    ferry_shapes, 
    vlt_shapes, 
    bus_shapes, 
    brt_shapes, 
    rail_shapes, 
    subway_shapes
  )
  transit_shapes$mode <- factor(
    transit_shapes$mode,
    labels = c("BRT", "Bus", "Ferry", "Rail", "Subway", "VLT")
  )
  
  return(transit_shapes)
  
}



generate_labels <- function(lang) {
  
  if (lang == "pt") {
    
    list(
      aps = list(
        parks = "Parques naturais",
        aps = "Áreas de planejamento"
      ),
      pop_job = list(
        pop_density = "Dens. populacional\n(1000/km²)",
        income_per_cap = "Renda per cap.\n(1000 R$)",
        job_density = "Dens. de empregos\n(1000/km²)"
      ),
      transit = list(
        legend_title = "Modo",
        legend_lable = c("BRT", "Ônibus", "Barca", "Trem", "Metrô", "VLT")
      )
    )
    
  } else if (lang == "en") {
    
    list(
      aps = list(
        parks = "Natural parks",
        aps = "Planning areas"
      ),
      pop_job = list(
        pop_density = "Population density\n(1000/km²)",
        income_per_cap = "Income per cap.\n(1000 R$)",
        job_density = "Job density\n(1000/km²)"
      ),
      transit = list(
        legend_title = "Mode",
        legend_lable = c("BRT", "Bus", "Ferry", "Rail", "Subway", "VLT")
      )
    )
    
  }
  
}