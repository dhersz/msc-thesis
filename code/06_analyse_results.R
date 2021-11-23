library(dplyr)
library(sf)
library(ggplot2)
library(data.table)
library(ggtext)

analyse_results <- function(grid_name = "grid_with_data_aop",
                            router = "rio_no_inter",
                            n_quantiles = 10,
                            res = 8,
                            lang = "en") {
  
  # * read and prepare data -------------------------------------------------
  
  
  router_folder        <- paste0("./data/", router, "_res_", res)
  accessibility_folder <- paste0(router_folder, "/accessibility")
  
  accessibility_path <- paste0(accessibility_folder, "/median_accessibility.rds")
  accessibility_data <- readr::read_rds(accessibility_path)
  
  grid_data <- setDT(
    readr::read_rds(paste0(router_folder, "/", grid_name, ".rds"))
  )
  grid_data[, avg_income := total_income / population]
  
  # classify hexagons according to their residents' avg income per capita quantile.
  # not sure why, but the values returned by wtd.quantile() seem to have some 
  # kind of issue with their rounding. when trying to classify each hexagon into
  # a income quantile the values right at the boundaries of each quantile
  # interval are not consistently accounted (sometimes it behaves like a
  # open-close limit, sometimes as a close-open, etc).
  # in order to fix this, and to treat the intervals consistently as open-close
  # like quantile() does (with the exception of the first one being close-close),
  # the 0% value is set to -Inf e the avg_income - 0.0001 is used to place each
  # entrance in a quantile (i.e. if the Q2 interval is 10-20 and we're trying to
  # classify in which decile the 10 is in we use the values 9.9999 and 19.9999,
  # therefore the 10 is in Q1 and 20 is in Q2)
  
  qntls <- Hmisc::wtd.quantile(
    x       = grid_data$avg_income,
    weights = grid_data$population,
    probs   = seq(0, 1, 1 / n_quantiles)
  )
  qntls[1] <- -Inf
  
  grid_data[, income_quantile := cut(avg_income - 0.0001, qntls, labels = FALSE, include.lowest = TRUE)]
  
  # join grid data to accessibility data
  
  accessibility_data[grid_data, 
                     on = "id", 
                     `:=`(population = i.population, 
                          opportunities = i.opportunities,
                          income_quantile = i.income_quantile, 
                          geometry = i.geometry)]
  
  ttimes <- c(30, 60, 90)
  mcosts <- c(0, 4.05, 4.7, 5, 7.1, 8.75, 1000)
  
  text_labels <- text_labels_generator(mcosts, ttimes, lang)
  
  # figures dimensions and resolution
  
  dpi <- 600
  min_width <- 7
  max_width <- 13 
  dim_unit <- "cm"
  

  # ANPET analysis ----------------------------------------------------------

  
  analysis_folder <- paste0(router_folder, "/analysis")
  if (!file.exists(analysis_folder)) dir.create(analysis_folder)
  
  analysis_folder <- paste0(analysis_folder, "/", lang)
  if (!file.exists(analysis_folder)) dir.create(analysis_folder)
  
  analysis_folder <- paste0(analysis_folder, "/different_costs")
  if (!file.exists(analysis_folder)) dir.create(analysis_folder)
  
  purrr::walk(ttimes, function(tt) {
    
    filtered_data <- accessibility_data[(bilhete_unico == "with") & 
                                        (travel_time == tt) & 
                                        (monetary_cost %in% mcosts)]
    
    # distribution_map(copy(filtered_data), tt, mcosts, text_labels, analysis_folder)
    # reduction_map(copy(filtered_data), tt, mcosts, text_labels, analysis_folder)
    # reduction_hist(copy(filtered_data), tt, mcosts, text_labels, analysis_folder)
    # distribution_boxplot(copy(filtered_data), tt, mcosts, text_labels, analysis_folder, n_quantiles)
    # distribution_theil(copy(filtered_data), tt, mcosts, text_labels, analysis_folder)
    # average_access_different_costs(grid_data, travel_time[i], percentage_minimum_wage, n_quantiles, text_labels, res)
    
  })
  

  # WSTLUR/thesis analysis --------------------------------------------------

  
  analysis_folder <- paste0(router_folder, "/analysis/", lang)
  
  if (lang == "en") {
    
    # across_cost_palma(copy(accessibility_data), text_labels, analysis_folder, bu = "with", tt = ttimes, max_width, dpi, dim_unit)
    # across_time_palma(copy(accessibility_data), text_labels, analysis_folder, bu = "with", mc = mcosts, max_width, dpi, dim_unit)
    
    accessibility_data <- accessibility_data[
      (bilhete_unico == "with") &
        (travel_time %in% ttimes) &
        (monetary_cost %in% mcosts)
    ]
    
    all_thresholds_maps(copy(accessibility_data), ttimes, mcosts, bu = "with", max_width, dpi, dim_unit, text_labels, analysis_folder)
    couple_thresholds_maps(copy(accessibility_data), ttimes, mcosts, "with", max_width, dpi, dim_unit, text_labels, analysis_folder)
    one_threshold_map(copy(accessibility_data), ttimes, mcosts, "with", max_width, dpi, dim_unit, text_labels, analysis_folder)
    
  } else if (lang == "pt") {
    
    # across_cost_palma(copy(accessibility_data), text_labels, analysis_folder, bu = "with", tt = ttimes, max_width, dpi, dim_unit)
    # across_cost_theil(copy(accessibility_data), text_labels, analysis_folder, bu = "with", tt = ttimes, max_width, dpi, dim_unit)
    # across_cost_comps(copy(accessibility_data), text_labels, analysis_folder, bu = "with", tt = ttimes, max_width, dpi, dim_unit)
    
    # across_time_palma(copy(accessibility_data), text_labels, analysis_folder, bu = "with", mc = mcosts, max_width, dpi, dim_unit)
    # across_time_theil(copy(accessibility_data), text_labels, analysis_folder, bu = "with", mc = mcosts, max_width, dpi, dim_unit)
    # across_time_comps(copy(accessibility_data), text_labels, analysis_folder, bu = "with", mc = mcosts, max_width, dpi, dim_unit)
    
    accessibility_data <- accessibility_data[
      (bilhete_unico == "with") &
        (travel_time %in% ttimes) &
        (monetary_cost %in% mcosts)
    ]
    
    # all_thresholds_maps(copy(accessibility_data), ttimes, mcosts, bu = "with", max_width, dpi, dim_unit, text_labels, analysis_folder)
    # couple_thresholds_maps(copy(accessibility_data), ttimes, mcosts, "with", max_width, dpi, dim_unit, text_labels, analysis_folder)
    # one_threshold_map(copy(accessibility_data), ttimes, mcosts, "with", max_width, dpi, dim_unit, text_labels, analysis_folder)
    
  }
  
}


theme_thesis <- function(style = c("map", "graphic")) {
  
  if (style == "map") {
    
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(),
      axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      strip.text.x = element_markdown(size = 11), 
      strip.text.y.left = element_markdown(size = 10),
      strip.background = element_rect(fill = NA),
      panel.background = element_rect(fill = "#aadaff"),
      legend.position = "bottom", 
      legend.box.just = "right",
      legend.box.spacing = unit(0 ,"points"),
      legend.title = element_text(size = 10, hjust = 1),
      plot.margin = margin(b = 0)
    )
    
  } else if (style == "graphic") {
    
    theme(
      strip.text.x = element_text(size = 13),
      strip.background.x = element_rect(fill = NA),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_markdown(size = 10),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_markdown(size = 10),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "gray94")
    )
    
  }
  
}


text_labels_generator <- function(mcosts, ttimes, lang) {
  
  # create the text labels used in the maps and graphics according to the specified language
  
  mcosts <- format(mcosts, nsmall = 2)
  mcosts <- stringr::str_replace(mcosts, " +", "")
  mcosts <- paste0("R$ ", mcosts)
  
  if (lang == "pt") {
    
    text_labels <- list(
      "lang" = lang,
      "distribution_map" = list(
        "facets_title" = ifelse(mcosts != 1000, paste0("R$ ", format(mcosts, nsmall = 2)), "Não considerado"),
        "legend_title" = "  Empregos acessíveis (% do total)"
      ),
      "boxplot" = list(
        "facets_title" = ifelse(mcosts != 1000, paste0("R$ ", format(mcosts, nsmall = 2)), "Não considerado"),
        "palma_ratio" = "Razão de Palma: ",
        "y_axis" = "Empregos acessíveis (% do total)",
        "x_axis" = "Decil de renda"
      ),
      "theil" = list(
        "bar_labels" = ifelse(mcosts != 1000, paste0("R$ ", format(mcosts, nsmall = 2)), "Não considerado"),
        "y_axis" = "Índice de Theil",
        "x_axis" = "Valor limite de custo",
        "component" = "Componente",
        "components_names" = c("Entregrupos", "Intragrupos")
      ),
      "average_access" = list(
        "facets_title" = paste0("Custo ", ifelse(mcosts <= 100, paste0("<= ", mcosts, "% sal. mín."), "não consid.")),
        "y_axis" = "Acessibilidade média\n(% do total de empregos)",
        "x_axis" = "Decil de renda"
      ),
      "across_cost_palma" = list(
        "legend_title" = "Limite de tempo\nde viagem (min.)",
        "y_axis" = "Razão de Palma",
        "x_axis" = "Limite de custo monetário (R$)",
        "x_labels" = c(
          "0",
          "<span style='font-size:8pt'>4,05</span>",
          "",
          "5",
          "<span style='font-size:8pt'>7,1</span>",
          "<span style='font-size:8pt'>8,75</span>",
          "10",
          "Sem restrição"
        )
      ),
      "across_time_palma" = list(
        "legend_title" = "Limite de custo\nmonetário (R$)",
        "legend_values" = c(
          paste0("0,00 ", "<img src='data/icons/walking.png' width='10'>"),
          paste0("4,05 ", "<img src='data/icons/bus.png' width='10'>+<img src='data/icons/brt.png' width='10'>"),
          paste0("4,70 ", "<img src='data/icons/train.png' width='10'>"),
          paste0("5,00 ", "<img src='data/icons/subway.png' width='10'>"),
          paste0("7,10 ", "<img src='data/icons/brt.png' width='10'>+<img src='data/icons/subway.png' width='10'>"),
          paste0("8,75 ", "(<img src='data/icons/bus.png' width='8'>+<img src='data/icons/brt.png' width='8'>)+<img src='data/icons/train.png' width='8'>"),
          "Sem restrição"
        ),
        "y_axis" = "Razão de Palma",
        "x_axis" = "Limite de tempo de viagem (min.)"
      ),
      "across_cost_theil" = list(
        "legend_title" = "Limite de tempo\nde viagem (min.)",
        "y_axis" = "Índice de Theil",
        "x_axis" = "Limite de custo monetário (R$)",
        "x_labels" = c(
          "0",
          "<span style='font-size:8pt'>4,05</span>",
          "",
          "5",
          "<span style='font-size:8pt'>7,1</span>",
          "<span style='font-size:8pt'>8,75</span>",
          "10",
          "Sem restrição"
        )
      ),
      "across_time_theil" = list(
        "legend_title" = "Limite de custo\nmonetário (R$)",
        "legend_values" = c(
          paste0("0,00 ", "<img src='data/icons/walking.png' width='10'>"),
          paste0("4,05 ", "<img src='data/icons/bus.png' width='10'>+<img src='data/icons/brt.png' width='10'>"),
          paste0("4,70 ", "<img src='data/icons/train.png' width='10'>"),
          paste0("5,00 ", "<img src='data/icons/subway.png' width='10'>"),
          paste0("7,10 ", "<img src='data/icons/brt.png' width='10'>+<img src='data/icons/subway.png' width='10'>"),
          paste0("8,75 ", "(<img src='data/icons/bus.png' width='8'>+<img src='data/icons/brt.png' width='8'>)+<img src='data/icons/train.png' width='8'>"),
          "Sem restrição"
        ),
        "y_axis" = "Índice de Theil",
        "x_axis" = "Limite de tempo de viagem (min.)"
      ),
      "across_cost_comps" = list(
        "color_title" = "Limite de tempo\nde viagem (min.)",
        "lntp_title" = "Componente",
        "y_axis" = "Componente entregrupos\ndo Índice de Theil",
        "x_axis" = "Limite de custo monetário (R$)",
        "x_labels" = c(
          "0",
          "<span style='font-size:8pt'>4,05</span>",
          "",
          "5",
          "<span style='font-size:8pt'>7,1</span>",
          "<span style='font-size:8pt'>8,75</span>",
          "10",
          "<span style='font-size:8pt'>Sem restrição</span>"
        ),
        "components" = list(Entregrupos = "between", Intragrupos = "within")
      ),
      "across_time_comps" = list(
        "color_title" = "Limite de custo\nmonetário (R$)",
        "lntp_title" = "Componente",
        "legend_values" = c(
          paste0("0,00 ", "<img src='data/icons/walking.png' width='10'>"),
          paste0("4,05 ", "<img src='data/icons/bus.png' width='10'>+<img src='data/icons/brt.png' width='10'>"),
          paste0("4,70 ", "<img src='data/icons/train.png' width='10'>"),
          paste0("5,00 ", "<img src='data/icons/subway.png' width='10'>"),
          paste0("7,10 ", "<img src='data/icons/brt.png' width='10'>+<img src='data/icons/subway.png' width='10'>"),
          paste0("8,75 ", "(<img src='data/icons/bus.png' width='8'>+<img src='data/icons/brt.png' width='8'>)+<img src='data/icons/train.png' width='8'>"),
          "Sem restrição"
        ),
        "y_axis" = "Componente entregrupos\ndo Índice de Theil",
        "x_axis" = "Limite de tempo de viagem (min.)",
        "components" = list(Entregrupos = "between", Intragrupos = "within")
      ),
      "all_thresholds_maps" = list(
        "legend_title" = "Empregos acessíveis\n(% do total)",
        "times_facet_titles" = paste0(ttimes, " min."),
        "costs_facet_titles" = c(
          paste0("<img src='data/icons/walking.png' width='10'>", "<br>R$ 0,00"),
          paste0("<img src='data/icons/bus.png' width='10'>+<img src='data/icons/brt.png' width='10'>", "<br>R$ 4,05"),
          paste0("<img src='data/icons/train.png' width='10'>", "<br>R$ 4,70"),
          paste0("<img src='data/icons/subway.png' width='10'>", "<br>R$ 5,00"),
          paste0("<img src='data/icons/brt.png' width='10'>+<img src='data/icons/subway.png' width='10'>", "<br>R$ 7,10"),
          paste0("(<img src='data/icons/bus.png' width='8'>+<img src='data/icons/brt.png' width='8'>)+<img src='data/icons/train.png' width='8'>", "<br>R$ 8,75"),
          "Sem<br>restrição"
        )
      )
    )
    
  } else if (lang == "en") {
    
    text_labels <- list(
      "lang" = lang,
      "distribution_map" = list(
        "facets_title" = ifelse(mcosts != 1000, paste0("R$ ", format(mcosts, nsmall = 2)), "Não considerado"),
        "legend_title" = "  Empregos acessíveis (% do total)"
      ),
      "boxplot" = list(
        "facets_title" = ifelse(mcosts != 1000, paste0("R$ ", format(mcosts, nsmall = 2)), "Não considerado"),
        "palma_ratio" = "Razão de Palma: ",
        "y_axis" = "Empregos acessíveis (% do total)",
        "x_axis" = "Decil de renda"
      ),
      "theil" = list(
        "bar_labels" = ifelse(mcosts != 1000, paste0("R$ ", format(mcosts, nsmall = 2)), "Não considerado"),
        "y_axis" = "Índice de Theil",
        "x_axis" = "Valor limite de custo",
        "component" = "Componente",
        "components_names" = c("Entregrupos", "Intragrupos")
      ),
      "average_access" = list(
        "facets_title" = paste0("Custo ", ifelse(mcosts <= 100, paste0("<= ", mcosts, "% sal. mín."), "não consid.")),
        "y_axis" = "Acessibilidade média\n(% do total de empregos)",
        "x_axis" = "Decil de renda"
      ),
      "across_cost_palma" = list(
        "legend_title" = "Travel time\nthreshold (min.)",
        "y_axis" = "Palma Ratio",
        "x_axis" = "Monetary cost threshold (R$)",
        "x_labels" = c(
          "0",
          "<span style='font-size:8pt'>4.05</span>",
          "",
          "5",
          "<span style='font-size:8pt'>7.1</span>",
          "<span style='font-size:8pt'>8.75</span>",
          "10",
          "No limit"
        )
      ),
      "across_time_palma" = list(
        "legend_title" = "Monetary cost\nthreshold (R$)",
        "legend_values" = c(
          paste0("0.00 ", "<img src='data/icons/walking.png' width='10'>"),
          paste0("4.05 ","<img src='data/icons/bus.png' width='10'>+<img src='data/icons/brt.png' width='10'>"),
          paste0("4.70 ","<img src='data/icons/train.png' width='10'>"),
          paste0("5.00 ","<img src='data/icons/subway.png' width='10'>"),
          paste0("7.10 ","<img src='data/icons/brt.png' width='10'>+<img src='data/icons/subway.png' width='10'>"),
          paste0("8.75 ","(<img src='data/icons/bus.png' width='8'>+<img src='data/icons/brt.png' width='8'>)+<img src='data/icons/train.png' width='8'>"),
          "No cost limit"
        ),
        "y_axis" = "Palma Ratio",
        "x_axis" = "Travel time threshold (min.)"
      ),
      "across_cost_theil" = list(
        "legend_title" = "Tempo de\nviagem (min.)",
        "y_axis" = "Índice de Theil",
        "x_axis" = "Custo monetário (R$)",
        "x_labels" = c(
          "0",
          "<span style='font-size:8pt'>4.05</span>",
          "",
          "5",
          "<span style='font-size:8pt'>7.1</span>",
          "<span style='font-size:8pt'>8.75</span>",
          "10",
          "No limit"
        )
      ),
      "across_time_theil" = list(
        "legend_title" = "Custo\nmonetário",
        "y_axis" = "Índice de Theil",
        "x_axis" = "Tempo de viagem (min.)"
      ),
      "across_cost_comps" = list(
        "color_title" = "Tempo de\nviagem (min.)",
        "lntp_title" = "Componente",
        "y_axis" = "Índice de Theil",
        "x_axis" = "Custo monetário (R$)",
        "x_labels" = c(
          "0",
          "<span style='font-size:8pt'>4.05</span>",
          "",
          "5",
          "<span style='font-size:8pt'>7.1</span>",
          "<span style='font-size:8pt'>8.75</span>",
          "10",
          "No limit"
        ),
        "components" = list(Entregrupos = "between", Intragrupos = "within")
      ),
      "across_time_comps" = list(
        "color_title" = "Custo\nmonetário",
        "lntp_title" = "Componente",
        "y_axis" = "Índice de Theil",
        "x_axis" = "Tempo de viagem (min.)",
        "components" = list(Entregrupos = "between", Intragrupos = "within")
      ),
      "all_thresholds_maps" = list(
        "legend_title" = "Accessible jobs\n(% of total)",
        "times_facet_titles" = paste0(ttimes, " min."),
        "costs_facet_titles" = c(
          paste0("<img src='data/icons/walking.png' width='10'>", "<br>R$ 0.00"),
          paste0("<img src='data/icons/bus.png' width='10'>+<img src='data/icons/brt.png' width='10'>", "<br>R$ 4.05"),
          paste0("<img src='data/icons/train.png' width='10'>", "<br>R$ 4.70"),
          paste0("<img src='data/icons/subway.png' width='10'>", "<br>R$ 5.00"),
          paste0("<img src='data/icons/brt.png' width='10'>+<img src='data/icons/subway.png' width='10'>", "<br>R$ 7.10"),
          paste0("(<img src='data/icons/bus.png' width='8'>+<img src='data/icons/brt.png' width='8'>)+<img src='data/icons/train.png' width='8'>", "<br>R$ 8.75"),
          "No cost<br>limit"
        )
      )
    )
    
  }
  
  text_labels
  
}


# ANPET -------------------------------------------------------------------


distribution_map <- function(accessibility_data, 
                             tt, 
                             mcosts, 
                             text_labels, 
                             analysis_folder) {
  
  # read rio state and municipality shapes
  
  rj_state   <- readr::read_rds("./data/rj_state.rds")
  rio_border <- readr::read_rds("./data/rio_municipality.rds")
  
  # rapid_transit <- extract_rapid_transit("plotting")
  # lines       <- rapid_transit$lines
  # stations    <- rapid_transit$stations
  
  # convert min_wage_percent column to factor
  
  accessibility_data[, monetary_cost := factor(monetary_cost, 
                                               levels = mcosts, 
                                               labels = text_labels$distribution_map$facets_title)]
  
  # create sf objects

  expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(3000) %>% 
    st_transform(st_crs(rio_border))
  
  accessibility_data <- st_as_sf(accessibility_data[opportunities > 0 & population > 0])
  
  # plot settings
  
  xlim <- c(st_bbox(rio_border)[1], st_bbox(rio_border)[3])
  ylim <- c(st_bbox(expanded_rio_border)[2], st_bbox(rio_border)[4])
  
  max_accessibility <- max(accessibility_data$accessibility)
  total_opportunities <- sum(accessibility_data$opportunities) / length(mcosts)
  
  p <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(data = rio_border, color = "black", fill = NA, size = 0.3) +
    geom_sf(data = accessibility_data, aes(fill = accessibility), color = NA) +
    # geom_sf(data = lines, color = "black", alpha = 0.5) +
    # geom_sf(data = stations, color = "black", size = 1, alpha = 0.5) +
    facet_wrap(~ monetary_cost, ncol = 2) +
    ggsn::scalebar(
      data = rio_border, 
      dist = 10, 
      dist_unit = "km",
      location = "bottomright", 
      transform = TRUE, 
      model = "WGS84",
      border.size = 0.4, 
      st.dist = 0.05, 
      st.size = 3
    ) +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_viridis_c(
      name = text_labels$distribution_map$legend_title, 
      option = "inferno",
      breaks = seq(0, max_accessibility, max_accessibility / 3),
      labels = scales::label_percent(scale = 100 / total_opportunities)
    ) +
    guides(fill = guide_colorbar(title.vjust = 0.75)) +
    theme_thesis("map") +
    theme(
      legend.position = "bottom", 
      legend.box.just = "right",
      legend.box.spacing = unit(0 ,"points"),
      plot.margin = margin(b = 0)
    )
  
  # save plot
  
  ggsave(paste0(analysis_folder, "/distribution_map_tt_", tt, ".png"),
         width = 9,
         height = 5.8)
  
}


distribution_boxplot <- function(accessibility_data, 
                                 tt, 
                                 mcosts, 
                                 text_labels, 
                                 analysis_folder,
                                 n_quantiles) {
  
  # values to be used when setting the breaks and labels
  
  max_accessibility   <- max(accessibility_data$accessibility)
  total_opportunities <- sum(accessibility_data$opportunities) / length(mcosts)
  
  # filter out cells where population = 0 and, consequently, income_quantile = NA
  
  accessibility_data <- accessibility_data[population > 0]
  
  # calculate the palma ratio for each minimum wage percentage and create a 
  # dataframe to plot them as annotations
  
  ratios <- purrr::map_dbl(
    mcosts,
    function(i) palma_ratio(accessibility_data[monetary_cost == i])
  )
  
  palma_data <- data.frame(
    monetary_cost = factor(mcosts,
                           levels = mcosts,
                           labels = text_labels$boxplot$facets_title),
    x = 0.625,
    y = max(accessibility_data$accessibility),
    label = paste0(text_labels$boxplot$palma_ratio, format(round(ratios, digits = 4), nsmall = 4))
  )
  
  # convert monetary_cost column to factor
  
  accessibility_data[, monetary_cost := factor(monetary_cost, 
                                               levels = mcosts, 
                                               labels = text_labels$boxplot$facets_title)]
  
  # plot settings
  
  p <- ggplot(accessibility_data, aes(income_quantile, accessibility)) +
    geom_boxplot(aes(weight = population, group = income_quantile)) +
    facet_wrap(~ monetary_cost, ncol = 2) +
    labs(x = text_labels$boxplot$x_axis, y = text_labels$boxplot$y_axis) +
    geom_text(
      data = palma_data, 
      aes(x, y, label = label, hjust = "left"),
      size = 4.5, 
      color = "gray20", 
      vjust = 0.2
    ) +
    scale_x_continuous(breaks = 1:n_quantiles) +
    scale_y_continuous(
      limits = c(0, max_accessibility * 1.055),
      breaks = seq(0, max_accessibility, max_accessibility/3),
      labels = scales::percent_format(accuracy = 0.1, scale = 100 / total_opportunities)
    ) +
    theme_thesis("graphic")
  
  # save plot
  
  ggsave(paste0(analysis_folder, "/distribution_boxplot_tt_", tt, ".png"),
         plot = p,
         width = 8,
         height = 5,
         units = "in")
  
}


distribution_theil <- function(access_data, 
                               tt, 
                               mcosts, 
                               text_labels, 
                               analysis_folder) {
  
  access_data <- access_data[population > 0][, geometry := NULL]
  
  # calculate the theil index and its components in each case
  # set names for each observation and bind them together in the same dataframe
  
  access_data <- access_data[, .(data = list(.SD)), keyby = .(bilhete_unico, travel_time, monetary_cost)]
  
  theil_data <- setNames(
    lapply(access_data$data, function(i) theil_info(copy(i))),
    access_data$monetary_cost
  )
  
  theil_data <- rbindlist(theil_data, idcol = "monetary_cost")
  
  # aggregate component data and create a label_y column to place annotations 
  # within each stack
  
  theil_data <- theil_data[, .(share = sum(share)), by = .(monetary_cost, component)]
  theil_data <- theil_data[order(-monetary_cost, -component)]
  theil_data[, label_y := cumsum(share) - share / 2, by = monetary_cost]
  
  # convert component info and monetary cost to factors
  
  theil_data[, 
             `:=`(component = factor(component, levels = c("between", "within"), labels = text_labels$theil$components_names),
                  monetary_cost = factor(monetary_cost, levels = mcosts, labels = text_labels$theil$bar_labels))]
  
  # find max total theil to resize the plot's graphic area in order to fit the
  # annotation on top of bars
  
  max_theil_data <- theil_data[, .(theil = sum(share)), by = monetary_cost]
  max_theil      <- max(max_theil_data$theil)
  
  y_upper_limit <- purrr::map_dbl(
    max_theil + 0.05, 
    function(i, level = 1) round(i + 5 * 10 ^ (-level-1), level)
  )
  
  # plot settings
  
  p <- ggplot(theil_data) +
    geom_col(aes(monetary_cost, share, fill = component)) + 
    geom_text(
      aes(
        monetary_cost, 
        label_y, 
        label = format(round(share, digits = 4)), nsmall = 4
      ),
      color = "white"
    ) +
    stat_summary(
      fun = sum,
      aes(
        monetary_cost, 
        share, 
        label = format(round(..y.., digits = 4), nsmall = 4), 
        group = monetary_cost
      ),
      geom = "text",
      vjust = -0.5
    ) +
    labs(
      x = text_labels$theil$x_axis,
      y = text_labels$theil$y_axis,
      fill = text_labels$theil$component
    ) +
    coord_cartesian(ylim = c(0, y_upper_limit)) +
    theme_thesis("graphic") +
    theme(
      axis.text.x = element_text(angle = 22.5, hjust = 1),
      panel.grid = element_blank()
    )
  
  # save plot
  
  ggsave(paste0(analysis_folder, "/distribution_theil_tt_", tt, ".png"),
         plot = p,
         width = 7,
         height = 3,
         units = "in")
  
}


average_access_different_costs <- function(grid_data, 
                                           travel_time, 
                                           percentage_minimum_wage, 
                                           n_quantiles, 
                                           text_labels, 
                                           res) {
  
  # read and prepare data
  
  accessibility_data <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(paste0("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds"))) %>% 
    purrr::map(st_drop_geometry) %>% 
    purrr::map(function(i) left_join(i, grid_data, by = "id")) %>% 
    purrr::map(function(i) filter(i, population > 0))
  
  # bind each distribution to the same dataframe to plot it as a facet of the same plot
  # set names that can be used as the facet identifier and convert them to factors
  
  accessibility_data <- accessibility_data %>% 
    purrr::set_names(as.character(percentage_minimum_wage)) %>%
    bind_rows(.id = "percentage_minimum_wage") %>% 
    mutate(percentage_minimum_wage = factor(percentage_minimum_wage, unique(percentage_minimum_wage), labels = text_labels$average_access$facets_title))
  
  # calculate the average accessibility of the whole distribution and of each income quantile, weighted by the hexagons' population
  # also create columns in the quantiles dataframe to highlight the 9th decile in the plot
  
  distribution_mean <- accessibility_data %>% 
    group_by(percentage_minimum_wage) %>% 
    summarise(avg_accessibility = weighted.mean(accessibility, w = population), .groups = "drop")
  
  quantile_mean <- accessibility_data %>% 
    group_by(percentage_minimum_wage, income_quantile) %>% 
    summarise(avg_accessibility = weighted.mean(accessibility, w = population), .groups = "drop") %>%
    left_join(distribution_mean, by = "percentage_minimum_wage", suffix = c("", "_distribution")) %>% 
    mutate(
      highlight = ifelse(income_quantile == 9, TRUE, FALSE),
      ratio = ifelse(income_quantile == 9, avg_accessibility / avg_accessibility_distribution, NA),
      ratio = ifelse(!is.na(ratio), format(round(ratio, digits = 2), nsmall = 2), ratio),
      ratio = paste0(ratio, "x")
    )
  
  # plot settings
  
  max_accessibility <- max(quantile_mean$avg_accessibility)
  total_opportunities <- sum(grid_data$opportunities)
  
  upper_limit <- purrr::map_dbl(
    plyr::round_any(max_accessibility * 4/3, accuracy = 0.1 * total_opportunities),
    function(i) ifelse(i > total_opportunities, total_opportunities, i)
  )
  
  ggplot() +
    geom_segment(data = quantile_mean,
                 mapping = aes(x = income_quantile, xend = income_quantile,
                               y = avg_accessibility_distribution, yend = avg_accessibility,
                               color = highlight)) +
    geom_hline(data = distribution_mean, mapping = aes(yintercept = avg_accessibility),
               linetype = "dashed", color = "gray50") +
    geom_point(data = quantile_mean, mapping = aes(income_quantile, avg_accessibility, color = highlight),
               size = 2) +
    geom_text(data = quantile_mean, mapping = aes(income_quantile, avg_accessibility, label = ratio),
              na.rm = TRUE, vjust = -0.5) +
    geom_text(data = distribution_mean,
              mapping = aes(0.625, upper_limit, label = paste0("Média geral: ", scales::percent(avg_accessibility, scale = 100 / total_opportunities, accuracy = 1))),
              hjust = 0, vjust = 0.625, color = "gray20") +
    facet_wrap(~ percentage_minimum_wage, nrow = 1) +
    labs(x = text_labels$average_access$x_axis, y = text_labels$average_access$y_axis) +
    scale_color_manual(values = c("black", "dodgerblue3"), guide = FALSE) +
    scale_x_continuous(breaks = 1:n_quantiles) +
    scale_y_continuous(limits = c(0, upper_limit),
                       breaks = seq(0, upper_limit, upper_limit / 4),
                       labels = scales::percent_format(accuracy = 1, scale = 100 / total_opportunities)) +
    theme(strip.text.x = element_text(size = 13), strip.background.x = element_rect(fill = NA),
          axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 11),
          axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 11),
          panel.background = element_rect(fill = "gray94"),
          panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
  
  # save plot
  
  ggsave(paste0("./analysis/", text_labels$lang, "/different_costs/average_accessibility_tt_", travel_time, "_res_", res, ".png"),
         width = 9,
         height = 2.5,
         units = "in")
  
}


# WSTLUR and thesis -------------------------------------------------------


all_thresholds_maps <- function(access_data, 
                                ttimes, 
                                mcosts, 
                                bu,
                                max_width, 
                                dpi, 
                                dim_unit, 
                                text_labels, 
                                analysis_folder) {
  
  access_data <- access_data[
    (bilhete_unico == bu) & (travel_time %in% ttimes) & (monetary_cost %in% mcosts)
  ]
  
  # convert monetary_cost and travel_time columns to facets
  
  access_data[
    , 
    `:=`(
      monetary_cost = factor(
        monetary_cost, 
        levels = mcosts, 
        labels = text_labels$all_thresholds_maps$costs_facet_titles
      ),
      travel_time = factor(
        travel_time, 
        levels = ttimes, 
        labels = text_labels$all_thresholds_maps$times_facet_titles
      )
    )
  ]

  # read rio state and municipality shapes
  
  rj_state   <- readr::read_rds("./data/rj_state.rds")
  rio_border <- readr::read_rds("./data/rio_municipality.rds")

  # set facets' bounding box
  
  xlim <- c(st_bbox(rio_border)[1], st_bbox(rio_border)[3])
  ylim <- c(st_bbox(rio_border)[2], st_bbox(rio_border)[4])
  
  # calculate max accessibility and total opportunities for a pretty legend
  
  max_accessibility   <- max(access_data$accessibility)
  total_opportunities <- sum(access_data$opportunities) / (length(mcosts) * length(ttimes))
  
  # convert access_data to sf
  
  access_data <- st_as_sf(access_data[opportunities > 0 & population > 0])
  
  # plot settings
  
  p <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(data = access_data, aes(fill = accessibility), color = NA) +
    geom_sf(data = rio_border, color = "black", fill = NA, size = 0.3) +
    facet_grid(monetary_cost ~ travel_time, switch = "y") +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_viridis_c(
      name = text_labels$all_thresholds_maps$legend_title,
      option = "inferno",
      breaks = seq(0, max_accessibility, max_accessibility / 3),
      labels = scales::label_percent(scale = 100 / total_opportunities)
    ) +
    guides(fill = guide_colorbar(title.vjust = 0.75)) +
    theme_thesis("map")
  
  # save plot
  
  ggsave(
    paste0(analysis_folder, "/accessibility_distribution_", bu, ".png"),
    plot   = p,
    width  = max_width,
    height = max_width * 1.35,
    units  = dim_unit,
    dpi    = dpi
  )
  
}



couple_thresholds_maps <- function(access_data,
                                   ttimes,
                                   mcosts,
                                   bu,
                                   max_width,
                                   dpi,
                                   dim_unit,
                                   text_labels,
                                   analysis_folder) {
  
  access_data <- access_data[
    (bilhete_unico == bu) & (travel_time %in% ttimes) & (monetary_cost %in% mcosts)
  ]
  
  # calculate max accessibility and total opportunities for a pretty legend
  
  max_accessibility   <- max(access_data$accessibility)
  total_opportunities <- sum(access_data$opportunities) / (length(mcosts) * length(ttimes))
  
  # filter access_data to keep only two facets of 'all_thresholds_maps'
  
  access_data <- access_data[
    (bilhete_unico == bu) &
      (travel_time == max(ttimes)) &
      (monetary_cost %in% c(4.70, 5))
  ]
  access_data <- access_data[opportunities > 0 & population > 0]
  
  # read rio state and municipality shapes
  
  rj_state   <- readr::read_rds("./data/rj_state.rds")
  rio_border <- readr::read_rds("./data/rio_municipality.rds")
  
  expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(3000) %>% 
    st_transform(st_crs(rio_border))
  
  less_expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(1500) %>% 
    st_transform(st_crs(rio_border))
  
  # set bounding boxes
  
  xlim <- c(st_bbox(rio_border)[1], st_bbox(rio_border)[3])
  ylim <- c(st_bbox(expanded_rio_border)[2], st_bbox(rio_border)[4])
  
  # scalebar
  
  scalebar_data <- rio_border
  scalebar_data_bbox <- st_bbox(scalebar_data)
  scalebar_data_bbox["ymin"] <- st_bbox(less_expanded_rio_border)["ymin"]
  attr(st_geometry(scalebar_data), "bbox") <- scalebar_data_bbox
  
  scalebar <- ggsn::scalebar(
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
  
  # north
  
  north_data <- rio_border
  north_data_bbox <- st_bbox(north_data)
  north_data_bbox["xmin"] <- st_bbox(expanded_rio_border)["xmin"]
  attr(st_geometry(north_data), "bbox") <- north_data_bbox
  
  north <- ggsn::north(
    data = north_data,
    location = "topleft",
    scale = 0.15,
    symbol = 4
  )
  
  # plot settings
  
  ptop <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(
      data = st_as_sf(access_data[monetary_cost == 4.70]),
      aes(fill = accessibility),
      color = NA
    ) +
    geom_sf(data = rio_border, color = "black", fill = NA) +
    coord_sf(xlim, ylim) +
    scale_fill_viridis_c(
      name = text_labels$all_thresholds_maps$legend_title,
      option = "inferno",
      limits = c(0, max_accessibility),
      breaks = seq(0, max_accessibility, max_accessibility / 3),
      labels = scales::label_percent(scale = 100 / total_opportunities)
    ) +
    scalebar +
    north +
    guides(fill = guide_colorbar(title.vjust = 0.75)) +
    theme_thesis("map") +
    theme(legend.position = "none")
  
  pbot <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(
      data = st_as_sf(access_data[monetary_cost == 5]),
      aes(fill = accessibility),
      color = NA
    ) +
    geom_sf(data = rio_border, color = "black", fill = NA) +
    coord_sf(xlim, ylim) +
    scale_fill_viridis_c(
      name = text_labels$all_thresholds_maps$legend_title,
      option = "inferno",
      limits = c(0, max_accessibility),
      breaks = seq(0, max_accessibility, max_accessibility / 3),
      labels = scales::label_percent(scale = 100 / total_opportunities)
    ) +
    guides(fill = guide_colorbar(title.vjust = 0.75)) +
    theme_thesis("map") +
    theme(
      legend.position = c(0.99, 0),
      legend.direction = "horizontal",
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = "#aadaff"),
      legend.title = element_text(size = 10, hjust = 1),
      legend.text = element_text(size = 10)
    )
  
  # add labels
  
  ptop <- cowplot::ggdraw(ptop) +
    cowplot::draw_plot_label("A", fontface = "bold")
  
  pbot <- cowplot::ggdraw(pbot) +
    cowplot::draw_plot_label("B", fontface = "bold")
  
  # joined together
  
  pf <- cowplot::plot_grid(ptop, pbot, ncol = 1)
  
  # save plot
  
  ggsave(
    paste0(analysis_folder, "/selected_access_distribution_", bu, ".png"),
    plot   = pf,
    width  = max_width,
    height = max_width * 1.14,
    units  = dim_unit,
    dpi    = dpi
  )
  
}



one_threshold_map <- function(access_data,
                              ttimes,
                              mcosts,
                              bu,
                              max_width,
                              dpi,
                              dim_unit,
                              text_labels,
                              analysis_folder) {
  
  access_data <- access_data[
    (bilhete_unico == bu) & (travel_time %in% ttimes) & (monetary_cost %in% mcosts)
  ]
  
  # calculate max accessibility and total opportunities for a pretty legend
  
  max_accessibility   <- max(access_data$accessibility)
  total_opportunities <- sum(access_data$opportunities) / (length(mcosts) * length(ttimes))
  
  # filter access_data to keep one facet of 'all_thresholds_maps'
  
  access_data <- access_data[
    (bilhete_unico == bu) &
      (travel_time == max(ttimes)) &
      (monetary_cost == 4.05)
  ]
  access_data <- access_data[opportunities > 0 & population > 0]
  
  # read rio state and municipality shapes
  
  rj_state   <- readr::read_rds("./data/rj_state.rds")
  rio_border <- readr::read_rds("./data/rio_municipality.rds")
  
  expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(3000) %>% 
    st_transform(st_crs(rio_border))
  
  less_expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(1500) %>% 
    st_transform(st_crs(rio_border))
  
  # set bounding boxes
  
  xlim <- c(st_bbox(rio_border)[1], st_bbox(rio_border)[3])
  ylim <- c(st_bbox(expanded_rio_border)[2], st_bbox(rio_border)[4])
  
  # scalebar
  
  scalebar_data <- rio_border
  scalebar_data_bbox <- st_bbox(scalebar_data)
  scalebar_data_bbox["ymin"] <- st_bbox(less_expanded_rio_border)["ymin"]
  attr(st_geometry(scalebar_data), "bbox") <- scalebar_data_bbox
  
  scalebar <- ggsn::scalebar(
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
  
  # north
  
  north_data <- rio_border
  north_data_bbox <- st_bbox(north_data)
  north_data_bbox["xmin"] <- st_bbox(expanded_rio_border)["xmin"]
  attr(st_geometry(north_data), "bbox") <- north_data_bbox
  
  north <- ggsn::north(
    data = north_data,
    location = "topleft",
    scale = 0.15,
    symbol = 4
  )
  
  # plot settings
  
  p <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(
      data = st_as_sf(access_data),
      aes(fill = accessibility),
      color = NA
    ) +
    geom_sf(data = rio_border, color = "black", fill = NA) +
    coord_sf(xlim, ylim) +
    scale_fill_viridis_c(
      name = text_labels$all_thresholds_maps$legend_title,
      option = "inferno",
      limits = c(0, max_accessibility),
      breaks = seq(0, max_accessibility, max_accessibility / 3),
      labels = scales::label_percent(scale = 100 / total_opportunities)
    ) +
    scalebar +
    north +
    guides(fill = guide_colorbar(title.vjust = 0.75)) +
    theme_thesis("map") +
    theme(
      legend.position = c(0.99, 0),
      legend.direction = "horizontal",
      legend.justification = c(1, 0),
      legend.background = element_rect(fill = "#aadaff"),
      legend.title = element_text(size = 10, hjust = 1),
      legend.text = element_text(size = 10)
    )
  
  # save plot
  
  ggsave(
    paste0(analysis_folder, "/one_thresh_access_distribution_", bu, ".png"),
    plot   = p,
    width  = max_width,
    height = max_width * 0.57,
    units  = dim_unit,
    dpi    = dpi
  )
  
}



across_cost_palma <- function(access_data, 
                              text_labels, 
                              analysis_folder, 
                              bu, 
                              tt,
                              max_width, 
                              dpi, 
                              dim_unit) {
  
  # drop large unnecessary columns and filter data
  
  access_data[, geometry := NULL]
  access_data <- access_data[population > 0][monetary_cost != 1000][bilhete_unico == bu][travel_time %in% tt]
  
  # calculate the ratio in each case
  
  access_data <- access_data[, .(data = list(.SD)), keyby = .(bilhete_unico, travel_time, monetary_cost)]
  ratios      <- purrr::map_dbl(access_data$data, palma_ratio)
  access_data <- cbind(access_data, palma_ratio = ratios)[, data := NULL]
  
  # cast travel time to factor to create a discrete plot legend
  
  access_data[, travel_time := as.factor(travel_time)]
  
  # specify x axis breaks and labels
  
  breaks <- c(0, 4.05, 4.7, 5, 7.10, 8.75, 10, 15)
  labels <- text_labels$across_cost_palma$x_labels
  
  # plot settings
  
  p <- ggplot(access_data) + 
    geom_step(
      aes(monetary_cost, palma_ratio, group = travel_time, color = travel_time)
    ) +
    labs(
      x     = text_labels$across_cost_palma$x_axis, 
      y     = text_labels$across_cost_palma$y_axis,
      color = text_labels$across_cost_palma$legend_title
    ) +
    scale_color_brewer(palette = "Paired") +
    scale_x_continuous(
      breaks = breaks,
      labels = labels
    ) +
    theme_thesis("graphic")
  
  ggsave(
    paste0(analysis_folder, "/palma_across_costs_", bu, ".png"),
    plot   = p,
    width  = max_width,
    height = max_width * 0.5,
    units  = dim_unit,
    dpi    = dpi
  )
  
}



across_time_palma <- function(access_data, 
                              text_labels, 
                              analysis_folder, 
                              bu, 
                              mc,
                              max_width, 
                              dpi, 
                              dim_unit) {
  
  # drop large unnecessary columns and filter data
  
  access_data[, geometry := NULL]
  access_data <- access_data[population > 0][bilhete_unico == bu][monetary_cost %in% mc]
  
  # calculate the ratio in each case
  
  access_data <- access_data[, .(data = list(.SD)), keyby = .(bilhete_unico, travel_time, monetary_cost)]
  ratios      <- purrr::map_dbl(access_data$data, palma_ratio)
  access_data <- cbind(access_data, palma_ratio = ratios)[, data := NULL]
  
  # cast travel time to factor to create a discrete plot legend
  
  access_data[, monetary_cost := as.factor(monetary_cost)]
  
  levels(access_data$monetary_cost) <- text_labels$across_time_palma$legend_values
  
  # specify x axis breaks
  
  breaks <- c(0, 30, 60, 90, 120)
  
  # plot settings
  
  p <- ggplot(access_data) + 
    geom_step(
      aes(
        travel_time, 
        palma_ratio, 
        group = monetary_cost, 
        color = monetary_cost
      )
    ) +
    labs(
      x     = text_labels$across_time_palma$x_axis, 
      y     = text_labels$across_time_palma$y_axis,
      color = text_labels$across_time_palma$legend_title
    ) +
    scale_color_brewer(palette = "Paired") +
    scale_x_continuous(
      breaks = breaks
    ) +
    theme_thesis("graphic")
  
  ggsave(
    paste0(analysis_folder, "/palma_across_times_", bu, ".png"),
    plot   = p,
    width  = max_width,
    height = max_width * 0.5,
    units  = dim_unit,
    dpi    = dpi
  )
  
}



across_cost_theil <- function(access_data, 
                              text_labels, 
                              analysis_folder, 
                              bu, 
                              tt,
                              max_width, 
                              dpi, 
                              dim_unit) {
  
  # drop large unnecessary columns and filter data
  
  access_data[, `:=`(geometry = NULL, variable = accessibility, accessibility = NULL)]
  access_data <- access_data[population > 0][monetary_cost != 1000][bilhete_unico == bu][travel_time %in% tt]
  
  # calculate the ratio in each case
  
  access_data <- access_data[, .(data = list(.SD)), keyby = .(bilhete_unico, travel_time, monetary_cost)]
  theil       <- purrr::map_dbl(access_data$data, theil_index)
  access_data <- cbind(access_data, theil_index = theil)[, data := NULL]
  
  # cast travel time to factor to create a discrete plot legend
  
  access_data[, travel_time := as.factor(travel_time)]
  
  # specify x axis breaks and labels
  
  breaks <- c(0, 4.05, 4.7, 5, 7.10, 8.75, 10, 15)
  labels <- text_labels$across_cost_theil$x_labels
  
  # plot settings
  
  p <- ggplot(access_data) + 
    geom_step(
      aes(monetary_cost, theil_index, group = travel_time, color = travel_time)
    ) +
    labs(
      x     = text_labels$across_cost_theil$x_axis, 
      y     = text_labels$across_cost_theil$y_axis,
      color = text_labels$across_cost_theil$legend_title
    ) +
    scale_x_continuous(
      breaks = breaks,
      labels = labels
    ) +
    theme_thesis("graphic")
  
  ggsave(
    paste0(analysis_folder, "/theil_across_costs_", bu, ".png"),
    plot   = p,
    width  = max_width,
    height = max_width * 0.5,
    units  = dim_unit,
    dpi    = dpi
  )
  
}


across_time_theil <- function(access_data, 
                              text_labels, 
                              analysis_folder, 
                              bu, 
                              mc,
                              max_width, 
                              dpi, 
                              dim_unit) {
  
  # drop large unnecessary columns and filter data
  
  access_data[, `:=`(geometry = NULL, variable = accessibility, accessibility = NULL)]
  access_data <- access_data[population > 0][bilhete_unico == bu][monetary_cost %in% mc]
  
  # calculate the ratio in each case
  
  access_data <- access_data[, .(data = list(.SD)), keyby = .(bilhete_unico, travel_time, monetary_cost)]
  theil       <- purrr::map_dbl(access_data$data, theil_index)
  access_data <- cbind(access_data, theil_index = theil)[, data := NULL]
  
  # cast travel time to factor to create a discrete plot legend
  
  access_data[, monetary_cost := as.factor(monetary_cost)]
  
  levels(access_data$monetary_cost) <- ifelse(
    levels(access_data$monetary_cost) == "1000",
    "Sem restrição",
    paste0(
      "R$ ", 
      stringr::str_replace(
        format(as.numeric(levels(access_data$monetary_cost)), nsmall = 2), 
        " +", 
        ""
      )
    )
  )
  
  # specify x axis breaks
  
  breaks <- c(0, 30, 60, 90, 120)
  
  # plot settings
  
  p <- ggplot(access_data) + 
    geom_step(
      aes(
        travel_time, 
        theil_index, 
        group = monetary_cost, 
        color = monetary_cost
      )
    ) +
    labs(
      x     = text_labels$across_time_theil$x_axis, 
      y     = text_labels$across_time_theil$y_axis,
      color = text_labels$across_time_theil$legend_title
    ) +
    scale_x_continuous(
      breaks = breaks
    ) +
    theme_thesis("graphic")
  
  ggsave(
    paste0(analysis_folder, "/theil_across_times_", bu, ".png"),
    plot   = p,
    width  = max_width,
    height = max_width * 0.5,
    units  = dim_unit,
    dpi    = dpi
  )
  
}


across_cost_comps <- function(access_data, 
                              text_labels, 
                              analysis_folder, 
                              bu, 
                              tt,
                              max_width, 
                              dpi, 
                              dim_unit) {
  
  # drop large unnecessary columns and filter data
  
  access_data[, `:=`(geometry = NULL)]
  access_data <- access_data[population > 0][monetary_cost != 1000][bilhete_unico == bu][travel_time %in% tt]
  
  # calculate the ratio in each case
  
  access_data <- access_data[, .(data = list(.SD)), keyby = .(bilhete_unico, travel_time, monetary_cost)]
  components  <- lapply(
    access_data$data, 
    function(i) {
      theil <- theil_info(copy(i))[, .(share = sum(share)), keyby = .(component)]
      comps <- data.table(between = theil$share[1], within = theil$share[2])
    }
  )
  components  <- rbindlist(components)
  access_data <- cbind(access_data[, data := NULL], components)
  access_data <- data.table::melt(
    access_data,
    id = c("bilhete_unico", "travel_time", "monetary_cost"),
    measure = c("between", "within"),
    variable.name = "component",
    value.name = "share"
  )
  
  # cast travel time to factor to create a discrete plot legend
  
  access_data[, travel_time := as.factor(travel_time)]
  
  # cast components to factor to change labels
  
  access_data[, component := as.factor(component)]
  
  levels(access_data$component) <- text_labels$across_cost_comps$components
 
  # keeping only between-group component
  
  access_data <- access_data[component == "Entregrupos"]
  
  # specify x axis breaks and labels
  
  breaks <- c(0, 4.05, 4.7, 5, 7.10, 8.75, 10, 15)
  labels <- text_labels$across_cost_comps$x_labels
  
  # plot settings
  
  p <- ggplot(access_data) + 
    geom_step(
      aes(
        monetary_cost, 
        share, 
        group = interaction(component, travel_time), 
        color = travel_time
        #, linetype = component
      )
    ) +
    labs(
      x        = text_labels$across_cost_comps$x_axis, 
      y        = text_labels$across_cost_comps$y_axis,
      color    = text_labels$across_cost_comps$color_title
      #, linetype = text_labels$across_cost_comps$lntp_title
    ) +
    scale_x_continuous(
      breaks = breaks,
      labels = labels
    ) +
    theme_thesis("graphic")
  
  ggsave(
    paste0(analysis_folder, "/comps_across_costs_", bu, ".png"),
    plot   = p,
    width  = max_width,
    height = max_width * 0.5,
    units  = dim_unit,
    dpi    = dpi
  )

}


across_time_comps <- function(access_data, 
                              text_labels, 
                              analysis_folder, 
                              bu, 
                              mc,
                              max_width, 
                              dpi, 
                              dim_unit) {
  
  # drop large unnecessary columns and filter data
  
  access_data[, `:=`(geometry = NULL)]
  access_data <- access_data[population > 0][bilhete_unico == bu][monetary_cost %in% mc]
  
  # calculate the ratio in each case
  
  access_data <- access_data[, .(data = list(.SD)), keyby = .(bilhete_unico, travel_time, monetary_cost)]
  
  components <- lapply(
    access_data$data, 
    function(i) {
      theil <- theil_info(copy(i))[, .(share = sum(share)), keyby = .(component)]
      comps <- data.table(between = theil$share[1], within = theil$share[2])
    }
  )
  
  components  <- rbindlist(components)
  access_data <- cbind(access_data[, data := NULL], components)
  access_data <- data.table::melt(
    access_data,
    id = c("bilhete_unico", "travel_time", "monetary_cost"),
    measure = c("between", "within"),
    variable.name = "component",
    value.name = "share"
  )
  
  # cast travel time to factor to create a discrete plot legend
  
  access_data[, monetary_cost := as.factor(monetary_cost)]
  
  levels(access_data$monetary_cost) <- ifelse(
    levels(access_data$monetary_cost) == "1000",
    "Sem restrição",
    paste0(
      "R$ ", 
      stringr::str_replace(
        format(as.numeric(levels(access_data$monetary_cost)), nsmall = 2), 
        " +", 
        ""
      )
    )
  )
  
  # cast components to factor to change labels
  
  access_data[, component := as.factor(component)]
  
  levels(access_data$component) <- text_labels$across_cost_comps$components
  
  # keeping only between-group component
  
  access_data <- access_data[component == "Entregrupos"]
  
  # specify x axis breaks
  
  breaks <- c(0, 30, 60, 90, 120)
  
  # plot settings
  
  p <- ggplot(access_data) + 
    geom_step(
      aes(
        travel_time, 
        share, 
        group = interaction(component, monetary_cost), 
        color = monetary_cost
        #, linetype = component
      )
    ) +
    labs(
      x        = text_labels$across_time_comps$x_axis, 
      y        = text_labels$across_time_comps$y_axis,
      color    = text_labels$across_time_comps$color_title
      #, linetype = text_labels$across_time_comps$lntp_title
    ) +
    scale_x_continuous(
      breaks = breaks
    )  +
    theme_thesis("graphic")
  
  ggsave(
    paste0(analysis_folder, "/comps_across_times_", bu, ".png"),
    plot   = p,
    width  = max_width,
    height = max_width * 0.5,
    units  = dim_unit,
    dpi    = dpi
  )
  
}



# support functions -------------------------------------------------------



palma_ratio <- function(data, variable = "accessibility") {
  
  richest_10 <- data[income_quantile == 10,
                     .(avg_variable = weighted.mean(get(variable), population))]
  
  poorest_40 <- data[income_quantile %in% 1:4,
                     .(avg_variable = weighted.mean(get(variable), population))]
  
  palma_ratio <- richest_10$avg_variable / poorest_40$avg_variable
  
  return(palma_ratio)
  
}

theil_info <- function(access_data, variable = "accessibility") {
  
  unique_quantiles <- unique(access_data$income_quantile)
  unique_quantiles <- unique_quantiles[! is.na(unique_quantiles)]
  unique_quantiles <- unique_quantiles[order(unique_quantiles)]
  n <- length(unique_quantiles)
  
  # change the desired variable column name to "variable" in order to allow calculations for any variable
  
  access_data[, variable := get(..variable)]
  access_data[, total_variable := variable * population]
  
  # calculate the between-group component
  # group by quantile, calculate its share of the component and save it in a list
  
  between_group_data <- access_data[variable > 0, 
                                    .(population = sum(population), 
                                      total_variable = sum(total_variable)), 
                                    keyby = income_quantile]

  between_group_data[, theil_share := (total_variable / sum(total_variable)) * log((total_variable / sum(total_variable)) / (population / sum(population)))]
  
  between_group <- data.table(
    component = rep("between", n), 
    income_quantile = unique_quantiles, 
    share = between_group_data$theil_share
  )
  
  # calculate the within-group component
  # it is the weighted average of each group's own theil index, where accessibility is the weight
  
  within_group_share <- purrr::map_dbl(
    unique_quantiles, 
    function(i) {
      filtered_data <- copy(access_data)[income_quantile == i]
      share <- theil_index(filtered_data) * sum(filtered_data$total_variable) / sum(access_data$total_variable)
    }
  )
  
  within_group <- data.table(
    component = rep("within", n), 
    income_quantile = unique_quantiles, 
    share = within_group_share
  )
  
  # bind between- and within-group components lists together in a dataframe
  
  info <- rbindlist(list(within_group, between_group))
  
  info
  
}

theil_index <- function(access_data) {
  
  # variable > 0 prevents log(0) which is NaN
  
  access_data <- access_data %>% 
    filter(variable > 0) %>% 
    mutate(
      total_variable = variable * population,
      theil_share = (total_variable / sum(total_variable)) * log((total_variable / sum(total_variable)) / (population / sum(population)))
    )
  
  index <- sum(access_data$theil_share)
  
  index
  
}