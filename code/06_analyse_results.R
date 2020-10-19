library(dplyr)
library(sf)
library(tmap)
library(ggplot2)
library(data.table)

analyse_results <- function(grid_name = "grid_with_data",
                            router = "rio",
                            n_quantiles = 10,
                            res = 7,
                            lang = "pt") {
  
  # * read and prepare data -------------------------------------------------
  
  
  router_folder <- paste0("./data/", router, "_res_", res)
  
  # read accessibility for each departure time and use their average in the
  # following analysis
  
  accessibility_folder <- paste0(router_folder, "/accessibility")
  
  accessibility_data <- rbindlist(
    lapply(
      paste0(accessibility_folder, "/", list.files(accessibility_folder)),
      function(i) fread(i)
    )
  )[, .(accessibility = mean(accessibility)), keyby = .(id, bilhete_unico, travel_time, min_wage_percent)]
  
  # read grid_data and calculate distance to closest rail/subway/brt station and 
  # to cbd
  
  grid_data <- readr::read_rds(paste0(router_folder, "/", grid_name, ".rds"))
  
  stations <- extract_rapid_transit(router)
  
  rail   <- stations[stations$mode == "rail", ]
  subway <- stations[stations$mode == "subway", ]
  brt    <- stations[stations$mode == "brt", ]
  cbd    <- grid_data[grid_data$opportunities == max(grid_data$opportunities), ]
  
  dist_rail   <- st_distance(grid_data, rail) %>% matrixStats::rowMins()
  dist_subway <- st_distance(grid_data, subway) %>% matrixStats::rowMins()
  dist_brt    <- st_distance(grid_data, brt) %>% matrixStats::rowMins()
  dist_cbd    <- st_distance(grid_data, cbd) %>% matrixStats::rowMins()
  
  grid_data <- setDT(grid_data)[, avg_income := total_income / population]
  grid_data <- cbind(grid_data, dist_rail, dist_subway, dist_brt, dist_cbd)
  
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
  
  
  # * different costs analysis ----------------------------------------------

  
  analysis_folder <- paste0(router_folder, "/analysis")
  if (!file.exists(analysis_folder)) dir.create(analysis_folder)
  
  analysis_folder <- paste0(analysis_folder, "/", lang)
  if (!file.exists(analysis_folder)) dir.create(analysis_folder)
  
  ttimes <- c(30, 60, 90, 120)
  mwpcts <- c(0.2, 0.3, 0.4, 10)
  
  oi <- purrr::map(ttimes, function(tt) {
    
    analysis_folder <- paste0(analysis_folder, "/different_costs")
    if (!file.exists(analysis_folder)) dir.create(analysis_folder)
    
    filtered_data <- accessibility_data[(bilhete_unico == "with") & 
                                        (travel_time == tt) & 
                                        (min_wage_percent %in% mwpcts)]
    
    text_labels <- labels_different_costs(tt, mwpcts, lang)
    
    maps_different_costs(copy(filtered_data), tt, mwpcts, text_labels, analysis_folder)
    maps_reduction_different_costs(copy(filtered_data), tt, mwpcts, text_labels, analysis_folder)
    # hist_reduction_different_costs(copy(filtered_data), tt, mwpcts, text_labels, analysis_folder)
    boxplot_different_costs(copy(filtered_data), tt, mwpcts, text_labels, analysis_folder, n_quantiles)
    # theil_different_costs(grid_data, travel_time[i], percentage_minimum_wage, text_labels, res)
    # average_access_different_costs(grid_data, travel_time[i], percentage_minimum_wage, n_quantiles, text_labels, res)
    
  })
  
  # second analysis: effect of BU fare policy
  # create visualisations comparing accessibility at a specific combination of time travel and cost thresholds
  
  percentage_minimum_wage <- c(20, 30, 40)
  
  for (i in seq_along(ttimes)) {
    
    #text_labels <- labels_cases_bu(travel_time[i], percentage_minimum_wage, lang)
   
    # maps_cases_bu(grid_data, travel_time[i], percentage_minimum_wage, text_labels, res)
    # maps_increase_cases_bu(grid_data, travel_time[i], percentage_minimum_wage, text_labels, res)
    # boxplot_increase_cases_bu(grid_data, travel_time[i], percentage_minimum_wage, text_labels, res)
    # theil_cases_bu(grid_data, travel_time[i], percentage_minimum_wage, text_labels, res)
     
  }
  
}


# different monetary cost thresholds --------------------------------------


labels_different_costs <- function(t, mwpcts, lang) {
  
  # create the text labels used in the maps and graphics according to the specified language
  
  mwpcts <- mwpcts * 100
  
  if (lang == "pt") {
    
    text_labels <- list(
      "lang" = lang,
      "maps" = list(
        "facets_title" = paste0("Custo monetário ", ifelse(mwpcts <= 100, paste0("<= ", mwpcts, "% do salário mínimo"), "não considerado")),
        "legend_title" = "  Empregos acessíveis (% do total)"
      ),
      "reduction_maps" = list(
        "facets_title" = paste0("Custo monetário ", ifelse(mwpcts <= 100, paste0("<= ", mwpcts, "% do salário mínimo"), "não considerado")),
        "percent_reduction_legend_title" = "Redução (% da acess.\nsem limite de dinheiro)",
        "total_reduction_legend_title" = "Redução (% do total\nde empregos)",
        "mode_legend_title" = "Modo",
        "mode_options" = c("BRT", "Metrô e trem")
      ),
      "boxplot" = list(
        "facets_title" = paste0("Custo monetário ", ifelse(mwpcts <= 100, paste0("<= ", mwpcts, "% do salário mínimo"), "não considerado")),
        "palma_ratio" = "Razão de Palma: ",
        "y_axis" = "Empregos acessíveis (% do total)",
        "x_axis" = "Decil de renda"
      ),
      "theil" = list(
        "bar_labels" = ifelse(mwpcts <= 100, paste0(mwpcts, "% do sal. mín."), "Não considerado"),
        "y_axis" = "Índice de Theil",
        "x_axis" = "Valor limite de custo",
        "component" = "Componente",
        "components_names" = c("Entregrupos", "Intragrupos")
      ),
      "average_access" = list(
        "facets_title" = paste0("Custo ", ifelse(mwpcts <= 100, paste0("<= ", mwpcts, "% sal. mín."), "não consid.")),
        "y_axis" = "Acessibilidade média\n(% do total de empregos)",
        "x_axis" = "Decil de renda"
      )
    )
    
    
  }
  
  text_labels
  
}


maps_different_costs <- function(accessibility_data, 
                                 tt, 
                                 mwpcts, 
                                 text_labels, 
                                 analysis_folder) {
  
  # read rio state and municipality shapes
  
  rj_state   <- readr::read_rds("./data/rj_state.rds")
  rio_border <- readr::read_rds("./data/rio_municipality.rds")
  
  # rapid_transit <- extract_rapid_transit("plotting")
  # lines       <- rapid_transit$lines
  # stations    <- rapid_transit$stations
  
  # convert min_wage_percent column to factor
  
  accessibility_data[, min_wage_percent := factor(min_wage_percent, 
                                                 levels = mwpcts, 
                                                 labels = text_labels$maps$facets_title)]
  
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
  total_opportunities <- sum(accessibility_data$opportunities) / length(mwpcts)
  
  p <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(data = rio_border, color = "black", fill = NA, size = 0.3) +
    geom_sf(data = accessibility_data, aes(fill = accessibility), color = NA) +
    # geom_sf(data = lines, color = "black", alpha = 0.5) +
    # geom_sf(data = stations, color = "black", size = 1, alpha = 0.5) +
    facet_wrap(~ min_wage_percent, nrow = 2) +
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
      name = text_labels$maps$legend_title, 
      option = "inferno",
      breaks = seq(0, max_accessibility, max_accessibility / 3),
      labels = scales::label_percent(scale = 100 / total_opportunities)
    ) +
    guides(fill = guide_colorbar(title.vjust = 0.75)) +
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(),
      axis.ticks = element_blank(), 
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#aadaff"),
      legend.position = "bottom", 
      legend.box.just = "right",
      legend.box.spacing = unit(0 ,"points"),
      strip.text.x = element_text(size = 13), 
      strip.text.y = element_text(size = 13),
      strip.background = element_rect(fill = NA),
      plot.margin = margin(b = 0)
    )
  
  # save plot
  
  ggsave(paste0(analysis_folder, "/comparative_map_tt_", tt, ".png"),
         width = 9,
         height = 5.8)
  
}


maps_reduction_different_costs <- function(accessibility_data, 
                                           tt, 
                                           mwpcts, 
                                           text_labels, 
                                           analysis_folder) {
 
  # read rio state and municipality shapes
  
  rj_state   <- readr::read_rds("./data/rj_state.rds")
  rio_border <- readr::read_rds("./data/rio_municipality.rds")
  
  # rapid_transit_info <- extract_rapid_transit("plotting") %>% 
  #   purrr::map(function(i) mutate(i, Mode = factor(Mode, levels = unique(Mode), 
  #                                 labels = text_labels$reduction_maps$mode_options)))
  
  # calculate the accessibility difference between the cases with and the one
  # without a cost threshold. then filter out the case without a cost threshold
  
  comparison_case <- accessibility_data[min_wage_percent == 10]
  
  accessibility_data <- accessibility_data[min_wage_percent < 10
   ][comparison_case,  on = "id", nc_accessibility := i.accessibility
     ][, `:=`(total_reduction = nc_accessibility - accessibility,
              percent_reduction = (nc_accessibility - accessibility) / nc_accessibility)]
  
  # convert min_wage_percent column to factor
  
  accessibility_data[, min_wage_percent := factor(min_wage_percent, 
                                                  levels = mwpcts, 
                                                  labels = text_labels$reduction_maps$facets_title)]
  
  # create sf objects
  
  expanded_rio_border <- rio_border %>% 
    st_transform(5880) %>% 
    st_buffer(3000) %>% 
    st_transform(st_crs(rio_border))
  
  accessibility_data <- st_as_sf(accessibility_data[opportunities > 0 & population > 0])
  
  # create function with plot settings

  plot_reduction <- function(type = c("percent", "total")) {
    
    # use different denominator for accessibility legend labels depending on 
    # the type of reduction
    
    if (type == "percent") denominator <- 1
    else denominator <- sum(accessibility_data$opportunities) / (length(mwpcts) - 1)
    
    # use following objects to conditionally access objects depending on type
    
    reduction_type <- paste0(type, "_reduction")
    access_legend_title <- paste0(reduction_type, "_legend_title")
    
    # plot settings
    
    xlim <- c(st_bbox(rio_border)[1], st_bbox(rio_border)[3])
    ylim <- c(st_bbox(expanded_rio_border)[2], st_bbox(rio_border)[4])
    
    p <- ggplot() +
      geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
      geom_sf(
        data = accessibility_data, 
        aes_string(fill = reduction_type), 
        color = NA
      ) +
      geom_sf(data = rio_border, color = "gray50", fill = NA, size = 0.3) +
      facet_wrap(~ min_wage_percent, nrow = 2) +
      # geom_sf(
      #   data = rapid_transit_info[["lines"]], 
      #   aes(shape = Mode),
      #   color = "gray30", 
      #   show.legend = "line"
      # ) +
      # geom_sf(
      #   data = rapid_transit_info[["stations"]], 
      #   aes(shape = Mode),
      #   color = "gray30", 
      #   show.legend = "point"
      # ) +
      ggsn::scalebar(
        data = rio_border, 
        dist = 10, 
        dist_unit = "km",
        location = "bottomright", 
        transform = TRUE, 
        model = "WGS84",
        height = 0.03, 
        border.size = 0.4, 
        st.dist = 0.05, 
        st.size = 3
      ) +
      coord_sf(xlim = xlim, ylim = ylim) +
      # scale_color_manual(
      #   name = text_labels$reduction_maps$mode_legend_title,
      #   values = c("royalblue3", "gray30")
      # ) +
      scale_fill_gradient(
        name = text_labels$reduction_maps[[access_legend_title]],
        low = "#efeeec", 
        high = "red",
        labels = scales::label_percent(scale = 100 / denominator)
      ) +
      guides(
        shape = guide_legend(order = 1), 
        fill = guide_colorbar(order = 2)
      ) +
      theme(
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#aadaff"),
        legend.position = "bottom", 
        legend.box = "vertical",
        legend.box.just = "left", 
        strip.text.x = element_text(size = 13),
        strip.background = element_rect(fill = NA)
      )
    
    p <- lemon::reposition_legend(
      p,
      position = "bottom left",
      panel = "panel-2-2",
      plot = FALSE
    )
    
    # save plot
    
    ggsave(paste0(analysis_folder, "/", reduction_type, "_map_tt_", tt, ".png"),
           plot = p,
           width = 9,
           height = 5.8)
    
  }
  
  plot_reduction("percent")
  plot_reduction("total")
  
}


hist_reduction_different_costs <- function(accessibility_data, 
                                           tt, 
                                           mwpcts, 
                                           text_labels, 
                                           analysis_folder) {
  
  # calculate the accessibility difference between the cases with and the one
  # without a cost threshold. then filter out the case without a cost threshold
  
  comparison_case <- accessibility_data[min_wage_percent == 10]
  
  accessibility_data <- accessibility_data[min_wage_percent < 10 & (population > 0 | opportunities > 0)
                                           ][comparison_case,  on = "id", nc_accessibility := i.accessibility
                                             ][, `:=`(total_reduction = nc_accessibility - accessibility,
                                                      percent_reduction = (nc_accessibility - accessibility) / nc_accessibility)]
  
  # convert min_wage_percent column to factor
  
  accessibility_data[, min_wage_percent := factor(min_wage_percent, 
                                                  levels = mwpcts, 
                                                  labels = text_labels$reduction_maps$facets_title)]
  
  # plot settings
  
  p <- ggplot(accessibility_data, aes(x = percent_reduction)) +
    geom_histogram(binwidth = 0.01, boundary = 0) +
    facet_wrap(~ min_wage_percent, nrow = 2)
  
  print(p)
  
}


boxplot_different_costs <- function(accessibility_data, 
                                    tt, 
                                    mwpcts, 
                                    text_labels, 
                                    analysis_folder,
                                    n_quantiles) {
  
  # values to be used when setting the breaks and labels
  
  max_accessibility   <- max(accessibility_data$accessibility)
  total_opportunities <- sum(accessibility_data$opportunities) / length(mwpcts)
  
  # filter out cells where population = 0 and, consequently, income_quantile = NA
  
  accessibility_data <- accessibility_data[population > 0]
  
  # calculate the palma ratio for each minimum wage percentage and create a 
  # dataframe to plot them as annotations
  
  ratios <- purrr::map_dbl(
    mwpcts,
    function(i) palma_ratio(accessibility_data[min_wage_percent == i])
  )
  
  palma_data <- data.frame(
    min_wage_percent = factor(mwpcts,
                              levels = mwpcts,
                              labels = text_labels$boxplot$facets_title),
    x = 0.625,
    y = max(accessibility_data$accessibility),
    label = paste0(text_labels$boxplot$palma_ratio, format(round(ratios, digits = 4), nsmall = 4))
  )
  
  # convert min_wage_percent column to factor
  
  accessibility_data[, min_wage_percent := factor(min_wage_percent, 
                                                  levels = mwpcts, 
                                                  labels = text_labels$boxplot$facets_title)]
  
  # plot settings
  
  p <- ggplot(accessibility_data, aes(income_quantile, accessibility)) +
    geom_boxplot(aes(weight = population, group = income_quantile)) +
    facet_wrap(~ min_wage_percent, ncol = 2) +
    labs(x = text_labels$boxplot$x_axis, y = text_labels$boxplot$y_axis) +
    geom_text(
      data = palma_data, 
      aes(x, y, label = label, hjust = "left"),
      size = 4.5, 
      color = "gray20", 
      vjust = -0.3
    ) +
    scale_x_continuous(breaks = 1:n_quantiles) +
    scale_y_continuous(
      limits = c(0, max_accessibility * 1.055),
      breaks = seq(0, max_accessibility, max_accessibility/3),
      labels = scales::percent_format(accuracy = 0.1, scale = 100 / total_opportunities)
    ) +
    theme(
      strip.text.x = element_text(size = 13),
      strip.background.x = element_rect(fill = NA),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "gray94")
    )
  
  # save plot
  
  ggsave(paste0(analysis_folder, "/boxplot_tt_", tt, ".png"),
         plot = p,
         width = 8,
         height = 5,
         units = "in")
  
}

theil_different_costs <- function(grid_data, travel_time, percentage_minimum_wage, text_labels, res) {
  
  # read and prepare data
  
  accessibility_data <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(paste0("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds"))) %>%
    purrr::map(st_drop_geometry) %>% 
    purrr::map(function(i) left_join(i, grid_data, by = "id")) %>% 
    purrr::map(function(i) filter(i, population > 0))
  
  # calculate the theil index and its components in each case
  # set names for each observation and bind them together in the same dataframe
  
  theil_data <- accessibility_data %>% 
    purrr::map(theil_info) %>%
    purrr::set_names(percentage_minimum_wage) %>%
    bind_rows(.id = "percentage_minimum_wage") %>% 
    mutate(percentage_minimum_wage = as.integer(percentage_minimum_wage))
  
  # aggregate component data and create a label_y column to place annotations within each stack
  
  theil_data <- theil_data %>% 
    group_by(percentage_minimum_wage, component) %>% 
    summarise(share = sum(share), .groups = "drop_last") %>% 
    arrange(desc(percentage_minimum_wage), desc(component)) %>% 
    mutate(label_y = cumsum(share) - share/2) %>% 
    ungroup()
  
  # convert component info and minimum wage percentages to factors to adequately order and stack the bars2
  
  theil_data <- theil_data %>% 
    mutate(
      component = factor(component, levels = c("between", "within"), labels = text_labels$theil$components_names),
      percentage_minimum_wage = factor(percentage_minimum_wage, levels = unique(percentage_minimum_wage), labels = text_labels$theil$bar_labels)
    )
  
  # find max total theil to resize the plot's graphic area in order to fit the annotation on top of bars
  
  max_theil_data <- theil_data %>% 
    group_by(percentage_minimum_wage) %>% 
    summarise(theil = sum(share), .groups = "drop")
  
  max_theil <- max(max_theil_data$theil)
  
  y_upper_limit <- purrr::map_dbl(max_theil + 0.05, function(i, level = 1) round(i + 5*10^(-level-1), level))
  
  # plot settings
  
  ggplot(theil_data) +
    geom_col(aes(percentage_minimum_wage, share, fill = component)) + 
    geom_text(aes(percentage_minimum_wage, label_y, label = format(round(share, digits = 4)), nsmall = 4),
              color = "white") +
    stat_summary(fun = sum,
                 aes(percentage_minimum_wage, share, label = format(round(..y.., digits = 4), nsmall = 4), group = percentage_minimum_wage),
                 geom = "text",
                 vjust = -0.5) +
    labs(x = text_labels$theil$x_axis, y = text_labels$theil$y_axis, fill = text_labels$theil$component) +
    coord_cartesian(ylim = c(0, y_upper_limit)) +
    theme(
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(size = 11, angle = 22.5, hjust = 1),
      axis.title.y = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "gray94")
    )
  
  # save plot
  
  ggsave(paste0("./analysis/", text_labels$lang, "/different_costs/theil_tt_", travel_time, "_res_", res, ".png"),
         width = 7,
         height = 3,
         units = "in")
  
}

average_access_different_costs <- function(grid_data, travel_time, percentage_minimum_wage, n_quantiles, text_labels, res) {
  
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


###########################
# DIFFERENT FARE POLICIES #
###########################


labels_cases_bu <- function(travel_time, percentage_minimum_wage, lang) {
  
  # create the text labels used in the maps and graphics according to the specified language
  
  if (lang == "pt") {
    
    text_labels <- list(
      "lang" = lang,
      "maps" = list(
        "cost_title" = paste0("Custo <= ", percentage_minimum_wage, "% do sal. mín."),
        "fare_title" = c("Sem Bilhete Único", "Com Bilhete Único"),
        "legend_title" = "Empregos acessíveis (% do total)"
      ),
      "increase_maps" = list(
        "facets_title" = paste0("Custo monetário <= ", percentage_minimum_wage, "% do salário mínimo"),
        "legend_title" = "Aumento de acessibilidade\n(% do total de empregos)"
        # "mode_legend_title" = "Modo",
        # "mode_options" = c("BRT", "Metrô e trem")
      ),
      "boxplot_difference" = list(
        "facets_title" = paste0("Custo <= ", percentage_minimum_wage, "% do sal. mín."),
        "palma_ratio" = "Razão de Palma: ",
        "y_axis" = "Aumento de acess. (% total de empregos)",
        "x_axis" = "Decil de renda"
      ),
      "theil" = list(
        "bar_labels" = paste0(percentage_minimum_wage, "% do sal. mín."),
        "y_axis" = "Índice de Theil",
        "x_axis" = "Valor limite de custo",
        "component" = "Componente",
        "components_names" = c("Entregrupos", "Intragrupos")
      ),
      "between_group" = list(
        "facets_title" = paste0("Custo monetário ", ifelse(percentage_minimum_wage <= 100, paste0("<= ", percentage_minimum_wage, "% do salário mínimo"), "não considerado")),
        "y_axis" = "Participação",
        "x_axis" = "Decil de renda"
      )
    )
    
    
  }
  
  text_labels
  
}

maps_cases_bu <- function(grid_data, travel_time, percentage_minimum_wage, text_labels, res) {
  
  # create objects used to read the desired files
  
  without_with <- rep(c("without", "with"), length(percentage_minimum_wage))
  percentage_minimum_wage <- rep(percentage_minimum_wage, 2)
  
  # read data (the mutate before the bind_rows is due to a strange behaviour with bind_rows when .id is not NULL and sf objects...
  # without the random mutate first the function won't work)
  
  rj_state <- readr::read_rds("./data/rj_state.rds")
  
  accessibility_data <- purrr::map2(without_with, percentage_minimum_wage, function(i, j) readr::read_rds(paste0("./results/", i, "_bu_tt_", travel_time, "_mc_", j, "_res_", res, ".rds"))) %>% 
    purrr::map(function(i) mutate(i, bilhete_unico = "temp")) %>% 
    bind_rows(.id = "case") %>% 
    mutate(
      case = as.numeric(case),
      bilhete_unico = without_with[case],
      percentage_minimum_wage = percentage_minimum_wage[case]
    )
  
  # convert bilhete_unico and percentage_minimum_wage columns to factors to order the facets
  
  accessibility_data <- accessibility_data %>% 
    mutate(
      bilhete_unico = factor(bilhete_unico, levels = unique(without_with), labels = text_labels$maps$fare_title),
      percentage_minimum_wage = factor(percentage_minimum_wage, levels = unique(percentage_minimum_wage), labels = text_labels$maps$cost_title)
    )
  
  # plot settings
  
  rio_border <- accessibility_data %>% group_by(id) %>% slice(1) %>% ungroup() %>% summarise()
  expanded_rio_border <- rio_border %>% st_transform(5880) %>% st_buffer(3000) %>% st_transform(st_crs(rio_border))
  
  xlim <- c(st_bbox(rio_border)[1], st_bbox(rio_border)[3])
  ylim <- c(st_bbox(expanded_rio_border)[2], st_bbox(rio_border)[4])
  
  max_accessibility <- max(accessibility_data$accessibility)
  total_opportunities <- sum(grid_data$opportunities)
  
  p <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(data = accessibility_data, aes(fill = accessibility), color = NA) +
      facet_grid(percentage_minimum_wage ~ bilhete_unico, switch = "y") +
    geom_sf(data = rio_border, color = "black", fill = NA) +
    ggsn::scalebar(data = rio_border, dist = 10, dist_unit = "km", location = "bottomright", transform = TRUE, model = "WGS84",
                   height = 0.03, border.size = 0.4, st.dist = 0.05, st.size = 3) +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_viridis_c(name = text_labels$maps$legend_title, option = "inferno", breaks = seq(0, max_accessibility, max_accessibility / 3),
                         labels = scales::label_percent(scale = 100 / total_opportunities)) +
    guides(fill = guide_colorbar(title.vjust = 0.75)) +
    theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid = element_blank(), panel.background = element_rect(fill = "#aadaff"),
          legend.position = "bottom", legend.box.just = "right", legend.box.spacing = unit(0 ,"points"),
          strip.text.x = element_text(size = 13), strip.text.y = element_text(size = 13), strip.background = element_rect(fill = NA),
          plot.margin = margin(b = 0))

  # save plot
  
  ggsave(paste0("./analysis/", text_labels$lang, "/cases_bu/comparative_map_tt_", travel_time, "_res_", res, ".png"),
         width = 9,
         height = 8.3)
}

maps_increase_cases_bu <- function(grid_data, travel_time, percentage_minimum_wage, text_labels, res) {
  
  # read data
  
  rj_state <- readr::read_rds("./data/rj_state.rds")
  
  accessibility_with_bu <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(paste0("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds")))
  accessibility_without_bu <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(paste0("./results/without_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds")))
  
  # calculate the accessibility difference between the cases with and without bilhete único
  # then bind everything in the same dataframe with adequate names, later converted to factors
  
  accessibility_data <- purrr::map(seq_along(percentage_minimum_wage), function(i) left_join(accessibility_with_bu[[i]], st_drop_geometry(accessibility_without_bu[[i]]), by = "id")) %>%
    purrr::map(function(i) mutate(i, increase = accessibility.x - accessibility.y)) %>% 
    purrr::set_names(as.character(percentage_minimum_wage)) %>% 
    bind_rows(.id = "percentage_minimum_wage") %>% 
    mutate(percentage_minimum_wage = factor(percentage_minimum_wage, levels = unique(percentage_minimum_wage), labels = text_labels$increase_maps$facets_title))
  
  # plot settings
  
  rio_border <- accessibility_data %>% group_by(id) %>% slice(1) %>% ungroup() %>% summarise()
  expanded_rio_border <- rio_border %>% st_transform(5880) %>% st_buffer(3000) %>% st_transform(st_crs(rio_border))
  
  xlim <- c(st_bbox(rio_border)[1], st_bbox(rio_border)[3])
  ylim <- c(st_bbox(expanded_rio_border)[2], st_bbox(rio_border)[4])

  max_increase <- max(accessibility_data$increase)
  total_opportunities <- sum(grid_data$opportunities)
  
  p <- ggplot() +
    geom_sf(data = rj_state, color = NA, fill = "#efeeec") +
    geom_sf(data = accessibility_data, aes(fill = increase), color = NA) +
      facet_wrap(~ percentage_minimum_wage, nrow = 2) +
    geom_sf(data = rio_border, color = "gray50", fill = NA) +
    ggsn::scalebar(data = rio_border, dist = 10, dist_unit = "km", location = "bottomright", transform = TRUE, model = "WGS84",
                   height = 0.03, border.size = 0.4, st.dist = 0.05, st.size = 3) +
    coord_sf(xlim = xlim, ylim = ylim) +
    scale_fill_gradient(name = text_labels$increase_maps$legend_title, low = "#efeeec", high = "red",
                        breaks = seq(0, max_increase, max_increase / 3), labels = scales::label_percent(scale = 100 / total_opportunities)) +
    theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid = element_blank(), panel.background = element_rect(fill = "#aadaff"),
          legend.position = "bottom", legend.box.just = "left",
          strip.text.x = element_text(size = 13), strip.background = element_rect(fill = NA))
  
  p <- lemon::reposition_legend(p, position = "bottom left", panel = "panel-2-2")
  
  # save plot
  
  ggsave(paste0("./analysis/", text_labels$lang, "/cases_bu/increase_map_tt_", travel_time, "_res_", res, ".png"),
         plot = p,
         width = 9,
         height = 5.8)
}

boxplot_increase_cases_bu <- function(grid_data, travel_time, percentage_minimum_wage, text_labels, res) {
  
  # read data
  
  accessibility_with_bu <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(paste0("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds")))
  accessibility_without_bu <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(paste0("./results/without_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds")))
  
  # calculate the accessibility difference between the cases with and without bilhete único

  accessibility_data <- purrr::map(seq_along(percentage_minimum_wage), function(i) left_join(accessibility_with_bu[[i]], st_drop_geometry(accessibility_without_bu[[i]]), by = "id")) %>%
    purrr::map(function(i) mutate(i, increase = accessibility.x - accessibility.y)) %>% 
    purrr::map(st_drop_geometry)
  
  # prepare the dataset to be used in the plots
  
  accessibility_data <- accessibility_data %>% 
    purrr::map(function(i) left_join(i, grid_data, by = "id")) %>% 
    purrr::map(function(i) filter(i, population > 0))
  
  # calculate the palma ratio in each case
  
  ratios <- accessibility_data %>%
    purrr::map_dbl(palma_ratio, variable = "increase")
  
  # bind each distribution to the same dataframe to plot it as a facet of the same plot
  # then convert id to factors to ensure adequate facet order 
  
  accessibility_data <- accessibility_data %>% 
    purrr::set_names(as.character(percentage_minimum_wage)) %>%
    bind_rows(.id = "percentage_minimum_wage") %>% 
    mutate(percentage_minimum_wage = factor(percentage_minimum_wage, levels = unique(percentage_minimum_wage), labels = text_labels$boxplot_difference$facets_title))
  
  # create a dataframe with the palma ratios to plot them as annotations
  
  palma_data <- tibble::tibble(
    percentage_minimum_wage = factor(percentage_minimum_wage, levels = percentage_minimum_wage, labels = text_labels$boxplot_difference$facets_title),
    x = 0.625,
    y = max(accessibility_data$increase),
    label = paste0(text_labels$boxplot_difference$palma_ratio, format(round(ratios, digits = 4), nsmall = 4))
  )
  
  # plot settings
  
  max_increase <- max(accessibility_data$increase)
  total_opportunities <- sum(grid_data$opportunities)
  
  ggplot(accessibility_data, aes(income_quantile, increase)) +
    geom_boxplot(aes(weight = population)) +
    facet_wrap(~ percentage_minimum_wage, nrow = 1) +
    labs(x = text_labels$boxplot$x_axis, y = text_labels$boxplot$y_axis) +
    geom_text(data = palma_data, aes(x, y, label = label, hjust = "left"), size = 4.5) +
    scale_y_continuous(
      limits = c(0, max_increase),
      breaks = seq(0, max_increase, max_increase / 3),
      labels = scales::percent_format(scale = 100 / total_opportunities)
    ) +
    theme(
      strip.text.x = element_text(size = 13),
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.title.y = element_text(size = 12),
      axis.text.y = element_text(size = 11)
    )
  
  # save plot
  
  ggsave(paste0("./analysis/", text_labels$lang, "/cases_bu/boxplot_increase_tt_", travel_time, "_res_", res, ".png"),
         width = 9,
         height = 3.5,
         units = "in")
  
}

theil_cases_bu <- function(grid_data, travel_time, percentage_minimum_wage, text_labels, res) {
  
  # read data
  
  accessibility_with_bu <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(paste0("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds")))
  accessibility_without_bu <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(paste0("./results/without_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds")))
  
  # calculate the accessibility difference between the cases with and without bilhete único and join grid_data
  
  accessibility_data <- purrr::map(seq_along(percentage_minimum_wage), function(i) left_join(accessibility_with_bu[[i]], st_drop_geometry(accessibility_without_bu[[i]]), by = "id")) %>%
    purrr::map(function(i) mutate(i, increase = accessibility.x - accessibility.y)) %>% 
    purrr::map(st_drop_geometry) %>% 
    purrr::map(function(i) left_join(i, grid_data, by = "id")) %>% 
    purrr::map(function(i) filter(i, population > 0))
  
  # calculate the theil index and its components in each case
  # set names for each observation and bind them together in the same dataframe
  
  theil_data <- accessibility_data %>% 
    purrr::map(theil_info, variable = "increase")%>%
    purrr::set_names(as.character(percentage_minimum_wage)) %>%
    bind_rows(.id = "percentage_minimum_wage")
  
  # prepare dataframe to plot and create a label_y column to place annotations within each stack
  # convert component info and minimum wage percentages to factors
  
  theil_data <- theil_data %>% 
    group_by(percentage_minimum_wage, component) %>% 
    summarise(share = sum(share), .groups = "drop_last") %>% 
    arrange(percentage_minimum_wage, desc(component)) %>% 
    mutate(label_y = cumsum(share) - share/2) %>% 
    ungroup() %>% 
    mutate(
      component = factor(component, levels = c("between", "within"), labels = text_labels$theil$components_names),
      percentage_minimum_wage = factor(percentage_minimum_wage, unique(percentage_minimum_wage), text_labels$theil$bar_labels)
    )
  
  # find max total theil to resize the plot's graphic area in order to fit the annotation on top of bars
  
  max_theil_data <- theil_data %>% 
    group_by(percentage_minimum_wage) %>% 
    summarise(theil = sum(share), .groups = "drop")
  
  max_theil <- max(max_theil_data$theil)
  
  y_upper_limit <- purrr::map_dbl(max_theil + 0.05, function(i, level = 1) round(i + 5*10^(-level-1), level))
  
  # plot settings
  
  ggplot(theil_data) +
    geom_col(aes(percentage_minimum_wage, share, fill = component)) + 
    geom_text(aes(percentage_minimum_wage, label_y, label = format(round(share, digits = 4)), nsmall = 4), color = "white") +
    stat_summary(fun = sum, aes(percentage_minimum_wage, share, label = format(round(..y.., digits = 4), nsmall = 4), group = percentage_minimum_wage), geom = "text", vjust = -0.5) +
    labs(x = text_labels$theil$x_axis, y = text_labels$theil$y_axis, fill = text_labels$theil$component) +
    coord_cartesian(ylim = c(0, y_upper_limit)) +
    theme(
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(size = 11, angle = 22.5, hjust = 1),
      axis.title.y = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
    )
  
  # save plot
  
  ggsave(paste0("./analysis/", text_labels$lang, "/cases_bu/theil_components_tt_", travel_time, "_res_", res, ".png"),
         width = 7,
         height = 3,
         units = "in")
  
}


# support functions -------------------------------------------------------




extract_rapid_transit <- function(router) {
  
  # create folders and files paths
  
  temp_folder   <- tempdir()
  sp_folder     <- paste0(temp_folder, "/supervia")
  ft_folder     <- paste0(temp_folder, "/fetranspor")
  router_folder <- paste0("./otp/graphs/", router)
  
  filenames <- list.files(router_folder)
  
  ft_file <- filenames[grep("fetranspor", filenames)]
  ft_file <- paste0(router_folder, "/", ft_file)
  
  sp_file <- filenames[grep("supervia", filenames)]
  sp_file <- paste0(router_folder, "/", sp_file)
  
  # extract required gtfs files to temp_folder
  
  unzip(
    ft_file, 
    files = c("routes.txt", "stops.txt", "trips.txt", "stop_times.txt"), 
    exdir = ft_folder
  )
  
  unzip(
    sp_file, 
    files = c("routes.txt", "stops.txt", "trips.txt", "stop_times.txt"), 
    exdir = sp_folder
  )
  
  # read supervia stations and create a sf with their stations
  
  sp_routes_dt <- fread(paste0(sp_folder, "/routes.txt"))
  sp_routes    <- sp_routes_dt[grepl("^Ramal", route_long_name)]$route_id
  sp_stations  <- routes_to_sf(sp_folder, sp_routes, "rail", 4674)
  
  # same with subway and BRT
  
  ft_routes_dt <- fread(paste0(ft_folder, "/routes.txt"))
  
  sbw_routes   <- ft_routes_dt[route_short_name %chin% c("L1/L4", "L2")]$route_id
  sbw_stations <- routes_to_sf(ft_folder, sbw_routes, "subway", 4674)
  
  brt_routes   <- ft_routes_dt[grepl("^BRT", route_short_name)]$route_id
  brt_stations <- routes_to_sf(ft_folder, brt_routes, "brt", 4674)
  
  # clean up - remove temp folders and create list with stations
  
  unlink(sp_folder, recursive = TRUE)
  unlink(ft_folder, recursive = TRUE)
  
  stations <- rbind(sp_stations, sbw_stations, brt_stations)
  
  return(stations)
  
}

routes_to_sf <- function(folder, routes, mode, crs) {
  
  trips_dt  <- fread(paste0(folder, "/trips.txt"))
  trips_dt  <- trips_dt[route_id %in% routes]
  
  req_trips <- trips_dt$trip_id
  
  stop_times_dt <- fread(paste0(folder, "/stop_times.txt"))
  stop_times_dt <- stop_times_dt[trip_id %chin% req_trips]
  
  req_stops <- unique(stop_times_dt$stop_id)
  
  stops_dt <- fread(paste0(folder, "/stops.txt"))
  stops_dt <- stops_dt[stop_id %in% req_stops]
  
  stops_sf <- sfheaders::sf_point(stops_dt, x = "stop_lon", y = "stop_lat")
  stops_sf <- st_set_crs(stops_sf, crs)
  stops_sf <- cbind(stops_sf, stop_name = stops_dt$stop_name, mode = mode)
  
  return(stops_sf)
  
}

palma_ratio <- function(data, variable = "accessibility") {
  
  richest_10 <- data[income_quantile == 10,
                     .(avg_variable = weighted.mean(get(variable), population))]
  
  poorest_40 <- data[income_quantile %in% 1:4,
                     .(avg_variable = weighted.mean(get(variable), population))]
  
  palma_ratio <- richest_10$avg_variable / poorest_40$avg_variable
  
  palma_ratio
  
}

theil_info <- function(accessibility_data, variable = "accessibility") {
  
  # change the desired variable column name to "variable" in order to allow calculations for any variable
  
  accessibility_data <- accessibility_data %>% 
    rename(variable = any_of(variable)) %>%
    mutate(total_variable = variable * population)
  
  # calculate the between-group component
  # group by quantile, calculate its share of the component and save it in a list
  
  between_group_data <- accessibility_data %>% 
    group_by(income_quantile) %>% 
    summarise(
      population = sum(population),
      total_variable = sum(total_variable),
      .groups = "drop"
    ) %>% 
    mutate(theil_share = (total_variable / sum(total_variable)) * log((total_variable / sum(total_variable)) / (population / sum(population))))
  
  unique_quantiles <- between_group_data$income_quantile
  n <- length(unique_quantiles)
  
  between_group <- list(component = rep("between", n), income_quantile = unique_quantiles, share = between_group_data$theil_share)
  
  # calculate the within-group component
  # it is the weighted average of each group's own theil index, where accessibility is the weight
  
  within_group_share <- vector("double", length = n)
  
  for (i in seq_along(unique_quantiles)) {
    
    filtered_data <- accessibility_data %>% 
      filter(income_quantile == unique_quantiles[i])
    
    within_group_share[i] <- theil_index(filtered_data) * sum(filtered_data$total_variable)
    
  }
  
  within_group_share <- within_group_share / sum(accessibility_data$total_variable)
  
  within_group <- list(component = rep("within", n), income_quantile = unique_quantiles, share = within_group_share)
  
  # bind between- and within-group components lists together in a dataframe
  
  info <- bind_rows(within_group, between_group)
  
  info
  
}

theil_index <- function(accessibility_data) {
  
  accessibility_data <- accessibility_data %>% 
    filter(variable > 0) %>% 
    mutate(
      total_variable = variable * population,
      theil_share = (total_variable / sum(total_variable)) * log((total_variable / sum(total_variable)) / (population / sum(population)))
    )
  
  index <- sum(accessibility_data$theil_share)
  
  index
  
}