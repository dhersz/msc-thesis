library(dplyr)
library(sf)
library(tmap)
library(ggplot2)

analyse_results <- function(travel_time = 60, res = 7) {
  
  # read necessary objects
  
  grid_data <- readr::read_rds(stringr::str_c("./data/rio_h3_grid_res_", res, "_with_data.rds")) %>% 
    st_drop_geometry() %>% 
    as_tibble()

  # first analysis: effect of incorporating monetary costs
  # create visualisations comparing accessibility at different monetary costs thresholds and without a
  # monetary cost threshold
  
  travel_time <- travel_time
  percentage_minimum_wage <- c(20, 30, 40, 1000)
  
  maps_different_costs(grid_data, travel_time, percentage_minimum_wage, res = res)
  boxplot_different_costs(grid_data, travel_time, percentage_minimum_wage, res = res)
  theil_different_costs(grid_data, travel_time, percentage_minimum_wage, res = res)
  
  # second analysis: effect of BU fare policy
  # create visualisations comparing accessibility at a specific combination of time travel and cost thresholds
  
  # travel_time <- 60
  # percentage_minimum_wage <- 20
  # 
  # maps_cases_bu(grid_data, travel_time, percentage_minimum_wage, res)
  # boxplot_cases_bu(grid_data, travel_time, percentage_minimum_wage, res)
  # theil_cases_bu(grid_data, travel_time, percentage_minimum_wage, res)
  
}

accessibility_distribution <- function(accessibility, grid_data, n = 10) {
  
  # join income and population data to the accessibility dataframe, arrange by income and expand each row n times
  # where n equals the population with the given income
  
  distribution <- accessibility %>% 
    st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    left_join(grid_data, by = "id") %>% 
    mutate(
      avg_income = total_income / population,
      population = round(population)
    ) %>% 
    select(-c(id, opportunities, total_income)) %>% 
    arrange(avg_income) %>% 
    tidyr::uncount(population)
  
  # classify each person based on the income quantil he or she is located at
  
  qntls <- quantile(distribution$avg_income, probs = seq(0, 1, 1/n))
  
  distribution <- distribution %>% 
    mutate(
      income_quantile = cut(avg_income, qntls, labels = stringr::str_c("Q", 1:n), include.lowest = TRUE)
    )
  
  distribution
  
}

maps_different_costs <- function(grid_data, travel_time, percentage_minimum_wage, res) {
  
  # read data
  
  rj_state <- geobr::read_municipality(code_muni = "RJ")
  
  accessibility_data <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(stringr::str_c("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds")))
  
  # set breaks and labels
  
  max_accessibility <- max(purrr::map_dbl(accessibility_data, function(i) max(i$accessibility)))
  total_opportunities <- sum(grid_data$opportunities)
  proportion <- max_accessibility / total_opportunities
  
  n <- 4
  breaks <- seq(0, max_accessibility, length.out = n)
  labels <- format(round(seq(0, proportion, length.out = n) * 100, digits = 1), nsmall = 1)
  labels[n] <- stringr::str_c(labels[n], "%")
  
  # create character vector holding map titles
  
  titles <- purrr::map_chr(
    percentage_minimum_wage,
    function(i) ifelse(
      i <= 100,
      stringr::str_c("travel time <= ", travel_time, " min;\ncost <= ", i, "% of minimum wage"),
      stringr::str_c("travel time <= ", travel_time, " min;\nno cost considered")
      )
    )
  
  # create a vector used to show legend only in the last map
  
  n <- length(percentage_minimum_wage)
  show_legends <- c(rep(FALSE, n - 1), TRUE)
  
  # create maps, arrange them together in the same visualisation and save the resulting plot
  
  accessibility_maps <- purrr::map(
    seq_along(percentage_minimum_wage),
    function(i) map_accessibility(accessibility_data[[i]], grid_data, rj_state, breaks, labels,
                                  title = titles[i], title.size = 0.9, legend.show = show_legends[i])
  )
  
  maps_combined <- tmap_arrange(accessibility_maps, nrow = 2)
  
  tmap_save(maps_combined,
             stringr::str_c("./analysis/maps_different_costs_tt_", travel_time, "_res_", res, ".png"),
             width = 2100,
             height = 1650)
  
}

maps_cases_bu <- function(grid_data, travel_time, percentage_minimum_wage, res) {
  
  # create object used in map functions to differentiate the cases with and without BU
  
  without_with <- c("without", "with")
  
  # read data
  
  rj_state <- geobr::read_municipality(code_muni = "RJ")
  
  accessibility_data <- purrr::map(without_with, function(i) readr::read_rds(stringr::str_c("./results/", i, "_bu_tt_", travel_time, "_mc_", percentage_minimum_wage, "_res_", res, ".rds")))
  
  # set breaks and labels
  
  max_accessibility <- max(purrr::map_dbl(accessibility_data, function(i) max(i$accessibility)))
  total_opportunities <- sum(grid_data$opportunities)
  proportion <- max_accessibility / total_opportunities
  
  n <- 4
  breaks <- seq(0, max_accessibility, length.out = n)
  labels <- format(round(seq(0, proportion, length.out = n) * 100, digits = 1), nsmall = 1)
  labels[n] <- stringr::str_c(labels[n], "%")
  
  # create character vector holding map titles
  
  titles <- stringr::str_c(without_with, " BU;\ntravel time <= ", travel_time, " min;\ncost <= ", percentage_minimum_wage, "% of minimum wage")
  
  # create a vector used to show legend only in the last map
  
  show_legends <- c(FALSE, TRUE)
  
  # create maps, arrange them together in the same visualisation and save the resulting plot
  
  accessibility_maps <- purrr::map(
    seq_along(without_with),
    function(i) map_accessibility(accessibility_data[[i]], grid_data, rj_state, breaks, labels,
                                  title = titles[i], title.size = 0.9, legend.show = show_legends[i])
  )
  
  maps_combined <- tmap_arrange(accessibility_maps, nrow = 2)
  
  tmap_save(maps_combined,
            stringr::str_c("./analysis/maps_cases_bu_tt_", travel_time, "_mc_", percentage_minimum_wage, "_res_", res, ".png"),
            width = 1950,
            height = 2100)
  
}

map_accessibility <- function(accessibility, grid_data, rj_state, breaks, labels, title, title.size, legend.show) {
  
  # map settings
  
  rio_border <- accessibility %>% summarise()
  
  tm_shape(rj_state, bbox = st_bbox(rio_border)) +
    tm_fill(col = "#efeeec") +
    
  tm_shape(accessibility) +
    tm_fill(col = "accessibility",
            title = "   Accessibility",
            breaks = breaks,
            labels = labels, 
            style = "cont",
            palette = "inferno",
            legend.is.portrait = FALSE) +
    
  tm_shape(rio_border) +
    tm_borders(col = "#000004") +
    
  tm_scale_bar(c(0, 5, 10, 15), position = c("left", "bottom")) +
  
  tm_layout(title = title,
            title.position = c("left", "top"),
            title.size = title.size,
            frame = "white",
            inner.margins = c(0.10, 0.02, 0.02, 0.02),
            bg.color = "#aadaff",
            legend.position = c("right", "bottom"),
            legend.bg.color = "#aadaff",
            legend.show = legend.show)
  
}

boxplot_different_costs <- function(grid_data, travel_time, percentage_minimum_wage, res) {
  
  # read data 
  
  accessibility_data <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(stringr::str_c("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds")))
  
  # generate the accessibility distribution from the accessibility of each case
  
  accessibility_dist <- accessibility_data %>% 
    purrr::map(function(i) accessibility_distribution(i, grid_data, n = 10)) 
  
  # calculate the palma ratio in each case
  
  ratios <- accessibility_dist %>% 
    purrr::map_dbl(palma_ratio)
  
  # bind each distribution to the same dataframe to plot it as a facet of the same plot
  # set names that can be used as the facet identifier
  
  accessibility_dist <- accessibility_dist %>%
    purrr::set_names(as.character(percentage_minimum_wage)) %>%
    bind_rows(.id = "percentage_minimum_wage")
  
  # convert names to factors so the facets are adequately ordered 
  
  n <- length(percentage_minimum_wage)
  
  accessibility_dist$percentage_minimum_wage <- factor(
    accessibility_dist$percentage_minimum_wage,
    levels = percentage_minimum_wage,
    labels = c(stringr::str_c("Cost <= ", percentage_minimum_wage[1:(n-1)], "% of minimum wage"), "No cost considered")
  )

  # create a dataframe with the palma ratios to plot them as annotations
  
  palma_data <- tibble::tibble(
    percentage_minimum_wage = factor(percentage_minimum_wage,
                                     levels = percentage_minimum_wage,
                                     labels = c(stringr::str_c("Cost <= ", percentage_minimum_wage[1:(n-1)], "% of minimum wage"), "No cost considered")),
    x = 0.625,
    y = max(accessibility_dist$accessibility),
    label = stringr::str_c("Palma ratio: ", format(round(ratios, digits = 4), nsmall = 4))
  )
  
  # plot settings
  
  ggplot(accessibility_dist, aes(income_quantile, accessibility)) +
    geom_boxplot() +
    facet_wrap(~ percentage_minimum_wage, ncol = 2) +
    labs(title = stringr::str_c(travel_time, "-min transit job accessibility distribution"), x = "Income quantile", y = "Accessibility") +
    geom_text(data = palma_data, aes(x, y, label = label, hjust = "left")) +
    theme(strip.text.x = element_text(size = 12))
  
  # save plot
  
  ggsave(stringr::str_c("./analysis/boxplot_different_costs_tt_", travel_time, "_res_", res, ".png"),
         width = 7,
         height = 8,
         units = "in")
  
}

boxplot_cases_bu <- function(grid_data, travel_time, percentage_minimum_wage, res) {
  
  # create object used in map functions to differentiate the cases with and without BU
  
  without_with <- c("without", "with")
  
  # read data 
  
  accessibility_data <- purrr::map(without_with, function(i) readr::read_rds(stringr::str_c("./results/", i, "_bu_tt_", travel_time, "_mc_", percentage_minimum_wage, "_res_", res, ".rds")))
  
  # generate the accessibility distribution from the accessibility of each case
  
  accessibility_dist <- accessibility_data %>% 
    purrr::map(function(i) accessibility_distribution(i, grid_data, n = 10)) 
  
  # calculate the palma ratio in each case
  
  ratios <- accessibility_dist %>% 
    purrr::map_dbl(palma_ratio)
  
  # bind each distribution to the same dataframe to plot it as a facet of the same plot
  # set names that can be used as the facet identifier
  
  accessibility_dist <- accessibility_dist %>%
    purrr::set_names(without_with) %>%
    bind_rows(.id = "case")
  
  # convert names to factors so the facets are adequately ordered 
  
  n <- length(without_with)
  
  accessibility_dist$case <- factor(
    accessibility_dist$case,
    levels = without_with,
    labels = stringr::str_c(without_with, " BU")
  )
  
  # create a dataframe with the palma ratios to plot them as annotations
  
  palma_data <- tibble::tibble(
    case = factor(without_with,
                  levels = without_with,
                  labels = stringr::str_c(without_with, " BU")),
    x = 0.625,
    y = max(accessibility_dist$accessibility),
    label = stringr::str_c("Palma ratio: ", format(round(ratios, digits = 4), nsmall = 4))
  )
  
  # plot settings
  
  ggplot(accessibility_dist, aes(income_quantile, accessibility)) +
    geom_boxplot() +
    facet_wrap(~ case, ncol = 2) +
    labs(title = stringr::str_c(travel_time, "-min and ", percentage_minimum_wage, "% of min. wage transit job accessibility distribution"), x = "Income quantile", y = "Accessibility") +
    geom_text(data = palma_data, aes(x, y, label = label, hjust = "left")) +
    theme(strip.text.x = element_text(size = 12))
  
  # save plot
  
  ggsave(stringr::str_c("./analysis/boxplot_cases_bu_tt_", travel_time, "_mc_", percentage_minimum_wage, "_res_", res, ".png"),
         width = 7,
         height = 4,
         units = "in")
  
}

boxplot_analysis <- function(access_dist_with_bu, access_dist_without_bu, travel_time, percentage_minimum_wage, res) {
  
  # create an object that holds both with and without BU accessibility to be used as the plot data
  
  access_dist <- access_dist_with_bu %>% 
    mutate(type = "with_bu") %>% 
    bind_rows(access_dist_without_bu) %>% 
    mutate(type = ifelse(is.na(type), "without_bu", type))
  
  # palma ratio calculations and creating a tibble to hold annotations data
  
  palma_without_bu <- round(palma_ratio(access_dist_without_bu), digits = 4)
  palma_with_bu <- round(palma_ratio(access_dist_with_bu), digits = 4)
  
  # transforming to factor to ensure the correct order when plotting
  
  access_dist$type <- factor(access_dist$type,
                             levels = c("without_bu", "with_bu"),
                             labels = c("Without BU", "With BU"))

  palma_data <- tibble(type = factor(c("without_bu", "with_bu"), levels = c("without_bu", "with_bu"), labels = c("Without BU", "With BU")),
                       x = 0.75,
                       y = max(access_dist$accessibility),
                       label = stringr::str_c("Palma ratio: ", c(palma_without_bu, palma_with_bu)))
  
  # plot settings
  
  ggplot(access_dist, aes(income_quantile, accessibility)) +
    geom_boxplot() +
    facet_wrap(~ type, ncol = 2) +
    labs(title = "Accessibility distribution per income quantile", x = "Income quantile", y = "Accessibility") +
    geom_text(data = palma_data, aes(x, y, label = label, hjust = "left")) +
    theme(strip.text.x = element_text(size = 12))
  
  ggsave(stringr::str_c("./analysis/boxplot_accessibility_tt_", travel_time, "_mc_", percentage_minimum_wage, "_res_", res, ".png"),
         width = 7,
         height = 4,
         units = "in")
    
}

palma_ratio <- function(access_dist) {
  
  richest_10 <- access_dist %>% 
    filter(income_quantile == "Q10") %>% 
    summarise(accessibility_sum = sum(accessibility))
  
  poorest_40 <- access_dist %>% 
    filter(income_quantile %in% c("Q1", "Q2", "Q3", "Q4")) %>% 
    summarise(accessibility_sum = sum(accessibility))
  
  palma_ratio <- richest_10$accessibility_sum / poorest_40$accessibility_sum
  
  palma_ratio
  
}

theil_analysis <- function(access_dist_with_bu, access_dist_without_bu, travel_time, percentage_minimum_wage, res) {
  
  theil_with_bu <- theil_info(access_dist_with_bu)
  theil_without_bu <- theil_info(access_dist_without_bu)
  
  # first plot: stacked bar showing total theil index and its components
  
  theil_stacked <- tibble::tribble(
    ~case,        ~component, ~share,                        ~label,                                             ~y,
    "with_bu",    "within",   theil_with_bu$within_group,     round(theil_with_bu$within_group, digits = 4),     theil_with_bu$within_group / 2,
    "with_bu",    "between",  theil_with_bu$between_group,    round(theil_with_bu$between_group, digits = 4),    theil_with_bu$within_group + theil_with_bu$between_group / 2,
    "without_bu", "within",   theil_without_bu$within_group,  round(theil_without_bu$within_group, digits = 4),  theil_without_bu$within_group / 2,
    "without_bu", "between",  theil_without_bu$between_group, round(theil_without_bu$between_group, digits = 4), theil_without_bu$within_group + theil_without_bu$between_group / 2
  )
  
  theil_stacked$case <- factor(theil_stacked$case, levels = c("without_bu", "with_bu"), labels = c("Without BU", "With BU"))
  theil_stacked$component <- factor(theil_stacked$component, levels = c("between", "within"), labels = c("Between-group", "Within-group"))

  ggplot(theil_stacked) +
    geom_col(aes(case, share, fill = component)) + 
    geom_text(aes(case, y, label = label), color = "white") +
    stat_summary(fun = sum, aes(case, share, label = round(..y.., digits = 4), group = case), geom = "text", vjust = -0.5) +
    coord_cartesian(ylim = c(0, max(theil_with_bu$theil_index, theil_without_bu$theil_index) + 0.01)) +
    labs(title = "Theil index components", x = "Case", y = "Theil index", fill = "Component")
  
  ggsave(stringr::str_c("./analysis/theil_total_tt_", travel_time, "_mc_", percentage_minimum_wage, "_res_", res, ".png"),
         width = 7,
         height = 4,
         units = "in")
  
  # second plot: scatter plot showing the participation of each group in the between-group component
  
  n <- length(theil_with_bu$between_group_share)
  quantile_names <- names(theil_with_bu$between_group_share)
  
  theil_scatter <- tibble::tibble(
    case = c(rep("with_bu", n), rep("without_bu", n)),
    income_quantile = rep(quantile_names, 2),
    share = c(theil_with_bu$between_group_share, theil_without_bu$between_group_share)
  )
  
  theil_scatter$case <- factor(theil_scatter$case, levels = c("without_bu", "with_bu"), labels = c("Without BU", "With BU"))
  theil_scatter$income_quantile <- factor(theil_scatter$income_quantile, levels = names(theil_with_bu$between_group_share))
  
  ggplot(theil_scatter, aes(income_quantile, share)) +
    geom_col() +
    facet_wrap(~ case, ncol = 2) + 
    labs(title = "Theil index between-group component portion per income quantile", x = "Income quantile", y = "Portion") +
    theme(strip.text.x = element_text(size = 12))
  
  ggsave(stringr::str_c("./analysis/between_group_portion_tt_", travel_time, "_mc_", percentage_minimum_wage, "_res_", res, ".png"),
         width = 7,
         height = 4,
         units = "in")
    
}

theil_different_costs <- function(grid_data, travel_time, percentage_minimum_wage, res) {
  
  # read data 
  
  accessibility_data <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(stringr::str_c("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds")))
  
  # generate the accessibility distribution from the accessibility of each case
  
  accessibility_dist <- accessibility_data %>% 
    purrr::map(function(i) accessibility_distribution(i, grid_data, n = 10)) 
  
  # calculate the theil index and its components in each case
  # set names for each observation and bind them together in the same dataframe
  
  theil_data <- accessibility_dist %>% 
    purrr::map(theil_info)%>%
    purrr::set_names(as.character(percentage_minimum_wage)) %>%
    bind_rows(.id = "percentage_minimum_wage")
  
  t <- theil_data %>% 
    filter(component == "Between-group", income_quantile %in% c("Q9", "Q10"))
  
  return(t)
  
  # convert names to factors so the facets are adequately ordered 
  
  n <- length(percentage_minimum_wage)
  
  theil_data$percentage_minimum_wage <- factor(
    theil_data$percentage_minimum_wage,
    levels = percentage_minimum_wage,
    labels = c(stringr::str_c(percentage_minimum_wage[1:(n-1)], "% of min. wage"), "No cost considered")
  )
  
  # first plot: stacked bar chart
  
  theil_components_stacked(theil_data, travel_time, res)
  
  # second plot: between-group component share per decile
  
  theil_between_group_share(theil_data, travel_time, res)
  
}

theil_info <- function(accessibility_dist) {
  
  # create a population column with 1s to use theil_index() both when grouped or when ungrouped
  
  accessibility_dist <- accessibility_dist %>% mutate(population = 1)
  
  # calculate the between-group component
  # group by quantile, calculate its share of the component and save it in a list
  
  grouped_dist <- accessibility_dist %>% 
    group_by(income_quantile) %>% 
    summarise(
      population = n(),
      accessibility = sum(accessibility)
    ) %>% 
    mutate(
      theil_share = (accessibility / sum(accessibility)) * log((accessibility / sum(accessibility)) / (population / sum(population)))
    )
  
  unique_quantiles <- unique(accessibility_dist$income_quantile)
  n <- length(unique_quantiles)
  
  between_group <- list(component = rep("between", n), income_quantile = unique_quantiles, share = grouped_dist$theil_share)
  
  # calculate the within-group component
  # it is the weighted average of each group's own theil index, where accessibility is the weight
  
  within_group_share <- vector("double", length = n)
  
  for (i in seq_along(unique_quantiles)) {
    
    filtered_dist <- accessibility_dist %>% 
      filter(income_quantile == unique_quantiles[i])
    
    within_group_share[i] <- theil_index(filtered_dist) * sum(filtered_dist$accessibility)
    
  }
  
  within_group_share <- within_group_share / sum(accessibility_dist$accessibility)
  
  within_group <- list(component = rep("within", n), income_quantile = unique_quantiles, share = within_group_share)
  
  # bind between- and within-group components lists together in a dataframe
  
  info <- bind_rows(within_group, between_group)
  
  # convert component column to factors so the plots later generated are adequately ordered
  
  info$component <- factor(info$component, levels = c("between", "within"), labels = c("Between-group", "Within-group"))
  
  info
  
}

theil_index <- function(access_dist) {
  
  accessibility_total <- sum(access_dist$accessibility)
  population_total <- sum(access_dist$population)
  
  access_dist <- access_dist %>% 
    mutate(
      theil_share = (accessibility / accessibility_total) * log((accessibility / accessibility_total) / (population / population_total))
    )
  
  index <- sum(access_dist$theil_share)
  
  index
  
}

theil_components_stacked <- function(theil_data, travel_time, res) {
  
  # create a dataframe with aggregated component data
  # create a label_y column to place annotations within each stack
  
  theil_stacked <- theil_data %>% 
    group_by(percentage_minimum_wage, component) %>% 
    summarise(share = sum(share)) %>% 
    arrange(percentage_minimum_wage, desc(component)) %>% 
    mutate(label_y = cumsum(share) - share/2) %>% 
    ungroup()
  
  # find max total theil to resize the plot's graphic area in order to fit the annotation on top of bars
  
  max_theil_data <- theil_stacked %>% 
    group_by(percentage_minimum_wage) %>% 
    summarise(theil = sum(share)) %>% 
    ungroup
  
  max_theil <- max(max_theil_data$theil)
  
  y_upper_limit <- purrr::map_dbl(max_theil + 0.05, function(i, level = 1) round(i + 5*10^(-level-1), level))
  
  # plot settings
  
  ggplot(theil_stacked) +
    geom_col(aes(percentage_minimum_wage, share, fill = component)) + 
    geom_text(aes(percentage_minimum_wage, label_y, label = round(share, digits = 4)), color = "white") +
    stat_summary(fun = sum, aes(percentage_minimum_wage, share, label = round(..y.., digits = 4), group = percentage_minimum_wage), geom = "text", vjust = -0.5) +
    labs(title = "60-min transit job accessibility Theil index", x = "Cost threshold", y = "Theil index", fill = "Component") +
    coord_cartesian(ylim = c(0, y_upper_limit)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # save plot
  
  ggsave(stringr::str_c("./analysis/theil_components_stacked_tt_", travel_time, "_res_", res, ".png"),
         width = 7,
         height = 4,
         units = "in")
  
}

theil_between_group_share <- function(theil_data, travel_time, res) {
  
  # filter between-group data only
  
  theil_between <- theil_data %>% 
    filter(component == "Between-group")
  
  # plot settings
  
  ggplot(theil_between, aes(income_quantile, share)) +
    geom_col() +
    facet_wrap(~ percentage_minimum_wage, ncol = 2) + 
    labs(title = "60-min transit job accessibility Theil index between-group component\nshare per cost threshold and income quantile", x = "Income quantile", y = "Share") +
    theme(strip.text.x = element_text(size = 12))
  
  # save plot
  
  ggsave(stringr::str_c("./analysis/between_group_share_tt_", travel_time, "_res_", res, ".png"),
         width = 7,
         height = 8,
         units = "in")
  
}