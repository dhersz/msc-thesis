library(dplyr)
library(sf)
library(tmap)
library(ggplot2)

analyse_results <- function(travel_time_threshold = 90, monetary_cost_threshold = 7.5, res = 7) {
  
  accessibility_with_bu <- readr::read_rds(stringr::str_c("./results/accessibility_with_bu_tt_", travel_time_threshold, "_mc_", monetary_cost_threshold * 10, "_res_", res, ".rds"))
  accessibility_without_bu <- readr::read_rds(stringr::str_c("./results/accessibility_without_bu_tt_", travel_time_threshold, "_mc_", monetary_cost_threshold * 10, "_res_", res, ".rds"))
  
  grid_data <- readr::read_rds(stringr::str_c("./data/rio_h3_grid_res_", res, "_with_data.rds")) %>% 
    st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    select(-opportunities)
  
  access_dist_with_bu <- accessibility_distribution(accessibility_with_bu, grid_data, n = 10)
  access_dist_without_bu <- accessibility_distribution(accessibility_without_bu, grid_data, n = 10)
  
  # create a sf object which holds the accessibility difference due to the implementation of BU
  
  accessibility_difference <- accessibility_without_bu %>% 
    rename(without = accessibility) %>% 
    left_join(st_drop_geometry(accessibility_with_bu), by = "id") %>% 
    rename(with = accessibility) %>% 
    mutate(accessibility = with - without) %>% 
    select(id, accessibility)
  
  # maps for quick visualisation: before, after and difference --------------

  # find the percentage of the total opportunities that the max accessibility is, round to the immediately
  # heigher 10% and use it as the upper limit of the legend - function taken from https://stackoverflow.com/questions/35807523/r-decimal-ceiling
  # use this max to create the breaks used in the legend of the maps
  
  max_accessibility <- max(accessibility_with_bu$accessibility)
  total_opportunities <- sum(grid_data$opportunities)
  
  percentage_of_total <- (max_accessibility/total_opportunities) %>% 
    purrr::map_dbl(function(x, digits = 1) round(x + 5*10^(-digits-1), digits))
  
  n <- 5
  breaks <- seq(0, percentage_of_total * total_opportunities, length.out = n)
  labels <- seq(0, percentage_of_total, length.out = n) * 100
  labels[n] <- stringr::str_c(labels[n], "%")
  labels[c(2, 4)] <- ""
  
  # create individual maps and place them sideways in the same visualisation
  # maybe it's better to do this with facets!
  
  map_with_bu <- map_accessibility(accessibility_with_bu, breaks = breaks, labels = labels, title = "Accessibility with BU")
  map_without_bu <- map_accessibility(accessibility_without_bu, breaks = breaks, labels = labels, title = "Accessibility without BU")
  
  comparative_map <- tmap_arrange(map_without_bu, map_with_bu, ncol = 2)
  
  # save comparative_map here
  
  # create a map showing the increase in accessibility
  # still very bad
  
  map_difference <- map_accessibility(accessibility_difference, breaks = breaks, labels = labels, title = "Accessibility difference")
  
  # save map_difference

  # boxplot for income quantile analysis ------------------------------------
  
  boxplot_with_bu <- quantile_boxplot(access_dist_with_bu, title = "Accessibility with BU")
  boxplot_without_bu <- quantile_boxplot(access_dist_without_bu, title = "Accessibility without BU")

  # theil index -------------------------------------------------------------

  
  
}

map_accessibility <- function(accessibility, breaks, labels, title) {
  
  rio_border <- accessibility %>% st_union()
  
  tm_shape(accessibility) +
    tm_fill(col = "accessibility",
            title = "  Accessibility",
            breaks = breaks,
            labels = labels, 
            style = "cont",
            palette = "viridis",
            legend.is.portrait = FALSE) +
  tm_shape(rio_border) +
    tm_borders(col = "gray20") +
  tm_layout(title = title,
            title.position = c("center", "top"),
            inner.margins = c(0.225, 0.02, 0.2, 0.02),
            legend.position = c("right", "bottom"))
  
}

accessibility_distribution <- function(accessibility, grid_data, n = 10) {
  
  # join income and population data to the accessibility dataframe, arrange by income and expand each row n times
  # where n equals the population with the given income
  
  distribution <- accessibility %>% 
    st_drop_geometry() %>% 
    left_join(grid_data, by = "id") %>% 
    mutate(
      avg_income = total_income / population,
      population = round(population)
    ) %>% 
    select(-total_income, -id) %>% 
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

quantile_boxplot <- function(access_dist, title) {
  
  # palma ratio is the accessibility sum of the richest 10% over the accessibility sum of the poorest 40%
  
  richest_10 <- access_dist %>% 
    filter(income_quantile == "Q10") %>% 
    summarise(accessibility_sum = sum(accessibility))
  
  poorest_40 <- access_dist %>% 
    filter(income_quantile %in% c("Q1", "Q2", "Q3", "Q4")) %>% 
    summarise(accessibility_sum = sum(accessibility))
  
  palma_ratio = richest_10$accessibility_sum / poorest_40$accessibility_sum
  
  ggplot(access_dist) +
    geom_boxplot(aes(income_quantile, accessibility)) +
    ggtitle(label = title, subtitle = stringr::str_c("Palma ratio: ", round(palma_ratio, digits = 4))) +
    xlab("Income decile") +
    ylab("Accessibility") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
}

theil_analysis <- function(access_dist) {
  
  
  
}

theil_index <- function(access_dist) {
  
  accessibility_total <- sum(access_dist$accessibility)
  
  access_dist <- access_dist %>% 
    mutate(
      theil_share = (accessibility / accessibility_total) * log((accessibility / accessibility_total) / (1 / nrow(access_dist)))
    )
  
  index <- sum(access_dist$theil_share)
  
  index
  
}