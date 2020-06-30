library(dplyr)
library(sf)
library(tmap)
library(ggplot2)

analyse_results <- function(travel_time = 60, n_quantiles = 10, res = 7, lang = "pt", minimum_wage = 1045) {
  
  # classify each hexagon according to its residents' avg income per capita quantile
  # not sure why, but the values returned by wtd.quantile() seem to have some kind of issue with their rounding
  # when trying to classify each hexagon into a income quantile the values right at the boundaries of each quantile interval are not
  # consistently accounted (sometimes it behaves like a open-close limit, sometimes as a close-open, etc)
  # in order to fix this and to treat the intervals consistently as open-close like quantile() does (with the exception of the first one
  # being close-close) the 0% value is set to -Inf e the avg_income - 0.0001 is used to place each entrance in a quantile (i.e. if the
  # Q2 interval is 10-20 and we're trying to classify in which decile the 10 is in we use the values 9.9999 and 19.9999, therefore the
  # 10 is in Q1 and 20 is in Q2)
  
  grid_data <- readr::read_rds(stringr::str_c("./data/rio_h3_grid_res_", res, "_with_data.rds")) %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    mutate(avg_income = total_income / population)
    
  qntls <- Hmisc::wtd.quantile(grid_data$avg_income, weights = grid_data$population, probs = seq(0, 1, 1/n_quantiles))
  qntls[1] <- -Inf

  grid_data <- grid_data %>% 
    mutate(
      income_quantile = cut(avg_income - 0.0001, qntls, labels = stringr::str_c("Q", 1:n_quantiles), include.lowest = TRUE)
    )
  
  # first analysis: effect of incorporating monetary costs
  # create visualisations comparing accessibility at different monetary costs thresholds and without a
  # monetary cost threshold
  
  travel_time <- travel_time
  percentage_minimum_wage <- c(20, 30, 40, 1000)
  
  text_labels <- labels_different_costs(travel_time, percentage_minimum_wage, lang)
  maps_different_costs(grid_data, travel_time, percentage_minimum_wage, text_labels, res)
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

labels_different_costs <- function(travel_time, percentage_minimum_wage, lang) {
  
  # create the text labels used in the maps and graphics according to the specified language
  
  if (lang == "pt") {
  
    text_labels <- list(
      "maps" = list(
        "facets_title" = stringr::str_c("Custo monetário ", ifelse(percentage_minimum_wage <= 100, stringr::str_c("<= ", percentage_minimum_wage, "% do salário mínimo"), "não considerado")),
        "legend_title" = "  Empregos acessíveis (% do total)"
      )
    )
    
    
  }
  
  text_labels
  
}

maps_different_costs <- function(grid_data, travel_time, percentage_minimum_wage, text_labels, res) {
  
  # read data
  
  rj_state <- geobr::read_municipality(code_muni = "RJ")
  
  accessibility_data <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(stringr::str_c("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds")))
  
  # set breaks and labels
  
  max_accessibility <- max(purrr::map_dbl(accessibility_data, function(i) max(i$accessibility)))
  total_opportunities <- sum(grid_data$opportunities)
  proportion <- max_accessibility / total_opportunities
  
  n <- 5
  breaks <- seq(0, max_accessibility, length.out = n)
  labels <- format(round(seq(0, proportion, length.out = n) * 100, digits = 1), nsmall = 1)
  labels[c(2, 4)] <- ""
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
                                  title = text_labels$maps$facets_title[i], title.size = 0.85, legend.show = show_legends[i],
                                  legend.title = text_labels$maps$legend_title)
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

map_accessibility <- function(accessibility, grid_data, rj_state, breaks, labels, title, title.size, legend.show, legend.title) {
  
  # map settings
  
  rio_border <- accessibility %>% summarise()
  
  tm_shape(rj_state, bbox = st_bbox(rio_border)) +
    tm_fill(col = "#efeeec") +
    
  tm_shape(accessibility) +
    tm_fill(col = "accessibility",
            title = legend.title,
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
            legend.just = c("right", "bottom"),
            legend.width = -0.52,
            legend.hist.width = -1,
            legend.bg.color = "#aadaff",
            legend.title.size = 0.8,
            legend.text.size = 0.65,
            legend.show = legend.show)
  
}

boxplot_different_costs <- function(grid_data, travel_time, percentage_minimum_wage, res) {
  
  # read and prepare data 
  
  accessibility_data <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(stringr::str_c("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds"))) %>%
    purrr::map(st_drop_geometry) %>% 
    purrr::map(function(i) left_join(i, grid_data, by = "id")) %>% 
    purrr::map(function(i) filter(i, population > 0))
  
  # calculate the palma ratio in each case
  
  ratios <- accessibility_data %>% 
    purrr::map_dbl(palma_ratio)
  
  # bind each distribution to the same dataframe to plot it as a facet of the same plot
  # set names that can be used as the facet identifier
  
  accessibility_data <- accessibility_data %>% 
    purrr::set_names(as.character(percentage_minimum_wage)) %>%
    bind_rows(.id = "percentage_minimum_wage")
  
  # convert names to factors so the facets are adequately ordered 
  
  n <- length(percentage_minimum_wage)
  
  accessibility_data$percentage_minimum_wage <- factor(
    accessibility_data$percentage_minimum_wage,
    levels = percentage_minimum_wage,
    labels = c(stringr::str_c("Cost <= ", percentage_minimum_wage[1:(n-1)], "% of minimum wage"), "No cost considered")
  )
  
  # create a dataframe with the palma ratios to plot them as annotations
  
  palma_data <- tibble::tibble(
    percentage_minimum_wage = factor(percentage_minimum_wage,
                                     levels = percentage_minimum_wage,
                                     labels = c(stringr::str_c("Cost <= ", percentage_minimum_wage[1:(n-1)], "% of minimum wage"), "No cost considered")),
    x = 0.625,
    y = max(accessibility_data$accessibility),
    label = stringr::str_c("Palma ratio: ", format(round(ratios, digits = 4), nsmall = 4))
  )
  
  # plot settings
  
  ggplot(accessibility_data, aes(income_quantile, accessibility)) +
    geom_boxplot(aes(weight = population)) +
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

palma_ratio <- function(accessibility_data) {
  
  richest_10 <- accessibility_data %>% 
    filter(income_quantile == "Q10") %>% 
    summarise(total_accessibility = sum(accessibility * population))
  
  poorest_40 <- accessibility_data %>% 
    filter(income_quantile %in% c("Q1", "Q2", "Q3", "Q4")) %>% 
    summarise(total_accessibility = sum(accessibility * population))
  
  palma_ratio <- richest_10$total_accessibility / poorest_40$total_accessibility
  
  palma_ratio
  
}

theil_different_costs <- function(grid_data, travel_time, percentage_minimum_wage, res) {
  
  # read and prepare data
  
  accessibility_data <- purrr::map(percentage_minimum_wage, function(i) readr::read_rds(stringr::str_c("./results/with_bu_tt_", travel_time, "_mc_", i, "_res_", res, ".rds"))) %>%
    purrr::map(st_drop_geometry) %>% 
    purrr::map(function(i) left_join(i, grid_data, by = "id")) %>% 
    purrr::map(function(i) filter(i, population > 0))
  
  # calculate the theil index and its components in each case
  # set names for each observation and bind them together in the same dataframe
  
  theil_data <- accessibility_data %>% 
    purrr::map(theil_info)%>%
    purrr::set_names(as.character(percentage_minimum_wage)) %>%
    bind_rows(.id = "percentage_minimum_wage")
  
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

theil_info <- function(accessibility_data) {
  
  # create a population column with 1s to use theil_index() both when grouped or when ungrouped
  
  accessibility_data <- accessibility_data %>%
    arrange(income_quantile) %>%
    mutate(
      total_accessibility = accessibility * population
    )
  
  # calculate the between-group component
  # group by quantile, calculate its share of the component and save it in a list
  
  between_group_data <- accessibility_data %>% 
    group_by(income_quantile) %>% 
    summarise(
      population = sum(population),
      total_accessibility = sum(total_accessibility)
    ) %>% 
    ungroup() %>% 
    mutate(
      theil_share = (total_accessibility / sum(total_accessibility)) * log((total_accessibility / sum(total_accessibility)) / (population / sum(population)))
    )
  
  unique_quantiles <- unique(accessibility_data$income_quantile)
  n <- length(unique_quantiles)
  
  between_group <- list(component = rep("between", n), income_quantile = unique_quantiles, share = between_group_data$theil_share)
  
  # calculate the within-group component
  # it is the weighted average of each group's own theil index, where accessibility is the weight
  
  within_group_share <- vector("double", length = n)
  
  for (i in seq_along(unique_quantiles)) {
    
    filtered_data <- accessibility_data %>% 
      filter(income_quantile == unique_quantiles[i])
    
    within_group_share[i] <- theil_index(filtered_data) * sum(filtered_data$total_accessibility)
    
  }
  
  within_group_share <- within_group_share / sum(accessibility_data$total_accessibility)
  
  within_group <- list(component = rep("within", n), income_quantile = unique_quantiles, share = within_group_share)
  
  # bind between- and within-group components lists together in a dataframe
  
  info <- bind_rows(within_group, between_group)
  
  # convert component column to factors so the plots later generated are adequately ordered
  
  info$component <- factor(info$component, levels = c("between", "within"), labels = c("Between-group", "Within-group"))
  
  info
  
}

theil_index <- function(accessibility_data) {
  
  accessibility_data <- accessibility_data %>% 
    mutate(
      total_accessibility = accessibility * population,
      theil_share = (total_accessibility / sum(total_accessibility)) * log((total_accessibility / sum(total_accessibility)) / (population / sum(population)))
    )
  
  index <- sum(accessibility_data$theil_share)
  
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
  
  ggsave(stringr::str_c("./analysis/theil_components_stacked_tt_", travel_time, "_res_", res, "_test.png"),
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
  
  ggsave(stringr::str_c("./analysis/between_group_share_tt_", travel_time, "_res_", res, "_test.png"),
         width = 7,
         height = 8,
         units = "in")
  
}
