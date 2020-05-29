library(ggplot2)

lorenz_curve <- function(accessibility, aggregated_info, gini, BU = TRUE) {
  
  aggregated_pop <- aggregated_info %>% select(id, population)
  
  lorenz_curve_df <- accessibility %>% 
    left_join(aggregated_pop, by = "id") %>% 
    mutate(population = round(population)) %>%
    filter(population > 0) %>% 
    arrange(accessibility) %>%  
    mutate(
      cumulative_population = cumsum(population),
      percent_cumulative_population = cumulative_population / sum(population),
      lorenz_curve = cumsum(accessibility * population) / sum(accessibility * population),
      perfect_equality_line = percent_cumulative_population
    ) %>% 
    select(percent_cumulative_population, lorenz_curve, perfect_equality_line)
  
  point_0 <- tibble(percent_cumulative_population = 0, lorenz_curve = 0, perfect_equality_line = 0)
  
  lorenz_curve_df <- bind_rows(point_0, lorenz_curve_df)
  
  gini_text <- str_c(ifelse(BU, "with BU \n", "without BU \n"), "gini: ", round(gini, digits = 4))
  
  ggplot(lorenz_curve_df) +
    geom_line(aes(percent_cumulative_population, lorenz_curve)) +
    geom_line(aes(percent_cumulative_population, perfect_equality_line)) +
    annotate("text", x = 0, y = 1, label = gini_text, hjust = "left", vjust = "top") +
    coord_fixed() +
    theme_gray()
  
}