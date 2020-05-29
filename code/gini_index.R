gini_index <- function(accessibility, aggregated_info) {
  
  aggregated_pop <- aggregated_info %>% select(id, population)
  
  dist <- accessibility %>% 
    left_join(aggregated_pop, by = "id") %>% 
    mutate(population = round(population)) %>% 
    filter(population > 0) %>% 
    arrange(accessibility) %>% 
    uncount(population) %>% 
    select(-id) %>% 
    rowid_to_column("id") %>% 
    mutate(
      lorenz_curve = cumsum(accessibility) / sum(accessibility),
      perfect_equity_line = id / max(id)
    )
  
  n <- nrow(dist)
  
  gini <- sum(dist$lorenz_curve[-1] * dist$perfect_equity_line[-n]) - sum(dist$lorenz_curve[-n] * dist$perfect_equity_line[-1])
  
  gini
  
}