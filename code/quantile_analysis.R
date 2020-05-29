quantile_analysis <- function(accessibility, aggregated_info, n = 10) {
  
  aggregated_info <- aggregated_info %>% 
    left_join(accessibility, by = "id") %>% 
    mutate(earnings_per_capita = total_monthly_earnings / population, population = round(population)) %>% 
    select(-c(id, establishments, total_monthly_earnings)) %>% 
    arrange(earnings_per_capita) %>% 
    uncount(population) %>% 
    rowid_to_column("id")
  
  quantile_size <- nrow(aggregated_info) / n
  
  qntl_levels <- str_c("q", 1:n)
  
  aggregated_info <- aggregated_info %>% 
    mutate(qntl = id %/% quantile_size + 1, qntl = ifelse(qntl == 11, 10, qntl), qntl = factor(str_c("q", qntl), levels =  qntl_levels))
  
  aggregated_info
  
}