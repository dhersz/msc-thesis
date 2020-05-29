source("./code/setup.R")
source("./code/setup_local.R")

distinct_df <- function() { 
  total <- vector("integer", length = 8L)
  unique <- vector("integer", length = 8L)
  
  for (i in c(3:10)) {
    itineraries <- readr::read_csv(str_c("./data/rio_1000_legs_", i, "_itineraries.csv"))
    
    t <- itin_count_unique(itineraries)
    
    total[i - 2] = sum(t$total_itin)
    unique[i - 2] = sum(t$unique_itin)
  }
  
  f <- tibble(num_itineraries = 3:10, supposed = 3:10 * 1000,
                      total = total, unique = unique) %>% 
    pivot_longer(
      cols = supposed:unique,
      names_to = "count_type",
      values_to = "count"
    )
  
  f
}

p <- ggplot(distinct_df(), aes(num_itineraries, count, colour = count_type)) +
  geom_line() +
  geom_point() +
  scale_x_continuous("numItineraries parameter passed in request", breaks = c(3:10)) +
  scale_y_continuous("itineraries count") +
  scale_color_discrete(
    name = "legend",
    breaks = c("supposed", "total", "unique"),
    labels = c("max (n*1000)", "total", "unique")
  )

print(p)