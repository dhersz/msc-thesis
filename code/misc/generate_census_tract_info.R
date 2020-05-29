generate_census_tract_info <- function(muni = 3304557) {
  
  # doesn't have all census tracts, not sure why
  ibge_info <- read_csv("./data/info_setores_RJ_IBGE.csv")
  
  establishments_info <- read_csv("./data/estabelecimentos.csv")
  
  census_tract_info <- read_census_tract(muni) %>% 
    select(code_tract) %>% 
    mutate(code_tract = as.numeric(as.character(code_tract))) %>% 
    left_join(establishments_info, by = c("code_tract" = "CD_GEOCODI")) %>% 
    rename(establishments = Total) %>% 
    left_join(ibge_info, by = c("code_tract" = "cod_setor")) %>% 
    rename(population = pop, total_monthly_earnings = renda_mensal_total) %>% 
    mutate(
      establishments = ifelse(is.na(establishments), 0, establishments),
      population = ifelse(is.na(population), 0, population),
      total_monthly_earnings = as.numeric(ifelse(total_monthly_earnings == "X" | is.na(total_monthly_earnings) | population == 0, 0, total_monthly_earnings))
    )
  
  census_tract_info
  
}