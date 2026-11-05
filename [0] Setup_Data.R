library(tidyverse)
library(MMWRweek)
library(epidatr)
library(RSocrata)


#pneumonia and influenza deaths obtained from: https://data.cdc.gov/api/v3/views/pp7x-dyj2/
pneumonia_deaths <- readRDS("Data/pneumonia_deaths.rds") 


#CDc Flu-Survnet data, accessed via CMU Delphu Epiportal 11/4/2026
#library(epidatr)
#epidata_flusurv_flusurv_site_v5 <- epidata_snapshot(
#    source = "flusurv",
#    signals = c("rate_overall"),
#        reference_time = epirange("2010-01-01", "2020-12-31"),
#    geo_type = "nation"
# )
#saveRDS(epidata_flusurv_flusurv_site_v5,'./Data/flusurvnet_raw.rds')

flu_raw <- readRDS('./Data/flusurvnet_raw.rds')%>% 
    rename(date=reference_time,
    rate_overall = value) %>%
    dplyr::select(date, rate_overall) %>%
    arrange(date) %>%
    mutate(date=date-6)

  # flusurv only reports during the flu season; fill spring/summer gaps with 0
  flu <- tibble(date = seq(min(flu_raw$date), max(flu_raw$date), by = "1 week")) %>%
    left_join(flu_raw, by = "date") %>%
    mutate(rate_overall = tidyr::replace_na(rate_overall, 0))

  combined <- pneumonia_deaths %>%
    inner_join(flu, by = "date") %>%
    arrange(date)

saveRDS(combined, './Data/flu_pneumonia_weekly.rds')
