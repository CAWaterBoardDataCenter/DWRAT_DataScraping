#Base URL
Base_URL = "https://www.ncei.noaa.gov/access/services/data/v1?dataset=daily-summaries="

#Station List from InputData/RR_PRMS_StationList (2023-09-05).xlsx
Station_List = read_xlsx("InputData/RR_PRMS_StationList (2023-09-05).xlsx", sheet = "StationList") %>%
  filter(`Observed and PRISM Station Guide` == "DOWNSIZER") %>%
  select(...5) %>% unlist() %>% as.vector() %>%
  paste0("&stations=", .) %>% unique() %>%
  paste0(collapse = "")
Station_List