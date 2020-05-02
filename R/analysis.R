source("./R/functions.R")

# All .csv paths -----------------------------------------------
path_all <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE, pattern = ".csv"))

# path_all[path %like% "raw_data", data_type := "raw_data"]
# path_all[path %like% "clean", data_type := "clean"]

pm25 <- fread(path_all[path %like% "pm25"]$path ) %>% setnames("timestamp","datetime") %>%   .[, datetime := ymd_hms(datetime)]
psi <- fread(path_all[path %like% "psi"]$path ) %>% setnames("timestamp","datetime") %>%   .[, datetime := ymd_hms(datetime)]   


pm25 %>%
ggplot() +
geom_line(aes(datetime, PM25, color = location))


psi$parameter %>% unique

psi[parameter == "no2_one_hour_max"] %>%
  ggplot() +
  geom_line(aes(datetime, value, color = location))

psi[parameter == "psi_three_hourly"] %>%
  ggplot() +
  geom_line(aes(datetime, value, color = location))

psi[parameter == "o3_eight_hour_max"] %>%
  ggplot() +
  geom_line(aes(datetime, value, color = location))

  