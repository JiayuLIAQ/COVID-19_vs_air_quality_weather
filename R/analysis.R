source("./R/functions.R")

# read data -----------------------------------------------
path_all <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE, pattern = ".csv"))

# path_all[path %like% "raw_data", data_type := "raw_data"]
# path_all[path %like% "clean", data_type := "clean"]

dt <- rbind(fread(path_all[path %like% "pm25"]$path ) %>% setnames("PM25","value") %>% .[, parameter := "pm25_hourly"],
                    fread(path_all[path %like% "psi"]$path ) ) %>%
  setnames("timestamp","datetime") %>%   
  .[, datetime := ymd_hms(datetime)] %>%
  setorder(datetime)

# add conditions---------------------------------------------------------------------

cb_phase <- ymd("2020-04-07") %--% ymd("2020-06-01") 

dt[datetime %within% cb_phase, phase := "cb"]

cb_length <- dt[phase=="cb", .(cb_length = as.duration( date(datetime[.N]) - date(datetime[1]) ) + ddays(1))]

# cb_days <- as.numeric(cb_length) /60/60/24

before_cb <- (ymd("2020-04-07") - cb_length$cb_length) %--% ymd("2020-04-07")

dt[datetime %within% before_cb, phase := "before_cb"]

# analysis --------------------------------------------------------------------------
dt[!is.na(phase) & !parameter %like% "index", .(value = mean(value, na.rm = T) ), by = .(parameter, phase)] %>% 
  ggplot() +
  geom_line(aes(phase, value, color = parameter, group = parameter)) +
  facet_wrap(vars(parameter))

compare_table <- dt[!is.na(phase) & !parameter %like% "index", .(value = mean(value, na.rm = T) ), by = .(parameter, phase)] %>% 
  dcast(parameter~phase) %>% .[, change_prop := (cb-before_cb)/before_cb * 100] 

dt [!is.na(phase) & !parameter %like% "index"] %>% 
  ggplot (aes(phase, value, fill = phase)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom= "point", shape= 23, size= 2 , 
               fill = "white", position = position_dodge(width = 0.75)) +
  facet_wrap(vars(parameter_fct) , scales = "free", labeller=label_parsed) +
  mytheme_basic +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank())

dt[, parameter_fct := factor(parameter,
                             levels = c("pm10_twenty_four_hourly",  
                                        "pm25_twenty_four_hourly",
                                        "co_eight_hour_max", 
                                        "so2_twenty_four_hourly", 
                                        "no2_one_hour_max",         
                                        "psi_twenty_four_hourly", 
                                        "o3_eight_hour_max", 
                                        "psi_three_hourly", 
                                        "pm25_hourly"),
                             labels = c(bquote(PM[10]~(mu*g/m^3)),  
                                        "PM[2.5]",
                                        "co_eight_hour_max", 
                                        "so2_twenty_four_hourly", 
                                        "no2_one_hour_max",         
                                        "psi_twenty_four_hourly", 
                                        "o3_eight_hour_max", 
                                        "psi_three_hourly", 
                                        "pm25_hourly") )]


dt$parameter %>% unique
dt$location %>% unique

pm25 %>%
ggplot() +
geom_line(aes(datetime, PM25, color = location))



psi[parameter == "no2_one_hour_max" & year(datetime) == "2020"] %>%
  ggplot() +
  geom_line(aes(datetime, value, color = location))

psi[parameter == "pm10_twenty_four_hourly"] %>%
  ggplot() +
  geom_line(aes(datetime, value, color = location))


psi[parameter == "psi_three_hourly"] %>%
  ggplot() +
  geom_line(aes(datetime, value, color = location))

psi[parameter == "o3_eight_hour_max"] %>%
  ggplot() +
  geom_line(aes(datetime, value, color = location))

  