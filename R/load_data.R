
# read data ------------------------------------------------
path_all <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE, pattern = ".csv"))

# path_all[path %like% "raw_data", data_type := "raw_data"]
# path_all[path %like% "clean", data_type := "clean"]

dt <- rbind(fread(path_all[path %like% "pm25"]$path ) %>% setnames("PM25","value") %>% .[, parameter := "pm25_hourly"],
            fread(path_all[path %like% "psi"]$path ) ) %>%
  setnames("timestamp","datetime") %>%   
  .[, datetime := ymd_hms(datetime)]

# dt[,c("location","longitude","latitude")] %>% unique
# to remove duplicates (2016-03-16)
dt <- dt[,.(value = mean(value, na.rm = T)), by = .(datetime, location, longitude, latitude, parameter)]

# this "national" means that the max value among the five stations
dt <- rbind(dt, dt [parameter == "pm25_hourly", .(value = max(value, na.rm = T),
                                                  longitude = 0,
                                                  latitude = 0,
                                                  location = "national"), by = .(datetime, parameter)] ) 

dt <- rbind(dt, dt [location != "national", .(value = mean(value, na.rm = T),
                                              longitude = 0,
                                              latitude = 0,
                                              location = "national_mean"), by = .(datetime, parameter)] ) %>%  setorder(datetime)

# add conditions---------------------------------------------------------------------

cb_phase <- ymd("2020-04-07") %--% ymd_hms("2020-05-11 23:59:59") 

dt[datetime %within% cb_phase, phase := "cb"]

# cb_length <- dt[phase=="cb", .(cb_length = as.duration( date(datetime[.N]) - date(datetime[1]) ) + ddays(1))]
# before_cb <- (ymd("2020-04-07") - cb_length$cb_length) %--% ymd("2020-04-07")
# dt[datetime %within% before_cb, phase := "before_cb"]
# Add columns for year, yday and month
dt [, year := year(datetime)]
dt [, yday := yday(datetime)]
dt [, month := month(datetime, label = T)]

before_cb <- (ymd("2020-04-07") - 14) %--% ymd("2020-04-07")
dt[datetime %within% before_cb, phase := "before_cb"]

same_period <- dt[phase == "cb"]$yday %>% unique

dt[yday %in% same_period & is.na(phase), phase := "before"]

dt [, parameter_ori :=  parameter]
dt [, parameter := factor(parameter_ori,
                                levels = c("psi_twenty_four_hourly",
                                           "pm10_twenty_four_hourly",
                                           "pm25_twenty_four_hourly",
                                           "pm25_hourly",
                                           
                                           "no2_one_hour_max",
                                           "co_eight_hour_max",
                                           "so2_twenty_four_hourly",
                                           "o3_eight_hour_max",
                                           "psi_three_hourly")
)]


dt[, parameter_fct := factor(parameter_ori,
                             levels = c("psi_twenty_four_hourly",
                                        "pm10_twenty_four_hourly",
                                        "pm25_twenty_four_hourly",
                                        "pm25_hourly",
                                        
                                        "no2_one_hour_max",
                                        "co_eight_hour_max",
                                        "so2_twenty_four_hourly",
                                        "o3_eight_hour_max",
                                        "psi_three_hourly"),
                             labels = c(
                               bquote( 24-hr~PSI~index ),
                               bquote( 24-hr~PM[10]~(mu*g/m^3) ),
                               bquote( 24-hr~PM[2.5]~(mu*g/m^3) ),
                               bquote( 1-hr~PM[2.5]~(mu*g/m^3) ),
                               
                               bquote( 1-hr~NO[2]~(mu*g/m^3) ),
                               bquote( 8-hr~CO~(mg/m^3) ),
                               bquote( 24-hr~SO[2]~(mu*g/m^3) ),
                               bquote( 8-hr~O[3]~(mu*g/m^3) ),
                               bquote( 3-hr~PSI~index ) )
)]

dt [, location := factor(location,
                          levels = c("national", "central", "east", "west", "south", "north", "national_mean")
)]

# remove the redundant data----------------

dt_24h <- dt[hour(datetime) == 0 & parameter %in% c("pm10_twenty_four_hourly","pm25_twenty_four_hourly","so2_twenty_four_hourly","psi_twenty_four_hourly")]
dt_24h[, datetime := datetime - days(1)]

dt_8h <- dt[hour(datetime) %in% c(0,8,16) & parameter %in% c("co_eight_hour_max","o3_eight_hour_max")]
dt_8h[, datetime := datetime - hours(8)]

dt_1h <- rbind(dt[ parameter %in% c("no2_one_hour_max")], dt[ parameter %in% c("pm25_hourly")])

dt_all <- rbind(dt_24h, dt_8h, dt_1h)

# daily data old version-----------------
# dt_daily_24h <- dt[hour(datetime) == 0 & parameter %in% c("pm10_twenty_four_hourly","pm25_twenty_four_hourly","so2_twenty_four_hourly","psi_twenty_four_hourly")] %>% 
#   .[,.(value = mean(value, na.rm = T) ), by = .(date = date(datetime - days(1)), location, parameter)]
# 
# dt_daily_3h <- dt[hour(datetime) %in% c(0,8,16) & parameter %in% c("co_eight_hour_max","o3_eight_hour_max")]
# 
# dt_daily_3h[hour(datetime) == 0, date := date(datetime - days(1))]
# dt_daily_3h[hour(datetime) %in% c(8,16), date := date(datetime)]
# dt_daily_3h_ <- dt_daily_3h %>% 
#   .[, .(value = max(value, na.rm = T) ), by = .(date, location, parameter)]
# 
# dt_daily_1h_1 <- dt[ parameter %in% c("no2_one_hour_max")] %>% 
#   .[, .(value = max(value, na.rm = T) ), by = .(date = date(datetime), location, parameter)]
# dt_daily_1h_2 <- dt[ parameter %in% c("pm25_hourly")] %>% 
#   .[, .(value = mean(value, na.rm = T) ), by = .(date = date(datetime), location, parameter)]
# 
# dt_daily <- rbind(dt_daily_1h_1, dt_daily_1h_2, dt_daily_3h_, dt_daily_24h, use.names=TRUE) 
# 
# dt_daily[, datetime := date]
# 
# dt_daily_no_national <-  dt_daily[!location %in% c("national_mean", "national")]  # 去掉national以后重新算
# 
# dt_daily_1 <- dt_daily_no_national[parameter %in% c("pm10_twenty_four_hourly","pm25_twenty_four_hourly","so2_twenty_four_hourly","psi_twenty_four_hourly","pm25_hourly"),
#          .(value = mean(value, na.rm = T) ), by =.(datetime, date, parameter)] [, location := "national"]
# 
# dt_daily_2 <- dt_daily_no_national[parameter %in% c("no2_one_hour_max","co_eight_hour_max","o3_eight_hour_max"),
#                        .(value = max(value, na.rm = T) ), by =.(datetime, date, parameter)] [, location := "national"]
# 
# dt_daily <- rbind(dt_daily_no_national, dt_daily_1, dt_daily_2)

# daily data new version-----------------
dt_daily_24h <- dt[hour(datetime) == 0 & parameter %in% c("pm10_twenty_four_hourly","pm25_twenty_four_hourly","so2_twenty_four_hourly","psi_twenty_four_hourly")] %>% 
  .[,.(value = mean(value, na.rm = T) ), by = .(date = date(datetime - days(1)), location, parameter)]

dt_daily_3h <- dt[hour(datetime) %in% c(0,8,16) & parameter %in% c("co_eight_hour_max","o3_eight_hour_max")]

dt_daily_3h[hour(datetime) == 0, date := date(datetime - days(1))]
dt_daily_3h[hour(datetime) %in% c(8,16), date := date(datetime)]

dt_daily_3h_ <- dt_daily_3h %>% 
  .[, .(value = mean(value, na.rm = T) ), by = .(date, location, parameter)]

dt_daily_1h <- dt[ parameter %in% c("no2_one_hour_max","pm25_hourly")] %>% 
  .[, .(value = mean(value, na.rm = T) ), by = .(date = date(datetime), location, parameter)]

dt_daily <- rbind(dt_daily_1h, dt_daily_3h_, dt_daily_24h, use.names=TRUE) 

dt_daily[, datetime := date]

dt_daily_no_national <-  dt_daily[!location %in% c("national_mean", "national")]  # 去掉national以后重新算

dt_daily_national <- dt_daily_no_national[,.(value = mean(value, na.rm = T) ), by =.(datetime, date, parameter)] [, location := "national"]

dt_daily <- rbind(dt_daily_no_national, dt_daily_national)

# add conditions---------------------------------------------------------------------

dt_daily[datetime %within% cb_phase, phase := "cb"]

dt_daily[datetime %within% before_cb, phase := "before_cb"]


# Add columns for year, yday and month
dt_daily [, year := year(datetime)]
dt_daily [, yday := yday(datetime)]
dt_daily [, month := month(datetime, label = T)]


dt_daily [yday %in% same_period & is.na(phase), phase := "before"]

dt_daily [, parameter_fct := factor(parameter,
                             levels = c("psi_twenty_four_hourly",
                                        "pm10_twenty_four_hourly",
                                        "pm25_twenty_four_hourly",
                                        "pm25_hourly",

                                        "no2_one_hour_max",
                                        "co_eight_hour_max",
                                        "so2_twenty_four_hourly",
                                        "o3_eight_hour_max",
                                        "psi_three_hourly"),
                             labels = c(
                               bquote( 24-hr~PSI~index ),
                               bquote( 24-hr~PM[10]~(mu*g/m^3) ),
                               bquote( 24-hr~PM[2.5]~(mu*g/m^3) ),
                               bquote( 1-hr~PM[2.5]~(mu*g/m^3) ),

                               bquote( 1-hr~NO[2]~(mu*g/m^3) ),
                               bquote( 8-hr~CO~(mg/m^3) ),
                               bquote( 24-hr~SO[2]~(mu*g/m^3) ),
                               bquote( 8-hr~O[3]~(mu*g/m^3) ),
                               bquote( 3-hr~PSI~index ) )
)]

dt_daily [, parameter := factor(parameter,
                                    levels = c("psi_twenty_four_hourly",
                                               "pm10_twenty_four_hourly",
                                               "pm25_twenty_four_hourly",
                                               "pm25_hourly",
                                               
                                               "no2_one_hour_max",
                                               "co_eight_hour_max",
                                               "so2_twenty_four_hourly",
                                               "o3_eight_hour_max",
                                               "psi_three_hourly")
)]


# load mobility data------------------
dt_mobi_g <- fread(path_all[path %like% "Global_Mobility_Report.csv"]$path ) [country_region == "Singapore"] [, c(5:11)] %>% 
  melt(id.vars = "date", variable.name = "places", value.name = "percent_change_from_baseline")

dt_mobi_g[, places := str_extract(places, ".*(?=_percent_change)")] 
dt_mobi_g[, date := as_date(date)]
dt_mobi_g[, mobi_value := (100 + percent_change_from_baseline)/100]
dt_mobi_g[, percent_change_from_baseline := NULL]
setnames(dt_mobi_g, "places", "item")
dt_mobi_g[, dataset_source := "Google"]

dt_mobi_a <- fread(path_all[path %like% "applemobilitytrends"]$path )[region == "Singapore"][,c("geo_type","region","alternative_name"):=NULL] %>%
  melt(id.vars = "transportation_type", variable.name = "date", value.name = "mobi_value")
dt_mobi_a[, date := as_date(date)] 
dt_mobi_a[, mobi_value := mobi_value/100] 
setnames(dt_mobi_a, "transportation_type", "item")
dt_mobi_a[, dataset_source := "Apple"]



# dt_mobi[, item := factor(item, level = c("driving",
#                                         "transit", 
#                                         "workplaces",  
#                                         "transit_stations", 
#                                         "residential") )]


# read car park data-----------------------------
car_park_dt <- fread(path_all[path %like% "carpark-availability.csv"]$path) %>% setnames("timestamp", "datetime") %>%
    .[, datetime := ymd_hms(datetime)] 
# %>%    .[, .(mean_perc_available = mean(perc_available)), by = .(date(datetime))] %>% ggplot() +
#   geom_line( aes(date, mean_perc_available)) 
car_park_dt_daily <- car_park_dt [, .(mobi_value = mean(perc_available, na.rm = T) ), by = date(datetime)] [, `:=`(item = "car_park_availability",
                                                                                              dataset_source = "HDB")]
car_park_dt_daily <- car_park_dt [hour(datetime) %in% c(9:19), .(mobi_value = mean(perc_available, na.rm = T) ), by = date(datetime)] [, `:=`(item = "car_park_availability",
                                                                                                                   dataset_source = "HDB")]
car_park_dt_daily[yday(date) %in% same_period, .(mobi_value = mean(mobi_value, na.rm = T))]


dt_mobi <- rbind(dt_mobi_g, dt_mobi_a, car_park_dt_daily) [item %in% c("driving",
                                                                       # "transit", 
                                                                       "workplaces",  
                                                                       "transit_stations", 
                                                                       "residential",
                                                                       "car_park_availability") ]

# read weather data-----------------------------
dt_weather <- map(path_all[path %like% "weather"]$path, fread) %>%  rbindlist  %>%  setnames("timestamp","datetime") %>%
  .[, datetime := ymd_hms(datetime)] %>%  setorder(datetime)


dt_weather [, year := year(datetime)]
# dt_weather [, yday := yday(datetime)]
dt_weather [, month := month(datetime, label = T)]

dt_weather_national <- dt_weather[, .(Tmp = mean(Tmp, na.rm = T),
                                  RH= mean(RH, na.rm = T),
                                  Rainfall = mean(Rainfall, na.rm = T),
                                  WindDirection = mean(WindDirection, na.rm = T),
                                  WindSpeed = mean(WindSpeed, na.rm = T)), by = .(datetime, year, month)]


dt_weather_hour <- dt_weather[, .(RH= mean(RH, na.rm = T),
               Rainfall = sum(Rainfall, na.rm = T),
               WindDirection = mean(WindDirection, na.rm = T),
               WindSpeed = mean(WindSpeed, na.rm = T)), by = floor_date(datetime, "hours")]

