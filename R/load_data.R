
# read data ------------------------------------------------
path_all <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE, pattern = ".csv"))

# path_all[path %like% "raw_data", data_type := "raw_data"]
# path_all[path %like% "clean", data_type := "clean"]

dt <- rbind(fread(path_all[path %like% "pm25"]$path ) %>% setnames("PM25","value") %>% .[, parameter := "pm25_hourly"],
            fread(path_all[path %like% "psi"]$path ) ) %>%
  setnames("timestamp","datetime") %>%   
  .[, datetime := ymd_hms(datetime)]

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

# dt_weather <- fread(path_all[path %like% "weather"]$path )  %>%  setnames("timestamp","datetime") %>%   
#   .[, datetime := ymd_hms(datetime)] %>%  setorder(datetime)
# 
# test <- dt_weather[,.(datetime)] %>% unique

# add conditions---------------------------------------------------------------------

cb_phase <- ymd("2020-04-07") %--% ymd("2020-06-01") 

dt[datetime %within% cb_phase, phase := "cb"]

# cb_length <- dt[phase=="cb", .(cb_length = as.duration( date(datetime[.N]) - date(datetime[1]) ) + ddays(1))]
# before_cb <- (ymd("2020-04-07") - cb_length$cb_length) %--% ymd("2020-04-07")
# dt[datetime %within% before_cb, phase := "before_cb"]

before_cb <- (ymd("2020-04-07") - 14) %--% ymd("2020-04-07")
dt[datetime %within% before_cb, phase := "before_cb"]


# Add columns for year, yday and month
dt [, year := year(datetime)]
dt [, yday := yday(datetime)]
dt [, month := month(datetime, label = T)]

same_period <- dt[phase == "cb"]$yday %>% unique

dt[yday %in% same_period & is.na(phase), phase := "before"]

dt[, parameter_fct := factor(parameter,
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


# daily data-----------------
dt_daily_24h <- dt[hour(datetime) == 0 & parameter %in% c("pm10_twenty_four_hourly","pm25_twenty_four_hourly","so2_twenty_four_hourly","psi_twenty_four_hourly")] %>% 
  .[,.(value = mean(value, na.rm = T) ), by = .(date = date(datetime - days(1)), location, parameter)]

dt_daily_3h <- dt[hour(datetime) %in% c(0,8,16) & parameter %in% c("co_eight_hour_max","o3_eight_hour_max")]
# rm(dt_daily_3h)
dt_daily_3h[hour(datetime) == 0, date := date(datetime - days(1))]
dt_daily_3h[hour(datetime) %in% c(8,16), date := date(datetime)]
dt_daily_3h_ <- dt_daily_3h %>% 
  .[, .(value = max(value, na.rm = T) ), by = .(date, location, parameter)]

dt_daily_1h_1 <- dt[ parameter %in% c("no2_one_hour_max")] %>% 
  .[, .(value = max(value, na.rm = T) ), by = .(date = date(datetime), location, parameter)]
dt_daily_1h_2 <- dt[ parameter %in% c("pm25_hourly")] %>% 
  .[, .(value = mean(value, na.rm = T) ), by = .(date = date(datetime), location, parameter)]

dt_daily <- rbind(dt_daily_1h_1, dt_daily_1h_2, dt_daily_3h_, dt_daily_24h, use.names=TRUE) 


dt_daily[, datetime := date]

dt_daily_no_national <-  dt_daily[!location %in% c("national_mean", "national")]

dt_daily_1 <- dt_daily_no_national[parameter %in% c("pm10_twenty_four_hourly","pm25_twenty_four_hourly","so2_twenty_four_hourly","psi_twenty_four_hourly","pm25_hourly"),
         .(value = mean(value, na.rm = T) ), by =.(datetime, date, parameter)] [, location := "national"]

dt_daily_2 <- dt_daily_no_national[parameter %in% c("no2_one_hour_max","co_eight_hour_max","o3_eight_hour_max"),
                       .(value = max(value, na.rm = T) ), by =.(datetime, date, parameter)] [, location := "national"]

dt_daily <- rbind(dt_daily_no_national, dt_daily_1, dt_daily_2)

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
dt_mobi_g[, value_compare_to_baseline := (100 + percent_change_from_baseline)/100]

dt_mobi_a <- fread(path_all[path %like% "applemobilitytrends"]$path )[region == "Singapore"][,c("geo_type","region","alternative_name"):=NULL] %>%
  melt(id.vars = "transportation_type", variable.name = "date", value.name = "value_compare_to_baseline")
dt_mobi_a[, date := as_date(date)] 