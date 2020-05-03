source("./R/functions.R")

# read data -----------------------------------------------
path_all <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE, pattern = ".csv"))

# path_all[path %like% "raw_data", data_type := "raw_data"]
# path_all[path %like% "clean", data_type := "clean"]

dt <- rbind(fread(path_all[path %like% "pm25"]$path ) %>% setnames("PM25","value") %>% .[, parameter := "pm25_hourly"],
                    fread(path_all[path %like% "psi"]$path ) ) %>%
  setnames("timestamp","datetime") %>%   
  .[, datetime := ymd_hms(datetime)]

dt <- rbind(dt, dt [parameter == "pm25_hourly", .(value = mean(value, na.rm = T),
                                                  longitude = 0,
                                                  latitude = 0,
                                                  location = "national"), by = .(datetime, parameter)] ) %>%
      setorder(datetime)

# add conditions---------------------------------------------------------------------

cb_phase <- ymd("2020-04-07") %--% ymd("2020-06-01") 

dt[datetime %within% cb_phase, phase := "cb"]

cb_length <- dt[phase=="cb", .(cb_length = as.duration( date(datetime[.N]) - date(datetime[1]) ) + ddays(1))]

# cb_days <- as.numeric(cb_length) /60/60/24

before_cb <- (ymd("2020-04-07") - cb_length$cb_length) %--% ymd("2020-04-07")

dt[datetime %within% before_cb, phase := "before_cb"]

# Add columns for year, yday and month
dt [, year := year(datetime)]
dt [, yday := yday(datetime)]
dt [, month := month(datetime, label = T)]

same_period <- dt[phase == "cb"]$yday %>% unique

dt[is.na(phase), phase := "before"]

# analysis --------------------------------------------------------------------------
compare_table <- dt[!is.na(phase) & !parameter %like% "index", .(value = mean(value, na.rm = T) ), by = .(parameter, phase, location)] %>% 
  dcast(parameter+location~phase) %>% .[, change_prop := (cb-before_cb)/before_cb * 100] %>% setorder(location)

dt[, parameter_fct := factor(parameter,
                             levels = c(
                                        "psi_twenty_four_hourly", 
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

# dt [!is.na(phase) & !parameter %like% "index"] %>% 
#   ggplot (aes(phase, value, fill = phase)) +
#   geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
#   stat_summary(fun.y = mean, geom= "point", shape= 23, size= 2 , 
#                fill = "white", position = position_dodge(width = 0.75)) +
#   geom_signif(comparisons = list(c("before_cb", "cb")),
#               map_signif_level=TRUE) +
#   facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
#   
#   scale_fill_manual (name="Phase",
#                      labels= phase_names ,
#                      values = color_manual_phase) +
#   
#   mytheme_basic +
#   theme(axis.line.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.x=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank())
# 
# ggsave("plots/compare_before_and_during_cb.pdf", 
#        width = 10, height = 7, useDingbats=FALSE)

boxplot_par_compare_with_before_cb <- function(par){
dt [location == "national" & parameter == par & phase %in% c("before_cb","cb")] %>% 
  ggplot (aes(phase, value, fill = phase)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
  stat_summary(fun.y = mean, geom= "point", shape= 23, size= 2 , 
               fill = "white", position = position_dodge(width = 0.75)) +
  geom_signif(comparisons = list(c("before_cb", "cb")),
              map_signif_level=TRUE) +
  # facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +

  scale_fill_manual (name="Phase",
                     labels= phase_names ,
                     values = color_manual_phase) +
  mytheme_basic +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank())
}

p1 <- boxplot_par_compare_with_before_cb("psi_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PSI ) )) + guides(fill = FALSE)
p2 <- boxplot_par_compare_with_before_cb("pm10_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PM[10]~(mu*g/m^3) ) ))  + guides(fill = FALSE)
p3 <- boxplot_par_compare_with_before_cb("pm25_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PM[2.5]~(mu*g/m^3) ) ))  + guides(fill = FALSE)
p4 <- boxplot_par_compare_with_before_cb("pm25_hourly") +              ylab(bquote(bold( 1-hr~PM[2.5]~(mu*g/m^3) ) ))  

p5 <- boxplot_par_compare_with_before_cb( "no2_one_hour_max") +        ylab(bquote(bold( 1-hr~NO[2]~(mu*g/m^3)  ) ))  + guides(fill = FALSE)
p6 <- boxplot_par_compare_with_before_cb( "co_eight_hour_max") +       ylab(bquote(bold( 8-hr~CO~(mg/m^3)       ) ))  + guides(fill = FALSE)
p7 <- boxplot_par_compare_with_before_cb( "so2_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~SO[2]~(mu*g/m^3) ) ))  + guides(fill = FALSE)
p8 <- boxplot_par_compare_with_before_cb( "o3_eight_hour_max") +       ylab(bquote(bold( 8-hr~O[3]~(mu*g/m^3)   ) ))  + guides(fill = FALSE)

p1+p2+p3+p4+p5+p6+p7+p8 + plot_layout(nrow = 2) + plot_layout(guides = "collect")

ggsave("plots/compare_before_and_during_cb.pdf", 
       width = 9, height = 6, useDingbats=FALSE)


# 往年同期比较--------
dt$parameter %>% unique

dt[location == "national" & yday %in% same_period & !parameter %like% "index" & parameter != "psi_three_hourly" ]%>% 
  .[,year := as.character(year)] %>%
  ggplot (aes(year, value, fill = year)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
  stat_summary(fun.y = mean, geom= "point", shape= 23, size= 2 , 
               fill = "white", position = position_dodge(width = 0.75)) +
  # geom_signif(comparisons = list(c("2016", "2017", "2018", "2019", "2020")),
  #             map_signif_level=TRUE) +
  facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
  
  scale_fill_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  mytheme_basic +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

boxplot_par_compare_with_last_years <- function(par){
  dt[location == "national" & yday %in% same_period & parameter == par ]%>% 
    .[,year := as.character(year)] %>%
    ggplot (aes(year, value, fill = year)) +
    geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
    stat_summary(fun.y = mean, geom= "point", shape= 23, size= 2 , 
                 fill = "white", position = position_dodge(width = 0.75)) +
    # geom_signif(comparisons = list(c("before_cb", "cb")),
    #             map_signif_level=TRUE) +
    # facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
    
    scale_fill_manual (name="Year",
                       # labels= phase_names ,
                       values = color_manual_year) +
    mytheme_basic +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          axis.title.x = element_blank())
}

p1 <- boxplot_par_compare_with_last_years("psi_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PSI ) )) + guides(fill = FALSE)
p2 <- boxplot_par_compare_with_last_years("pm10_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PM[10]~(mu*g/m^3) ) ))  + guides(fill = FALSE)
p3 <- boxplot_par_compare_with_last_years("pm25_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PM[2.5]~(mu*g/m^3) ) ))  + guides(fill = FALSE)
p4 <- boxplot_par_compare_with_last_years("pm25_hourly") +              ylab(bquote(bold( 1-hr~PM[2.5]~(mu*g/m^3) ) ))  

p5 <- boxplot_par_compare_with_last_years( "no2_one_hour_max") +        ylab(bquote(bold( 1-hr~NO[2]~(mu*g/m^3)  ) ))  + guides(fill = FALSE)
p6 <- boxplot_par_compare_with_last_years( "co_eight_hour_max") +       ylab(bquote(bold( 8-hr~CO~(mg/m^3)       ) ))  + guides(fill = FALSE)
p7 <- boxplot_par_compare_with_last_years( "so2_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~SO[2]~(mu*g/m^3) ) ))  + guides(fill = FALSE)
p8 <- boxplot_par_compare_with_last_years( "o3_eight_hour_max") +       ylab(bquote(bold( 8-hr~O[3]~(mu*g/m^3)   ) ))  + guides(fill = FALSE)

p1+p2+p3+p4+p5+p6+p7+p8 + plot_layout(nrow = 2) + plot_layout(guides = "collect")

ggsave("plots/compare_last_years.pdf", 
       width = 9, height = 6, useDingbats=FALSE)


boxplot_par_compare_with_last_years <- function(par){
  dt[location == "national" & yday %in% same_period & parameter == par ] %>% 
    .[,year := as.character(year)] %>%
    ggplot (aes(phase, value, fill = year)) +
    # geom_jitter(aes(color = year), alpha = 0.3) +
    geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5, width = 0.8) +
    stat_summary(aes(group = year), fun.y = mean, geom= "point", shape= 23, size= 2 , 
               fill = "white", position = position_dodge(width = 0.8)) +
    geom_signif(comparisons = list(c("before", "cb")),
                map_signif_level=TRUE) +
    # facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
    
    scale_fill_manual (name="Year",
                       # labels= phase_names ,
                       values = color_manual_year) +
    # scale_color_manual (name="Year",
    #                    # labels= phase_names ,
    #                    values = color_manual_year) +
    mytheme_basic +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          axis.title.x = element_blank())
}

p1 <- boxplot_par_compare_with_last_years("psi_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PSI ) )) + guides(fill = FALSE)
p2 <- boxplot_par_compare_with_last_years("pm10_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PM[10]~(mu*g/m^3) ) ))  + guides(fill = FALSE)
p3 <- boxplot_par_compare_with_last_years("pm25_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PM[2.5]~(mu*g/m^3) ) ))  + guides(fill = FALSE)
p4 <- boxplot_par_compare_with_last_years("pm25_hourly") +              ylab(bquote(bold( 1-hr~PM[2.5]~(mu*g/m^3) ) ))  

p5 <- boxplot_par_compare_with_last_years( "no2_one_hour_max") +        ylab(bquote(bold( 1-hr~NO[2]~(mu*g/m^3)  ) ))  + guides(fill = FALSE)
p6 <- boxplot_par_compare_with_last_years( "co_eight_hour_max") +       ylab(bquote(bold( 8-hr~CO~(mg/m^3)       ) ))  + guides(fill = FALSE)
p7 <- boxplot_par_compare_with_last_years( "so2_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~SO[2]~(mu*g/m^3) ) ))  + guides(fill = FALSE)
p8 <- boxplot_par_compare_with_last_years( "o3_eight_hour_max") +       ylab(bquote(bold( 8-hr~O[3]~(mu*g/m^3)   ) ))  + guides(fill = FALSE)

p1+p2+p3+p4+p5+p6+p7+p8 + plot_layout(nrow = 2) + plot_layout(guides = "collect")

ggsave("plots/compare_last_years.pdf", 
       width = 9, height = 6, useDingbats=FALSE)


dt$parameter %>% unique
dt[location == "national" & yday %in% same_period & parameter == "pm10_twenty_four_hourly" & year %in% c(2019,2020)]%>% 
  .[,year := as.character(year)] %>%
  ggplot (aes(phase, value, fill = year)) +
  # geom_jitter(aes(color = year), alpha = 0.3) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5, width = 0.8) +
  stat_summary(aes(group = year), fun.y = mean, geom= "point", shape= 23, size= 2 , 
               fill = "white", position = position_dodge(width = 0.8)) +
  geom_signif(comparisons = list(c("before", "cb")),
              map_signif_level=TRUE) +
  # facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
  
  scale_fill_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  # scale_color_manual (name="Year",
  #                    # labels= phase_names ,
  #                    values = color_manual_year) +
  mytheme_basic +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank())

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

  