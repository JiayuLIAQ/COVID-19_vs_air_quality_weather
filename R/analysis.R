source("./R/functions.R")
source("./R/load_data.R")

# compare table----------------------------------------------------------------------

#compare with same period last years
t_table_years <- dt[location == "national_mean" & yday %in% same_period & parameter != "psi_three_hourly"]  %>%
  .[, {t <- wilcox.test(value[phase =="before"], value[phase == "cb"])
  p <- t$p.value
  list(p.value = p)
  }, by = .(parameter)]
t_table_years[p.value >= 0.05, sign := "NS."]
t_table_years[p.value < 0.05 & p.value >= 0.01, sign := "*"]
t_table_years[p.value < 0.01 & p.value >= 0.001, sign := "**"]
t_table_years[p.value < 0.001, sign := "***"]

compare_table_years <- dt[location == "national_mean" & yday %in% same_period & parameter != "psi_three_hourly"]  %>%
  .[, .(cb_mean = mean(value[phase == "cb"], na.rm = T),
        before_mean = mean(value[phase =="before"], na.rm = T),
        delta_mean = mean(value[phase == "cb"], na.rm = T) - mean(value[phase =="before"], na.rm = T),
        delta_mean_prop = 100 * (mean(value[phase == "cb"], na.rm = T) - mean(value[phase =="before"], na.rm = T))/mean(value[phase =="before"], na.rm = T)
  ), by = .(parameter)]

compare_table_year_t <- compare_table_years[t_table_years, on = .(parameter)]  %>% setorder(parameter)


#compare with two weeks before CB
t_before_two_weeks <- dt[ parameter != "psi_three_hourly"]  %>%
  .[, {t <- wilcox.test(value[phase =="before_cb"], value[phase == "cb"])
  p <- t$p.value
  list(p.value = p)
  }, by = .(parameter, location)]
t_before_two_weeks[p.value >= 0.05, sign := "NS."]
t_before_two_weeks[p.value < 0.05 & p.value >= 0.01, sign := "*"]
t_before_two_weeks[p.value < 0.01 & p.value >= 0.001, sign := "**"]
t_before_two_weeks[p.value < 0.001, sign := "***"]

compare_table_before_two_weeks <- dt[parameter != "psi_three_hourly"]  %>%
  .[, .(cb_mean = mean(value[phase == "cb"], na.rm = T),
        before_mean = mean(value[phase =="before_cb"], na.rm = T),
        delta_mean = mean(value[phase == "cb"], na.rm = T) - mean(value[phase =="before_cb"], na.rm = T),
        delta_mean_prop = 100 * (mean(value[phase == "cb"], na.rm = T) - mean(value[phase =="before_cb"], na.rm = T))/mean(value[phase =="before_cb"], na.rm = T)
  ), by = .(parameter,location)]

compare_table_before_two_weeks_t <- compare_table_before_two_weeks[t_before_two_weeks, on = .(parameter, location)]  %>% setorder(parameter, location)


compare_table_before_two_weeks_t[!parameter %like% "index" & location != "national"] %>% setorder(location) %>%
  write_file("./plots/compare_table_before_two_weeks_2.csv")

# plots --------------------------------------------------------------------------
symnum.args <- list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "NS."))

dt[parameter != "psi_three_hourly" & phase != "before" & !parameter %like% "index" & location != "national"]  %>%
  # .[, .(value = mean(value, na.rm = T)
  # ), by = .(parameter,location, phase)] %>% 
  ggplot(aes(location, value, fill = phase)) +
  # geom_col(position = "dodge") +
  # geom_bar(position = "dodge", stat = "identity") +
  stat_summary(aes(group = phase), fun.y = mean, geom= "bar", 
               position = position_dodge(width = 0.95)) +
  stat_compare_means(aes(group = phase), label = "p.signif", method = "wilcox.test", symnum.args = symnum.args) +
  # coord_cartesian(ylim = c(30,55))+
  facet_wrap(vars(parameter_fct), scales = "free_y", nrow = 2, labeller = label_parsed) +
  mytheme_basic +
  theme(
    axis.text.x = element_text(vjust=0.5, hjust = 1, angle = 90) )


dt[parameter != "psi_three_hourly" & phase != "before" & !parameter %like% "index" & location != "national"]  %>%
  ggplot(aes(location, value, fill = phase)) +
  geom_boxplot( position = "dodge") +
  stat_summary(aes(group = phase), fun.y = mean, geom= "point", shape= 23, size= 2 , 
               fill = "white", position = position_dodge(width = 0.75)) +
  facet_wrap(vars(parameter_fct), scales = "free_y", nrow = 2, labeller = label_parsed) +
  stat_compare_means(aes(group = phase), label = "p.signif", method = "wilcox.test", symnum.args = symnum.args) +
  mytheme_basic +
  theme(
    axis.text.x = element_text(vjust=0.5, hjust = 1, angle = 90) )


# dt$parameter %>% unique
# dt [location != "national" &  !parameter %like% "index" & phase %in% c("before_cb","cb")] %>% 
  
dt [location != "national" &  !parameter %like% "index" & datetime > ymd("2020-02-15")] %>%  
  .[, .(value = mean(value, na.rm = T) ), by = .(datetime = date(datetime), parameter, location, parameter_fct)] %>%
  ggplot (aes(datetime, value, color = location)) +
  geom_line() +
  geom_vline(xintercept = ymd("2020-04-07"),linetype = 2) +
  facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
  # scale_fill_manual (name="Phase",
  #                    labels= phase_names ,
  #                    values = color_manual_phase) +
  mytheme_basic 


dt [ phase %in% c("before","before_cb","cb") & !parameter %like% "index" & parameter != "psi_three_hourly"] %>%
  ggplot (aes(phase, value, fill = phase)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
  stat_summary(fun.y = mean, geom= "point", shape= 23, size= 2 ,
               fill = "white", position = position_dodge(width = 0.75)) +
  geom_signif(comparisons = list(c("before_cb", "cb"), c("before","cb")),
              map_signif_level=TRUE) +
  facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +

  # scale_fill_manual (name="Phase",
  #                    labels= phase_names ,
  #                    values = color_manual_phase) +

  mytheme_basic +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave("plots/compare_before_and_during_cb.pdf",
       width = 10, height = 7, useDingbats=FALSE)

boxplot_par_compare_with_before_cb <- function(par){
dt [location == "national_mean" & parameter == par & phase %in% c("before_cb","cb")] %>% 
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


dt[location == "national_mean" & yday %in% same_period & !parameter %like% "index" & parameter != "psi_three_hourly" ]%>% 
  .[,year := as.character(year)] %>%
  ggplot (aes(year, value, fill = year)) +
  geom_half_boxplot(outlier.alpha = 0, outlier.size = 0.5) +
  # geom_half_violin(side = "r") +
  geom_half_point(aes(color = year), side = "r", alpha = 0.1, size= 1,transformation = position_jitter(width = 1, height = 1)) +
  stat_summary(fun.y = mean, geom= "point", shape= 23, size= 2 , 
               fill = "white", position = position_dodge(width = 0.75)) +
  geom_signif(comparisons = list(c("2019", "2020")),
              map_signif_level=TRUE) +
  facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
  
  scale_fill_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  scale_color_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  mytheme_basic +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# fig_1 final-----
# need to load compare_table_year_t 
boxplot_par_compare_with_last_years <- function(dt_, par){
  
  dt_ <- dt_[location == "national" & yday %in% same_period & parameter == par ] 
    # .[, .(value = mean(value, na.rm = T) ), by =.(datetime = date(datetime), parameter, year, parameter) ]
  
  y_posi_1 <- max(dt_[year %in% c(2019, 2020)]$value) * 1.05
  
  y_posi_2 <- max(dt_$value) * 1.05
  range_ <- max(dt_$value)-min(dt_$value)
  
  if(y_posi_2-y_posi_1 < 0.05 * range_) y_posi_2 <- y_posi_2 + 0.1 * range_
  
  dt_ %>% 
    .[,year := as.character(year)] %>%
    ggplot (aes(year, value, fill = year)) +
    geom_half_boxplot(outlier.alpha = 0, outlier.size = 0.5) +
    # geom_half_violin(side = "r") +
    geom_half_point(aes(color = year), side = "r", alpha = 0.5, size= 1, transformation = position_jitter(width = 0.5)) +
    stat_summary(fun.y = mean, geom= "point", shape= 23, size= 2 , 
                 fill = "white", position = position_dodge(width = 0.75)) +
    geom_signif(y_position = y_posi_1, comparisons = list(c("2019", "2020")),
                map_signif_level=TRUE, test = "wilcox.test") +
    
    geom_signif(y_position=y_posi_2, xmin=2.5, xmax=5, annotation = compare_table_year_t[parameter == par]$sign, tip_length=0.03) +
    geom_signif(y_position=y_posi_2 - range_*0.03 , xmin=1, xmax=4, annotation=c(" "), tip_length=0) +
    scale_fill_manual (name="Year",
                       # labels= phase_names ,
                       values = color_manual_year) +
    scale_color_manual (name="Year",
                       # labels= phase_names ,
                       values = color_manual_year) +
    mytheme_basic +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x=element_blank(),
          axis.title.x = element_blank())
}

# boxplot_par_compare_with_last_years(dt, "psi_twenty_four_hourly") +   ylab(bquote(bold( 24-hr~PSI ) ))
  
p1 <- boxplot_par_compare_with_last_years(dt, "psi_twenty_four_hourly") +   ylab(bquote(bold( 24-hr~PSI ) )) + guides(fill = FALSE, color = FALSE) 
p2 <- boxplot_par_compare_with_last_years(dt, "pm10_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PM[10]~(mu*g/m^3) ) ))  + guides(fill = FALSE, color = FALSE) 
p3 <- boxplot_par_compare_with_last_years(dt, "pm25_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~PM[2.5]~(mu*g/m^3) ) ))  + guides(fill = FALSE, color = FALSE)   
p4 <- boxplot_par_compare_with_last_years(dt, "pm25_hourly") +              ylab(bquote(bold( 1-hr~PM[2.5]~(mu*g/m^3) ) ))  
p5 <- boxplot_par_compare_with_last_years(dt, "no2_one_hour_max") +        ylab(bquote(bold( 1-hr~NO[2]~(mu*g/m^3)  ) ))  + guides(fill = FALSE, color = FALSE)   
p6 <- boxplot_par_compare_with_last_years(dt, "co_eight_hour_max") +       ylab(bquote(bold( 8-hr~CO~(mg/m^3)       ) ))  + guides(fill = FALSE, color = FALSE)   
p7 <- boxplot_par_compare_with_last_years(dt, "so2_twenty_four_hourly") +  ylab(bquote(bold( 24-hr~SO[2]~(mu*g/m^3) ) ))  + guides(fill = FALSE, color = FALSE)   
p8 <- boxplot_par_compare_with_last_years(dt, "o3_eight_hour_max") +       ylab(bquote(bold( 8-hr~O[3]~(mu*g/m^3)   ) ))  + guides(fill = FALSE, color = FALSE)  
 

p1+p2+p3+p4+p5+p6+p7+p8 + plot_layout(nrow = 2) + plot_layout(guides = "collect")

ggsave("plots/compare_last_years_2.pdf", 
       width = 9, height = 5, useDingbats=FALSE)

p1 <- boxplot_par_compare_with_last_years(dt_daily, "psi_twenty_four_hourly") +   ylab(bquote(bold( PSI ) )) + guides(fill = FALSE, color = FALSE) 
p2 <- boxplot_par_compare_with_last_years(dt_daily, "pm10_twenty_four_hourly") +  ylab(bquote(bold( PM[10]~(mu*g/m^3) ) ))  + guides(fill = FALSE, color = FALSE) 
p3 <- boxplot_par_compare_with_last_years(dt_daily, "pm25_twenty_four_hourly") +  ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))    
p4 <- boxplot_par_compare_with_last_years(dt_daily, "pm25_hourly") +              ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))  + guides(fill = FALSE, color = FALSE) 
p5 <- boxplot_par_compare_with_last_years(dt_daily, "no2_one_hour_max") +        ylab(bquote(bold( NO[2]~(mu*g/m^3)  ) ))  + guides(fill = FALSE, color = FALSE)   
p6 <- boxplot_par_compare_with_last_years(dt_daily, "co_eight_hour_max") +       ylab(bquote(bold( CO~(mg/m^3)       ) ))  + guides(fill = FALSE, color = FALSE)   
p7 <- boxplot_par_compare_with_last_years(dt_daily, "so2_twenty_four_hourly") +  ylab(bquote(bold( SO[2]~(mu*g/m^3) ) ))  + guides(fill = FALSE, color = FALSE)   
p8 <- boxplot_par_compare_with_last_years(dt_daily, "o3_eight_hour_max") +       ylab(bquote(bold( O[3]~(mu*g/m^3)   ) ))  + guides(fill = FALSE, color = FALSE)  

p1+p2+p4+p3+p5+p6+p7+p8 + plot_layout(nrow = 2) + plot_layout(guides = "collect")

ggsave("plots/compare_last_years_2.pdf", 
       width = 9, height = 5, useDingbats=FALSE)
  