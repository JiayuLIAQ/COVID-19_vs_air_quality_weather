library(ggplot2)
library(dplyr)


# Add columns for year, yday and month-------------------------


library(ggridges)
dt [, year := year(datetime)]
dt [, yday := yday(datetime)]
dt [, month := month(datetime, label = T)]

dt[parameter == "no2_one_hour_max"] %>%
ggplot(aes(x = value, y = month, height = ..density..)) +
  geom_density_ridges(stat = "density") +
  facet_wrap(vars(year))

dt[location != "national_mean" & parameter == "no2_one_hour_max"] %>% .[, year := as.character(year)] %>%
ggplot(aes(x = value, y = month, height = ..density.., fill = year)) +
  geom_density_ridges(stat = "density") +
  facet_wrap(vars(location), ncol=) +
  scale_fill_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  mytheme_basic


dt[location != "national_mean" & parameter == "no2_one_hour_max"] %>% .[, year := as.character(year)] %>%
  ggplot(aes(x = month, y = value, fill= year, group = paste(month,year) )) +
  geom_boxplot(outlier.alpha = 0.5, outlier.size = 0.5) +
  facet_wrap(vars(location), labeller = labeller(location = location_names)) +
  # stat_compare_means( aes(group = year), comparisons = list(c("2019","2020")), label = "p.signif") +
  # geom_half_boxplot(outlier.alpha = 0, outlier.size = 0.5) +
  # geom_half_violin(side = "r") +
  # geom_half_point(aes(color = year), side = "r", alpha = 0.5, size= 1, transformation = position_jitter(width = 0.5)) +
  scale_fill_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  scale_color_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  mytheme_basic

dt[location == "national" & !is.na(parameter) & parameter != "psi_three_hourly" & parameter != "pm25_twenty_four_hourly"] %>% 
  # .[!(parameter == "co_eight_hour_max" & value >3) & !(parameter == "co_eight_hour_max" & value >3)] %>% 
  .[, year := as.character(year)] %>%
  ggplot(aes(x = month, y = value, fill= year, group = paste(month,year) )) +
  stat_summary(fun.data = calc_boxplot_stat, geom="boxplot", position = position_dodge(width = 0.75)) +     # without outlier
  # geom_boxplot(outlier.alpha = 0.5, outlier.size = 0.5) +
  facet_grid(parameter_fct~. , labeller = label_parsed, scales = "free_y") +
  # geom_half_boxplot(outlier.alpha = 0, outlier.size = 0.5) +
  # geom_half_violin(side = "r") +
  # geom_half_point(aes(color = year), side = "r", alpha = 0.5, size= 1, transformation = position_jitter(width = 0.5)) +
  scale_fill_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  scale_color_manual (name="Year",
                      # labels= phase_names ,
                      values = color_manual_year) +
  mytheme_basic


dt[location == "national" & !is.na(parameter) & parameter != "psi_three_hourly" & parameter != "pm25_twenty_four_hourly"] %>% 
  # .[!(parameter == "co_eight_hour_max" & value >3) & !(parameter == "co_eight_hour_max" & value >3)] %>% 
  .[, year := as.character(year)] %>%
  .[, .(
    value_mean = mean(value, na.rm = T),
    # value_max = max(value, na.rm = T),
    # value_min = min(value, na.rm = T),
    # value_05 = quantile(value,probs=c(.05), na.rm = T),
    value_25 = quantile(value,probs=c(.25), na.rm = T),
    value_50 = quantile(value,probs=c(.50), na.rm = T),
    value_75 = quantile(value,probs=c(.75), na.rm = T)
    # value_95 = quantile(value,probs=c(.95), na.rm = T),
    # value_sd = sd(value, na.rm = T)
  ), by = .(year, month, parameter, parameter_fct, location)] %>%
  
  ggplot(aes(x = month, fill= year, group = year)) +
  geom_ribbon(aes(ymin = value_25, ymax = value_75), alpha = 0.5) +
  geom_line(aes(y= value_50, color = year)) +

  facet_grid(parameter_fct~. , labeller = label_parsed, scales = "free_y") +
  # stat_compare_means( aes(group = year), comparisons = list(c("2019","2020")), label = "p.signif") +
  # geom_half_boxplot(outlier.alpha = 0, outlier.size = 0.5) +
  # geom_half_violin(side = "r") +
  # geom_half_point(aes(color = year), side = "r", alpha = 0.5, size= 1, transformation = position_jitter(width = 0.5)) +
  scale_fill_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  scale_color_manual (name="Year",
                      # labels= phase_names ,
                      values = color_manual_year) +
  mytheme_basic

calc_boxplot_stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  # set whiskers
  outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}

ggplot(diamonds, aes(x=cut, y=price, fill=cut)) + 
  stat_summary(fun.data = calc_boxplot_stat, geom="boxplot") + 
  facet_wrap(~clarity, scales="free")

# add significance stars------------------
library(ggplot2)
library(ggsignif)

ggplot(iris, aes(x=Species, y=Sepal.Length)) + 
  geom_boxplot() +
  geom_signif(comparisons = list(c("versicolor", "virginica")), 
              map_signif_level=TRUE)

dt [!is.na(phase) & !parameter %like% "index"] %>% 
  ggplot (aes(phase, value)) +
  geom_boxplot() +
  facet_wrap(vars(parameter) , scales = "free") +
  geom_signif(comparisons = list(c("before_cb", "cb")),
              map_signif_level=TRUE)


# ggsigniff use "wilcox.test" as default



ggbarplot(dt[parameter != "psi_three_hourly" & phase != "before" & !parameter %like% "index" & location != "national"] , 
          x = "location", y = "value", add = "mean_se",
          color = "phase", palette = "jco", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = phase), label = "p.signif") +
  facet_wrap(vars(parameter_fct), scales = "free", labeller = label_parsed) 

ggbarplot(ToothGrowth, x = "dose", y = "len", add = "mean_se",
          color = "supp", palette = "jco", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = supp), label = "p.signif", label.y = 29)



ggline(ToothGrowth, x = "dose", y = "len", add = "mean_se",
       color = "supp", palette = "jco")+
  stat_compare_means(aes(group = supp), label = "p.signif", 
                     label.y = c(16, 25, 29))




test <- dt[location == "national" & yday %in% same_period & parameter == "psi_twenty_four_hourly"]

t.results <- wilcox.test(test[phase == "before"]$value, test[phase == "cb"]$value)

t.results$p.value
t.results$

test <- dt[location == "national" & yday %in% same_period & parameter == "pm25_twenty_four_hourly"]

t.results <- t.test(test[phase == "before"]$value, test[phase == "cb"]$value)
t.results$p.value


dcast(datetime + parameter ~ phase, value.var = "value") 

test[, {t <-  t.test(before, cb)
p <- t$p.value
list(p)
}, by = .(parameter)]


test %>% dcast(index ~ phase, value.var = "value") %>% .[, .(t = t.test(before, cb))]

t.results <- t.test(test[phase == "before"]$value, test[phase == "cb"]$value)




tt <- t.test(1:10, y = c(7:20, 200)) 

tt$p.value


# plot test----
dt[location == "national" & yday %in% same_period & !parameter %like% "index" & parameter != "psi_three_hourly" ]%>% 
  .[,year := as.character(year)] %>%
  ggplot (aes(year, value)) +
  geom_half_boxplot(aes(fill = year), outlier.alpha = 0, outlier.size = 0.5) +
  
  # geom_half_violin(aes(fill = year), side = "r") +
  geom_half_point(aes(color = year), side = "r", alpha = 0.1, size= 1,transformation = position_jitter(width = 1, height = 1)) +
  stat_summary(fun.y = mean, geom= "point", shape= 23, size= 2 , 
               fill = "white", position = position_dodge(width = 0.75)) +
  geom_signif(comparisons = list(c("2019", "2020")),
              map_signif_level=TRUE, test = "t.test") +
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


# different phase-----------------------


dt_daily[, parameter_fct := factor(parameter,
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


dt_daily[, phase_2 := NULL]
dt_daily[datetime >= ymd("2020-04-07"), phase_2 := "cb"]
dt_daily[datetime >= ymd("2020-03-13") & datetime < ymd("2020-04-07"), phase_2 := "et"] #early treatment
dt_daily[datetime >= ymd("2020-01-03") & datetime < ymd("2020-02-06"), phase_2 := "baseline"] #baseline
dt_daily[, phase_2 := factor(phase_2, levels = c("baseline", "et", "cb") )]

dt_daily [ phase_2 %in% c("baseline","et","cb") & !parameter %like% "index" & parameter != "psi_three_hourly" & location == "west"] %>%
  # .[, .(value = mean(value, na.rm = T) ), by =.(datetime = date(datetime), parameter, phase_2, parameter_fct) ] %>%
  ggplot (aes(phase_2, value, fill = phase_2)) +
  # geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
  geom_violin() +
  stat_summary(fun.y = mean, geom= "point", shape= 23, size= 2 ,
               fill = "white", position = position_dodge(width = 0.75)) +
  geom_signif(comparisons = list(c("baseline", "et"), c("et","cb")),
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





boxplot_par_compare_with_last_years <- function(dt_, par){
  
  dt_ <- dt_[location == "national_mean" & yday %in% same_period & parameter == par ] 
  # .[, .(value = mean(value, na.rm = T) ), by =.(datetime = date(datetime), parameter, year, parameter) ]
  
  y_posi_1 <- max(dt_[year %in% c(2019, 2020)]$value) * 1.05
  
  y_posi_2 <- max(dt_$value) * 1.05
  range_ <- max(dt_$value)-min(dt_$value)
  
  if(y_posi_2-y_posi_1 < 0.05 * range_) y_posi_2 <- y_posi_2 + 0.1 * range_
  
  dt_ %>% 
    .[,year := as.character(year)] %>%
    ggplot (aes(year, value)) +
    geom_half_boxplot(aes(fill = year), outlier.alpha = 0, outlier.size = 0.5) +
    geom_half_violin(aes(color = year), side = "r") +
    # geom_half_point(aes(color = year), side = "r", alpha = 0.5, size= 1, transformation = position_jitter(width = 0.5)) +
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



# corelation ----


dt_mobi_g <- dt_mobi_g %>% dcast(date~places)
dt_daily$parameter %>% unique


my.formula <- y ~ x 
library(ggpmisc)
dt_daily[dt_mobi_g, on = .(date)] %>% .[date >= ymd("2020-04-07")] %>%
  ggplot(aes(percent_change_from_baseline, value)) +
  geom_point() +
  facet_wrap(vars(parameter, places), scales = "free") +
  geom_smooth( method = "lm", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")) , rr.digits = 4, 
               parse = TRUE) +  
  mytheme_basic


my.formula <- y ~ x 
library(ggpmisc)
dt_daily[dt_mobi_g, on = .(date)] %>% .[date >= ymd("2020-04-01")] %>%
  ggplot(aes(value, parks)) +
  geom_point() +
  facet_wrap(vars(parameter), scales = "free") +
  geom_smooth( method = "lm", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")) , rr.digits = 4, 
               parse = TRUE) +  
  mytheme_basic





# test considering the trend across years----

compare_table_years



year_index_dt[dt_daily, on = .(year)][ yday %in% same_period  & index != 5 & location =="national"] %>%
    .[, .(value = mean(value, na.rm = T) ), by = .(index, year, location, parameter)] %>%
  ggplot(aes(index, value, color = location, group = location)) +
  geom_point( ) +
  geom_smooth( method = "lm", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")) , rr.digits = 4, 
               parse = TRUE) +  
  facet_wrap(vars(parameter), scales = "free") +
  mytheme_basic



test <- year_index_dt[dt_daily, on = .(year)][ yday %in% same_period & location =="national" & parameter == "psi_twenty_four_hourly"] %>%
  .[, .(value = mean(value, na.rm = T) ), by = .(index, year, location, parameter)]   

t.model <- lm(value ~ index , data = test[index != 5])
library(broom)
tidy(t.model)
glance(t.model)$p.value

ttt <- summary(t.model)
ttt
new_dt <- data.table(index = 5)
ttt <- predict(t.model, newdata = new_dt)
ttt-1

tttt <- predict(t.model, newdata = new_dt, interval = "confidence" )
tttt[2]
tttt[3]

my.formula <- y ~ x 
library(ggpmisc)
dt_daily[dt_mobi_g, on = .(date)] %>% .[date >= ymd("2020-04-01")] %>%
  ggplot(aes(value, parks)) +
  geom_point() +
  facet_wrap(vars(parameter), scales = "free") +
  geom_smooth( method = "lm", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")) , rr.digits = 4, 
               parse = TRUE) +  
  mytheme_basic

  

p1 <- boxplot_par_compare_with_last_years(dt_daily, "psi_twenty_four_hourly") +   
  ylab(bquote(bold( PSI ) )) + 
  geom_abline(intercept = 61.59338, slope = -2.120664)

str(p1$layers)


delete_layers(p1, "GeomAbline")




dt_daily [location == "national" &  !parameter %like% "index" & datetime > ymd("2020-03-24")] %>%  
  .[, .(value = mean(value, na.rm = T) ), by = .(datetime = date(datetime), parameter, location, parameter_fct)] %>%
  ggplot (aes(datetime, value, color = location)) +
  geom_line() +
  geom_vline(xintercept = ymd("2020-04-07"),linetype = 2) +
  facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
  # scale_fill_manual (name="Phase",
  #                    labels= phase_names ,
  #                    values = color_manual_phase) +
  mytheme_basic 


# line plot------------------------------
dt_daily$parameter %>% unique
dt_daily [location == "national" &   datetime > ymd("2020-03-20") & parameter == "psi_twenty_four_hourly"] %>%  
  # .[, .(value = mean(value, na.rm = T) ), by = .(datetime = date(datetime), parameter, location, parameter_fct)] %>%
  ggplot (aes(datetime, value, fill = location)) +
  geom_area() +
  geom_vline(xintercept = ymd("2020-04-07"),linetype = 2) +
  # facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
  # scale_fill_manual (name="Phase",
  #                    labels= phase_names ,
  #                    values = color_manual_phase) +
  mytheme_basic 

dt_daily [location == "national" &   datetime > ymd("2020-03-20") & parameter == "no2_one_hour_max"] %>%  
  # .[, .(value = mean(value, na.rm = T) ), by = .(datetime = date(datetime), parameter, location, parameter_fct)] %>%
  ggplot (aes(datetime, value, fill = location)) +
  geom_area() +
  geom_vline(xintercept = ymd("2020-04-07"),linetype = 2) +
  # facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
  # scale_fill_manual (name="Phase",
  #                    labels= phase_names ,
  #                    values = color_manual_phase) +
  mytheme_basic 



dt_daily [location == "national" &   datetime > ymd("2020-03-20") ] %>%  
  # .[, .(value = mean(value, na.rm = T) ), by = .(datetime = date(datetime), parameter, location, parameter_fct)] %>%
  ggplot (aes(datetime, value, fill = parameter)) +
  geom_area() +
  geom_vline(xintercept = ymd("2020-04-07"), linetype = 2 ) +
  # coord_cartesian(ylim = c(6,14)) +
  facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
  scale_fill_manual (name="Phase",
                     labels= parameter_names ,
                     values = color_manual_parameter) +
  mytheme_basic 


dt_daily [location == "national" &   datetime > ymd("2020-03-20") ] %>%  
  ggplot () +
  geom_area(aes(datetime, value, fill = parameter)) +
  # geom_ribbon(aes(datetime, ymin = min(value), ymax = value, fill = parameter)) +
  geom_vline(xintercept = ymd("2020-04-07"), linetype = 2 ) +
  # coord_cartesian(ylim = c(6,14)) +
  facet_wrap(vars(parameter_fct) , scales = "free", nrow = 2, labeller=label_parsed) +
  scale_fill_manual (name="Phase",
                     labels= parameter_names ,
                     values = color_manual_parameter) +
  mytheme_basic +
  theme( legend.position = "bottom")

# which parameter determin the PSI----------


dt$parameter %>% unique

dt %>% dcast(datetime + location ~ parameter, value.var = "value") %>% .[pm25_sub_index == ""]


dt  [ datetime > ymd("2020-03-20") & (parameter %like% "index" | parameter %like% "psi") & parameter != "pm25_sub_index"] %>% 
  ggplot () +
  geom_line(aes(datetime, value, color = parameter)) +
  # geom_ribbon(aes(datetime, ymin = min(value), ymax = value, fill = parameter)) +
  geom_vline(xintercept = ymd("2020-04-07"), linetype = 2 ) +
  # coord_cartesian(ylim = c(6,14)) +
  facet_wrap(vars(location) , scales = "free", nrow = 2) +
  # scale_fill_manual (name="Phase",
  #                    labels= parameter_names ,
  #                    values = color_manual_parameter) +
  mytheme_basic +
  theme( legend.position = "bottom")



# test wind rose----


library(openair)
dt_weather_national[year==2016]  %>%
  windRose ( ws = "WindSpeed", wd = "WindDirection") +
  facet_wrap(vars(year))
dt_weather_national[year==2017]  %>%
  windRose ( ws = "WindSpeed", wd = "WindDirection") +
  facet_wrap(vars(year))
dt_weather_national[year==2018]  %>%
  windRose ( ws = "WindSpeed", wd = "WindDirection") +
  facet_wrap(vars(year))
dt_weather_national[year==2019]  %>%
  windRose ( ws = "WindSpeed", wd = "WindDirection") +
  facet_wrap(vars(year))
dt_weather_national[year==2020]  %>%
  windRose ( ws = "WindSpeed", wd = "WindDirection") +
  facet_wrap(vars(year))

# package clifro

library(forcats)
dt_weather_national %>% setorder(datetime)

dt_weather_national[year != 2016, year_month := paste(year, month) %>% fct_inorder ]

with(dt_weather_national[year != 2016], windrose(WindSpeed, WindDirection, facet = year_month, n_col = 12 ) ) #画所有

