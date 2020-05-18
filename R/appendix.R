#compare with same period last years
t_table_years_months <- dt_daily[parameter != "psi_three_hourly" & month %in% c("Jan","Feb","Mar","Apr","May")]  %>%
  .[, {t <- wilcox.test(value[year == 2020], value[year < 2020])
  p <- t$p.value
  list(p.value = p)
  }, by = .(parameter, location, month )]
t_table_years_months[p.value >= 0.05, sign := "NS."]
t_table_years_months[p.value < 0.05 & p.value >= 0.01, sign := "*"]
t_table_years_months[p.value < 0.01 & p.value >= 0.001, sign := "**"]
t_table_years_months[p.value < 0.001, sign := "***"]

compare_table_years_months <- dt_daily[ parameter != "psi_three_hourly"]  %>%
  .[, .(cb_mean = mean(value[year == 2020], na.rm = T),
        before_mean = mean(value[year < 2020], na.rm = T),
        delta_mean = mean(value[year == 2020], na.rm = T) - mean(value[year < 2020], na.rm = T),
        delta_mean_pctg = 100 * (mean(value[year == 2020], na.rm = T) - mean(value[year < 2020], na.rm = T))/mean(value[year < 2020], na.rm = T)
  ), by = .(parameter, location, month)]

compare_table_year_month_t <- compare_table_years_months[t_table_years_months, on = .(parameter, location)]  %>% setorder(location, parameter)

year_index_dt <- data.table(year = c(2016:2020),
                            index = c(1:5) )

lm_year_dt <- year_index_dt[dt_daily, on = .(year)] %>%
  .[, .(value = mean(value, na.rm = T) ), by = .(index, year, month, location, parameter)] %>%
  .[, { model <- lm(value ~ index, data = .SD[index != 5 ])
  new_dt <- data.table(index = 5)
  predicted_5 <- predict(model, new_dt)
  # predicted_5_05 <- predict(model, new_dt, interval = "confidence" )[2]
  # predicted_5_95 <- predict(model, new_dt, interval = "confidence" )[3]
  covid_change <- .SD[index == 5 ]$value - predicted_5 
  covid_change_pctg <- 100 * covid_change/.SD[index == 5 ]$value
  
  confi_DT  <-  confint(model, level =0.9) %>% data.table  # 5% ~ 95%的置信区间
  intercept <- coef(model)[1]
  intercept_05 <- confi_DT[[1]][1]
  intercept_95 <- confi_DT[[2]][1]
  slop <-  coef(model)[2]
  slop_05 <- confi_DT[[1]][2]
  slop_95 <-confi_DT[[2]][2]
  list(
    predicted_5 = predicted_5,
    # predicted_5_05 = predicted_5_05,
    # predicted_5_95 = predicted_5_95,
    covid_5 = .SD[index == 5 ]$value,
    covid_change = covid_change,
    covid_change_pctg = covid_change_pctg,
    r.squared = summary(model)$r.squared,
    slop = slop,
    slop_05 = slop_05,
    slop_95 = slop_95,
    intercept = intercept,
    intercept_05 = intercept_05,
    intercept_95 = intercept_95
  )}, by = .(month, location, parameter)] %>% setorder(month, location, parameter)


boxplot_par_compare_with_last_years_month <- function(dt_, par){
  
  dt_ <- dt_[location == "national"  & parameter == par ]
  # .[, .(value = mean(value, na.rm = T) ), by =.(datetime = date(datetime), parameter, year, parameter) ]
  # 
  # intercept_ <- lm_year_dt[parameter == par & location == "national"]$intercept 
  # slop_ <- lm_year_dt[parameter == par & location == "national"]$slop
  # covid_point <- lm_year_dt[parameter == par & location == "national" ]$covid_5 
  # if (lm_year_dt[parameter == par & location == "national" ]$r.squared > 0.6) {
  #   baseline_point <- lm_year_dt[parameter == par & location == "national" ]$predicted_5
  # } else {
  #   baseline_point <- compare_table_year_t[parameter == par & location == "national" ]$before_mean 
  # }
  # 
  # annotation_line_dt <- data.table (x_ = c(5.7,5.7),
  #                                   y_ = c(baseline_point, covid_point))
  # 
  # y_posi_1 <- max(dt_[year %in% c(2019, 2020)]$value) * 1.05
  # 
  # y_posi_2 <- max(dt_$value) * 1.05
  # range_ <- max(dt_$value)-min(dt_$value)
  # 
  # if(y_posi_2-y_posi_1 < 0.05 * range_) y_posi_2 <- y_posi_2 + 0.1 * range_
  # 
  dt_ %>% 
    .[,year := as.character(year)] %>%
    ggplot (aes(year, value, fill = year)) +
    # geom_hline(yintercept = baseline_point, size = 2, color = "#fc4e2a", alpha = 0.3) +
    geom_boxplot(outlier.alpha = 0.5, outlier.size = 0.5) +
    # geom_half_point(aes(color = year), side = "r", alpha = 0.5, size= 1, transformation = position_jitter(width = 0.5)) +
    
    stat_summary(fun.y = mean, geom= "point", shape= 23, size= 1.3 , 
                 fill = "white", position = position_dodge(width = 0.75)) +
    geom_signif( comparisons = list(c("2019", "2020")),
                 map_signif_level=TRUE, test = "wilcox.test") +
    
    # geom_signif(y_position=y_posi_2, xmin=2.5, xmax=5, annotation = compare_table_year_t[parameter == par & location == "national"]$sign, tip_length=0.03) +
    # geom_signif(y_position=y_posi_2 - range_*0.03 , xmin=1, xmax=4, annotation=c(" "), tip_length=0) +
    # geom_abline(intercept = intercept_, slope = slop_, size = 2, color = "#969696", alpha = 0.5) +
    # geom_point(inherit.aes = FALSE, x = 5, y = baseline_point, shape = 25, size = 2 , fill = "white") +
    # geom_line(inherit.aes = FALSE, data = annotation_line_dt, aes(x = x_, y = y_ ), size = 1, color = "#80b1d3", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    facet_grid(.~month) +
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


p1 <- (boxplot_par_compare_with_last_years_month (dt_daily, "psi_twenty_four_hourly") +  ylab(bquote(bold( PSI ) ))                   ) + coord_cartesian(ylim=c(20,80)) +    geom_signif( comparisons = list(c("2019", "2020")), y_position=80,
                                                                                                                                                                                           map_signif_level=TRUE, test = "wilcox.test") 
p2 <- (boxplot_par_compare_with_last_years_month (dt_daily, "pm10_twenty_four_hourly") +  ylab(bquote(bold( PM[10]~(mu*g/m^3) ) ))     ) + coord_cartesian(ylim=c(5,55)) +    geom_signif( comparisons = list(c("2019", "2020")), y_position=55,
                                                                                                                                                                                           map_signif_level=TRUE, test = "wilcox.test") 
# p3 <- (boxplot_par_compare_with_last_years_month (dt_daily, "pm25_twenty_four_hourly") +  ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))    ) 
p4 <- (boxplot_par_compare_with_last_years_month (dt_daily, "pm25_hourly") +              ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))    ) + coord_cartesian(ylim=c(5,40)) +    geom_signif( comparisons = list(c("2019", "2020")), y_position=40,
                                                                                                                                                                                           map_signif_level=TRUE, test = "wilcox.test") 
p5 <- (boxplot_par_compare_with_last_years_month (dt_daily, "no2_one_hour_max") +        ylab(bquote(bold( NO[2]~(mu*g/m^3)  ) ))     )  
p6 <- (boxplot_par_compare_with_last_years_month (dt_daily[value<2], "co_eight_hour_max") +       ylab(bquote(bold( CO~(mg/m^3)       ) ))      )
p7 <- (boxplot_par_compare_with_last_years_month (dt_daily, "so2_twenty_four_hourly") +  ylab(bquote(bold( SO[2]~(mu*g/m^3) ) ))      ) 
p8 <- (boxplot_par_compare_with_last_years_month (dt_daily, "o3_eight_hour_max") +       ylab(bquote(bold( O[3]~(mu*g/m^3)   ) ))     ) 

p1+p2+p4+p5+p6+p7+p8 + plot_layout(ncol = 1) + plot_layout(guides = "collect") & theme(legend.position = "top")

ggsave("plots/compare_last_years_months_4.pdf", 
       width = 10, height = 15, useDingbats=FALSE)


# meteological data-----------------

# library(clifro)

dt_weather_national[, year := as.character(year)]
with(dt_weather_national[month %in% c("Jan","Feb","Mar","Apr","May") & year != 2016], 
     windrose(WindSpeed, WindDirection, 
              facet = fct_inorder( paste(year,month) ), n_col = 5,
              legend_title = "Wind Speed\n(m/s)",
              legend.title.align = .5,
              ggtheme = "bw") )
ggsave("plots/windrose_Jan_Feb_Mar_Apr.pdf", 
       width = 9, height = 6, useDingbats=FALSE)


dt_weather_national %>%
  .[, .(Tmp = mean(Tmp, na.rm = T)), by = .(datetime = floor_date(datetime, "days"), month, year)] %>%
  ggplot(aes(year, Tmp, fill = year, group = year)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
  facet_grid(.~month) +
  scale_fill_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  geom_signif( comparisons = list(c("2019", "2020")),
               map_signif_level=TRUE, test = "wilcox.test") +
  ylab("Temperature (°C)") +
  mytheme_basic + theme(axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.line.x = element_blank())

ggsave("plots/temp_daily_avg_2.pdf", 
       width = 8, height = 4, useDingbats=FALSE)


dt_weather_national %>%
  .[, .(RH = mean(RH, na.rm = T)), by = .(datetime = floor_date(datetime, "days"), month, year)] %>%
  ggplot(aes(year, RH, fill = year, group = year)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5) +
  facet_grid(.~month) +
  scale_fill_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  geom_signif( comparisons = list(c("2019", "2020")),
               map_signif_level=TRUE, test = "wilcox.test") +
  ylab("RH (%)") +
  mytheme_basic + theme(axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.line.x = element_blank())

ggsave("plots/RH_daily_avg_2.pdf", 
       width = 8, height = 4, useDingbats=FALSE)


dt_weather_national [, month_2 := month]
dt_weather_national [month == "May" & day(datetime) %in% c(1:16), month_2 := "May1"]
dt_weather_national [month == "May" & day(datetime) %in% c(17:31), month_2 := "May2"]
dt_weather_national [, month_2 := fct_reorder(month_2, month(datetime))] 

dt_weather_national %>%
  .[, .(Rainfall = sum(Rainfall, na.rm = T)), by = .(datetime = floor_date(datetime, "days"), month_2, year)] %>%
  .[, .(Rainfall = sum(Rainfall, na.rm = T),
        I = .N), by = .(year, month_2)] %>%
  # .[, .(Rainfall = sum(Rainfall, na.rm = T)), by = .(month, year)] %>%
  ggplot() +
  geom_col( aes(month_2, Rainfall, fill = year ) , position =  position_dodge()) +
  scale_fill_manual (name="Year",
                     # labels= phase_names ,
                     values = color_manual_year) +
  ylab("Monthly rainfall for Singapore (mm)") +
  xlab("Month in the year") +
  mytheme_basic

ggsave("plots/Monthly rainfall_2.pdf", 
       width = 9, height = 5, useDingbats=FALSE)
