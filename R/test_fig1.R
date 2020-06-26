# v1------------------------------------------------

boxplot_par_compare_with_last_years <- function(dt_, par, loc_ = "central"){
  
  dt_ <- dt_[location == loc_ & yday %in% same_period & parameter == par ] 
  # .[, .(value = mean(value, na.rm = T) ), by =.(datetime = date(datetime), parameter, year, parameter) ]
  
  intercept_ <- lm_year_dt[parameter == par & location == loc_]$intercept 
  slop_ <- lm_year_dt[parameter == par & location == loc_]$slop
  covid_point <- lm_year_dt[parameter == par & location == loc_ ]$covid_5 
  

    baseline_point_1 <- lm_year_dt[parameter == par & location == loc_ ]$predicted_5

    baseline_point_2 <- compare_table_year_t[parameter == par & location == loc_ ]$before_mean 
  
  
  annotation_line_dt <- data.table (x_ = c(5.7,5.7),
                                    y_ = c(baseline_point_1, covid_point))
  
  y_posi_1 <- max(dt_[year %in% c(2019, 2020)]$value) * 1.05
  
  y_posi_2 <- max(dt_$value) * 1.05
  range_ <- max(dt_$value)-min(dt_$value)
  
  if(y_posi_2-y_posi_1 < 0.05 * range_) y_posi_2 <- y_posi_2 + 0.1 * range_
  
  dt_ %>% 
    .[,year := as.character(year)] %>%
    ggplot (aes(year, value, fill = year)) +
    geom_hline(yintercept = baseline_point_2, size = 2, color = "#fc4e2a", alpha = 0.3) +
    geom_half_boxplot(outlier.alpha = 0, outlier.size = 0.5) +
    # geom_half_violin(side = "r") +
    geom_half_point(aes(color = year), side = "r", alpha = 0.5, size= 1, transformation = position_jitter(width = 0.5)) +
    stat_summary(fun = mean, geom= "point", shape= 23, size= 2 , 
                 fill = "white", position = position_dodge(width = 0.75)) +
    geom_signif(y_position = y_posi_1, comparisons = list(c("2019", "2020")),
                map_signif_level=TRUE, test = "wilcox.test") +
    
    geom_signif(y_position=y_posi_2, xmin=2.5, xmax=5, annotation = compare_table_year_t[parameter == par & location == loc_]$sign, tip_length=0.03) +
    geom_signif(y_position=y_posi_2 - range_*0.03 , xmin=1, xmax=4, annotation=c(" "), tip_length=0) +
    geom_abline(intercept = intercept_, slope = slop_, size = 2, color = "#969696", alpha = 0.5) +
    geom_point(inherit.aes = FALSE, x = 5, y = baseline_point_1, shape = 25, size = 2 , fill = "white") +
    geom_line(inherit.aes = FALSE, data = annotation_line_dt, aes(x = x_, y = y_ ), size = 1, color = "#80b1d3", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    
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

p1 <- (boxplot_par_compare_with_last_years(dt_daily, "psi_twenty_four_hourly") +  ylab(bquote(bold( PSI ) ))                   ) # %>%  delete_layers(.,"GeomHline" )
p2 <- (boxplot_par_compare_with_last_years(dt_daily, "pm10_twenty_four_hourly") +  ylab(bquote(bold( PM[10]~(mu*g/m^3) ) ))     )#  %>%  delete_layers(.,"GeomHline" )
p3 <- (boxplot_par_compare_with_last_years(dt_daily, "pm25_twenty_four_hourly") +  ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))    )#  %>%  delete_layers(.,"GeomHline" ) 
p4 <- (boxplot_par_compare_with_last_years(dt_daily, "pm25_hourly") +              ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))    )#  %>%  delete_layers(.,"GeomAbline" ) 
p5 <- (boxplot_par_compare_with_last_years(dt_daily, "no2_one_hour_max") +        ylab(bquote(bold( NO[2]~(mu*g/m^3)  ) ))     ) # %>%  delete_layers(.,"GeomAbline" ) 
p6 <- (boxplot_par_compare_with_last_years(dt_daily, "co_eight_hour_max") +       ylab(bquote(bold( CO~(mg/m^3)       ) ))      )#  %>%  delete_layers(.,"GeomAbline")
p7 <- (boxplot_par_compare_with_last_years(dt_daily, "so2_twenty_four_hourly") +  ylab(bquote(bold( SO[2]~(mu*g/m^3) ) ))      ) # %>%  delete_layers(.,"GeomAbline")
p8 <- (boxplot_par_compare_with_last_years(dt_daily, "o3_eight_hour_max") +       ylab(bquote(bold( O[3]~(mu*g/m^3)   ) ))     ) # %>%   delete_layers(.,"GeomHline" ) 


p1+p2+p3 + guide_area() + p5+p6+p7+p8 + plot_layout(nrow = 2) + plot_layout(guides = "collect") 



# v2------------------------------------------------
t_table_years <- dt_daily[yday %in% same_period & parameter != "psi_three_hourly"]  %>%
  .[, {t <- wilcox.test(value[phase =="before"], value[phase == "cb"])
  p <- t$p.value
  list(p.value = p)
  }, by = .(parameter, location)]
t_table_years[p.value >= 0.05, sign := "NS."]
t_table_years[p.value < 0.05 & p.value >= 0.01, sign := "*"]
t_table_years[p.value < 0.01 & p.value >= 0.001, sign := "**"]
t_table_years[p.value < 0.001, sign := "***"]

compare_table_years <- dt_daily[yday %in% same_period & parameter != "psi_three_hourly"]  %>%
  .[, .(cb_mean = mean(value[phase == "cb"], na.rm = T),
        before_mean = mean(value[phase =="before"], na.rm = T),
        delta_mean = mean(value[phase == "cb"], na.rm = T) - mean(value[phase =="before"], na.rm = T),
        delta_mean_pctg = 100 * (mean(value[phase == "cb"], na.rm = T) - mean(value[phase =="before"], na.rm = T))/mean(value[phase =="before"], na.rm = T)
  ), by = .(parameter, location)]

compare_table_year_t <- compare_table_years[t_table_years, on = .(parameter, location)]  %>% setorder(location, parameter)

year_index_dt <- data.table(year = c(2016:2020),
                            index = c(1:5) )

lm_year_dt <- year_index_dt[dt_daily, on = .(year)][ yday %in% same_period] %>%
  .[, .(value = mean(value, na.rm = T) ), by = .(index, year, location, parameter)] %>%
  .[, { model <- lm(value ~ index, data = .SD[index != 5 ])
  new_dt <- data.table(index = 5)
  predicted_5 <- predict(model, new_dt)
  # predicted_5_05 <- predict(model, new_dt, interval = "confidence" )[2]
  # predicted_5_95 <- predict(model, new_dt, interval = "confidence" )[3]
  covid_change <- .SD[index == 5 ]$value - predicted_5 
  # covid_change_pctg <- 100 * covid_change/.SD[index == 5 ]$value
  covid_change_pctg <- 100 * covid_change/predicted_5 
  
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
  )}, by = .(location, parameter)] %>% setorder(location, parameter)


# fig_1 final-----
# need to load compare_table_year_t 
boxplot_par_compare_with_last_years <- function(dt_, par){
  
  dt_ <- dt_[location == "national" & yday %in% same_period & parameter == par ] 
  # .[, .(value = mean(value, na.rm = T) ), by =.(datetime = date(datetime), parameter, year, parameter) ]
  
  intercept_ <- lm_year_dt[parameter == par & location == "national"]$intercept 
  slop_ <- lm_year_dt[parameter == par & location == "national"]$slop
  covid_point <- lm_year_dt[parameter == par & location == "national" ]$covid_5 
  if (lm_year_dt[parameter == par & location == "national" ]$r.squared > 0.6) {
    baseline_point <- lm_year_dt[parameter == par & location == "national" ]$predicted_5
  } else {
    baseline_point <- compare_table_year_t[parameter == par & location == "national" ]$before_mean 
  }
  
  annotation_line_dt <- data.table (x_ = c(5.7,5.7),
                                    y_ = c(baseline_point, covid_point))
  
  y_posi_1 <- max(dt_[year %in% c(2019, 2020)]$value) * 1.05
  
  y_posi_2 <- max(dt_$value) * 1.05
  range_ <- max(dt_$value)-min(dt_$value)
  
  if(y_posi_2-y_posi_1 < 0.05 * range_) y_posi_2 <- y_posi_2 + 0.1 * range_
  
  dt_ %>% 
    .[,year := as.character(year)] %>%
    ggplot (aes(year, value, fill = year)) +
    geom_hline(yintercept = baseline_point, size = 2, color = "#fc4e2a", alpha = 0.3) +
    geom_half_boxplot(outlier.alpha = 0, outlier.size = 0.5) +
    # geom_half_violin(side = "r") +
    geom_half_point(aes(color = year), side = "r", alpha = 0.5, size= 1, transformation = position_jitter(width = 0.5)) +
    stat_summary(fun = mean, geom= "point", shape= 23, size= 2 , 
                 fill = "white", position = position_dodge(width = 0.75)) +
    geom_signif(y_position = y_posi_1, comparisons = list(c("2019", "2020")),
                map_signif_level=TRUE, test = "wilcox.test") +
    
    geom_signif(y_position=y_posi_2, xmin=2.5, xmax=5, annotation = compare_table_year_t[parameter == par & location == "national"]$sign, tip_length=0.03) +
    geom_signif(y_position=y_posi_2 - range_*0.03 , xmin=1, xmax=4, annotation=c(" "), tip_length=0) +
    geom_abline(intercept = intercept_, slope = slop_, size = 2, color = "#969696", alpha = 0.5) +
    geom_point(inherit.aes = FALSE, x = 5, y = baseline_point, shape = 25, size = 2 , fill = "white") +
    geom_line(inherit.aes = FALSE, data = annotation_line_dt, aes(x = x_, y = y_ ), size = 1, color = "#80b1d3", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    
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

p1 <- (boxplot_par_compare_with_last_years(dt_daily, "psi_twenty_four_hourly") +  ylab(bquote(bold( PSI ) ))                   ) %>%  delete_layers(.,"GeomHline" )
p2 <- (boxplot_par_compare_with_last_years(dt_daily, "pm10_twenty_four_hourly") +  ylab(bquote(bold( PM[10]~(mu*g/m^3) ) ))     ) %>%  delete_layers(.,"GeomHline" )
p3 <- (boxplot_par_compare_with_last_years(dt_daily, "pm25_twenty_four_hourly") +  ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))    ) %>%  delete_layers(.,"GeomHline" ) 
p4 <- (boxplot_par_compare_with_last_years(dt_daily, "pm25_hourly") +              ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))    ) %>%  delete_layers(.,"GeomAbline" ) 
p5 <- (boxplot_par_compare_with_last_years(dt_daily, "no2_one_hour_max") +        ylab(bquote(bold( NO[2]~(mu*g/m^3)  ) ))     ) %>%  delete_layers(.,"GeomAbline" ) 
p6 <- (boxplot_par_compare_with_last_years(dt_daily, "co_eight_hour_max") +       ylab(bquote(bold( CO~(mg/m^3)       ) ))      ) %>%  delete_layers(.,"GeomAbline")
p7 <- (boxplot_par_compare_with_last_years(dt_daily, "so2_twenty_four_hourly") +  ylab(bquote(bold( SO[2]~(mu*g/m^3) ) ))      ) %>%  delete_layers(.,"GeomAbline")
p8 <- (boxplot_par_compare_with_last_years(dt_daily, "o3_eight_hour_max") +       ylab(bquote(bold( O[3]~(mu*g/m^3)   ) ))     ) %>%   delete_layers(.,"GeomHline" ) 

# p1+p2+p3+p4+p5+p6+p7+p8 + plot_layout(nrow = 2) + plot_layout(guides = "collect") 

p1+p2+p3 + guide_area() + p5+p6+p7+p8 + plot_layout(nrow = 2) + plot_layout(guides = "collect") 

ggsave("plots/compare_last_years_7.pdf", 
       width = 8, height = 6, useDingbats=FALSE)

# table 2---------------------------------
lm_year_dt <- year_index_dt[dt_daily, on = .(year)][ yday %in% same_period] %>%
  .[, .(value = mean(value, na.rm = T) ), by = .(index, year, location, parameter)] %>%
  .[, { model <- lm(value ~ index, data = .SD[index != 5 ])
  new_dt <- data.table(index = 5)
  predicted_5 <- predict(model, new_dt)
  # predicted_5_05 <- predict(model, new_dt, interval = "confidence" )[2]
  # predicted_5_95 <- predict(model, new_dt, interval = "confidence" )[3]
  covid_change <- .SD[index == 5 ]$value - predicted_5 
  # covid_change_pctg <- 100 * covid_change/.SD[index == 5 ]$value
  covid_change_pctg <- 100 * covid_change/predicted_5 
  
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
    delta_mean = covid_change,
    delta_mean_pctg = covid_change_pctg,
    r.squared = summary(model)$r.squared,
    p.value = glance(model)$p.value,
    slop = slop,
    slop_05 = slop_05,
    slop_95 = slop_95,
    intercept = intercept,
    intercept_05 = intercept_05,
    intercept_95 = intercept_95
  )}, by = .(location, parameter)] %>% setorder(location, parameter)


lm_year_dt [r.squared > 0.6, baseline_type := "predicted"]
lm_year_dt [r.squared < 0.6, baseline_type := "avg_4_yr"]

lm_year_dt [r.squared > 0.5, baseline_type := "predicted"]
lm_year_dt [r.squared < 0.5, baseline_type := "avg_4_yr"]

reduction_dt <- rbind(
  lm_year_dt[baseline_type == "predicted"] [, c("location", "parameter", "delta_mean", "delta_mean_pctg", "baseline_type")],
  compare_table_year_t[lm_year_dt [baseline_type == "avg_4_yr", c("location","parameter", "baseline_type")] , on =.(location, parameter)] %>% 
    .[, c("location", "parameter", "delta_mean", "delta_mean_pctg", "baseline_type", "sign")], fill = T) %>% setorder(location, parameter)

write_file(reduction_dt, "./plots/reduction_dt_3.csv")


#v3----------------------------------


year_index_dt[dt_daily, on = .(year)][ yday %in% same_period] %>% .[index !=5 ] %>%
  .[, .(value = mean(value, na.rm = T) ), by = .(index, year, location, parameter)] %>%
  ggplot(aes(index, value, color = location)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free") +
  geom_smooth(aes(group = location), method = lm, se = T, fill = "#A0A0A0") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), r.digits = 2) +
  mytheme_basic
  

year_index_dt[dt_daily, on = .(year)][ yday %in% same_period] %>% .[index !=5 & location == "national" & parameter != "pm25_hourly"] %>%
  .[, .(value = mean(value, na.rm = T) ), by = .(index, year, location, parameter)] %>%
  ggplot(aes(index, value, color = location)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free") +
  geom_smooth(aes(group = location), method = lm, se = T, fill = "#A0A0A0") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), r.digits = 2) +
  # stat_stars () +
  mytheme_basic
  

# compare table----------------------------------------------------------------------
# dt_daily[yday %in% same_period & parameter == "psi_twenty_four_hourly" & value >= 50 & location == "national"]
# dt_daily[yday %in% same_period & parameter == "psi_twenty_four_hourly" & value >= 0 & location == "national"]
# 83/127 *100

#compare with same period last years
t_table_years <- dt_daily[yday %in% same_period & parameter != "psi_three_hourly"]  %>%
  .[, {t <- wilcox.test(value[phase =="before"], value[phase == "cb"])
  p <- t$p.value
  list(p.value = p)
  }, by = .(parameter, location)]
t_table_years[p.value >= 0.05, sign := "NS."]
t_table_years[p.value < 0.05 & p.value >= 0.01, sign := "*"]
t_table_years[p.value < 0.01 & p.value >= 0.001, sign := "**"]
t_table_years[p.value < 0.001, sign := "***"]

compare_table_years <- dt_daily[yday %in% same_period & parameter != "psi_three_hourly"]  %>%
  .[, .(cb_mean = mean(value[phase == "cb"], na.rm = T),
        before_mean = mean(value[phase =="before"], na.rm = T),
        delta_mean = mean(value[phase == "cb"], na.rm = T) - mean(value[phase =="before"], na.rm = T),
        delta_mean_pctg = 100 * (mean(value[phase == "cb"], na.rm = T) - mean(value[phase =="before"], na.rm = T))/mean(value[phase =="before"], na.rm = T)
  ), by = .(parameter, location)]

compare_table_year_t <- compare_table_years[t_table_years, on = .(parameter, location)]  %>% setorder(location, parameter)

year_index_dt <- data.table(year = c(2016:2020),
                            index = c(1:5) )

lm_year_dt <- year_index_dt[dt_daily, on = .(year)][ yday %in% same_period] %>%
  # .[, .(value = mean(value, na.rm = T) ), by = .(index, year, location, parameter)] %>%
  .[, { model <- lm(value ~ index, data = .SD[index != 5 ])
  new_dt <- data.table(index = 5)
  predicted_5 <- predict(model, new_dt)
  # predicted_5_05 <- predict(model, new_dt, interval = "confidence" )[2]
  # predicted_5_95 <- predict(model, new_dt, interval = "confidence" )[3]
  covid_change <- mean(.SD[index == 5 ]$value) - predicted_5 
  # covid_change_pctg <- 100 * covid_change/.SD[index == 5 ]$value
  covid_change_pctg <- 100 * covid_change/predicted_5 
  
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
    covid_5 = mean(.SD[index == 5 ]$value),
    covid_change = covid_change,
    covid_change_pctg = covid_change_pctg,
    r.squared = summary(model)$r.squared,
    p.value = glance(model)$p.value,
    slop = slop,
    slop_05 = slop_05,
    slop_95 = slop_95,
    intercept = intercept,
    intercept_05 = intercept_05,
    intercept_95 = intercept_95
  )}, by = .(location, parameter)] %>% setorder(location, parameter)


# fig_1 final-----
# need to load compare_table_year_t 
boxplot_par_compare_with_last_years <- function(dt_, par){
  
  dt_ <- dt_[location == "national" & yday %in% same_period & parameter == par ] 
  # .[, .(value = mean(value, na.rm = T) ), by =.(datetime = date(datetime), parameter, year, parameter) ]
  
  intercept_ <- lm_year_dt[parameter == par & location == "national"]$intercept 
  slop_ <- lm_year_dt[parameter == par & location == "national"]$slop
  covid_point <- lm_year_dt[parameter == par & location == "national" ]$covid_5 
  if (lm_year_dt[parameter == par & location == "national" ]$p.value < 0.05) {
    baseline_point <- lm_year_dt[parameter == par & location == "national" ]$predicted_5
  } else {
    baseline_point <- compare_table_year_t[parameter == par & location == "national" ]$before_mean 
  }
  
  annotation_line_dt <- data.table (x_ = c(5.7,5.7),
                                    y_ = c(baseline_point, covid_point))
  
  y_posi_1 <- max(dt_[year %in% c(2019, 2020)]$value) * 1.05
  
  y_posi_2 <- max(dt_$value) * 1.05
  range_ <- max(dt_$value)-min(dt_$value)
  
  if(y_posi_2-y_posi_1 < 0.05 * range_) y_posi_2 <- y_posi_2 + 0.1 * range_
  
  dt_ %>% 
    .[,year := as.character(year)] %>%
    ggplot (aes(year, value, fill = year)) +
    geom_hline(yintercept = baseline_point, size = 2, color = "#fc4e2a", alpha = 0.3) +
    geom_half_boxplot(outlier.alpha = 0, outlier.size = 0.5) +
    # geom_half_violin(side = "r") +
    geom_half_point(aes(color = year), side = "r", alpha = 0.5, size= 1, transformation = position_jitter(width = 0.5)) +
    stat_summary(fun = mean, geom= "point", shape= 23, size= 2 , 
                 fill = "white", position = position_dodge(width = 0.75)) +
    geom_signif(y_position = y_posi_1, comparisons = list(c("2019", "2020")),
                map_signif_level=TRUE, test = "wilcox.test") +
    
    geom_signif(y_position=y_posi_2, xmin=2.5, xmax=5, annotation = compare_table_year_t[parameter == par & location == "national"]$sign, tip_length=0.03) +
    geom_signif(y_position=y_posi_2 - range_*0.03 , xmin=1, xmax=4, annotation=c(" "), tip_length=0) +
    geom_abline(intercept = intercept_, slope = slop_, size = 2, color = "#969696", alpha = 0.5) +
    geom_point(inherit.aes = FALSE, x = 5, y = baseline_point, shape = 25, size = 2 , fill = "white") +
    geom_line(inherit.aes = FALSE, data = annotation_line_dt, aes(x = x_, y = y_ ), size = 1, color = "#80b1d3", arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    
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

p1 <- (boxplot_par_compare_with_last_years(dt_daily, "psi_twenty_four_hourly") +  ylab(bquote(bold( PSI ) ))                   ) %>%  delete_layers(.,"GeomHline" )
p2 <- (boxplot_par_compare_with_last_years(dt_daily, "pm10_twenty_four_hourly") +  ylab(bquote(bold( PM[10]~(mu*g/m^3) ) ))     ) %>%  delete_layers(.,"GeomAbline" )
p3 <- (boxplot_par_compare_with_last_years(dt_daily, "pm25_twenty_four_hourly") +  ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))    ) %>%  delete_layers(.,"GeomHline" ) 
p4 <- (boxplot_par_compare_with_last_years(dt_daily, "pm25_hourly") +              ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))    ) %>%  delete_layers(.,"GeomHline" )  
p5 <- (boxplot_par_compare_with_last_years(dt_daily, "no2_one_hour_max") +        ylab(bquote(bold( NO[2]~(mu*g/m^3)  ) ))     ) %>%  delete_layers(.,"GeomAbline" ) 
p6 <- (boxplot_par_compare_with_last_years(dt_daily, "co_eight_hour_max") +       ylab(bquote(bold( CO~(mg/m^3)       ) ))      ) %>%  delete_layers(.,"GeomHline" ) 
p7 <- (boxplot_par_compare_with_last_years(dt_daily, "so2_twenty_four_hourly") +  ylab(bquote(bold( SO[2]~(mu*g/m^3) ) ))      ) %>%  delete_layers(.,"GeomHline" ) 
p8 <- (boxplot_par_compare_with_last_years(dt_daily, "o3_eight_hour_max") +       ylab(bquote(bold( O[3]~(mu*g/m^3)   ) ))     ) %>%   delete_layers(.,"GeomHline" )

# p1+p2+p3+p4+p5+p6+p7+p8 + plot_layout(nrow = 2) + plot_layout(guides = "collect") 

p1+p2+p3 + guide_area() + p5+p6+p7+p8 + plot_layout(nrow = 2) + plot_layout(guides = "collect") 

ggsave("plots/compare_last_years_10.pdf", 
       width = 8, height = 6, useDingbats=FALSE)


# table 2---------------------------------
lm_year_dt <- year_index_dt[dt_daily, on = .(year)][ yday %in% same_period] %>%
  # .[, .(value = mean(value, na.rm = T) ), by = .(index, year, location, parameter)] %>%
  .[, { model <- lm(value ~ index, data = .SD[index != 5 ])
  new_dt <- data.table(index = 5)
  predicted_5 <- predict(model, new_dt)
  # predicted_5_05 <- predict(model, new_dt, interval = "confidence" )[2]
  # predicted_5_95 <- predict(model, new_dt, interval = "confidence" )[3]
  covid_change <- mean(.SD[index == 5 ]$value) - predicted_5 
  # covid_change_pctg <- 100 * covid_change/.SD[index == 5 ]$value
  covid_change_pctg <- 100 * covid_change/predicted_5 
  
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
    covid_5 = mean(.SD[index == 5 ]$value),
    delta_mean = covid_change,
    delta_mean_pctg = covid_change_pctg,
    r.squared = summary(model)$r.squared,
    p.value = glance(model)$p.value,
    slop = slop,
    slop_05 = slop_05,
    slop_95 = slop_95,
    intercept = intercept,
    intercept_05 = intercept_05,
    intercept_95 = intercept_95
  )}, by = .(location, parameter)] %>% setorder(location, parameter)


lm_year_dt [p.value < 0.01, baseline_type := "predicted"]
lm_year_dt [p.value > 0.01, baseline_type := "avg_4_yr"]

# lm_year_dt %>% write_file("lm_year_dt.csv")
# lm_year_dt[p.value >= 0.05, sign := "NS."]
# lm_year_dt[p.value < 0.05 & p.value >= 0.01, sign := "*"]
# lm_year_dt[p.value < 0.01 & p.value >= 0.001, sign := "**"]
# lm_year_dt[p.value < 0.001, sign := "***"]

lm_year_dt [location == "national" & parameter == "o3_eight_hour_max", baseline_type := "predicted"]

compare_table_year_t[, counterfactual := before_mean]
lm_year_dt[, `:=` (counterfactual = predicted_5,
                   cb_mean = covid_5)]

reduction_dt <- rbind(
  lm_year_dt[baseline_type == "predicted"] [, c("location", "parameter", "counterfactual", "cb_mean", "delta_mean", "delta_mean_pctg", "baseline_type")],
  compare_table_year_t[lm_year_dt [baseline_type == "avg_4_yr", c("location","parameter", "counterfactual", "cb_mean", "baseline_type")] , on =.(location, parameter)] %>% 
    .[, c("location", "parameter", "counterfactual", "cb_mean", "delta_mean", "delta_mean_pctg", "baseline_type", "sign")], fill = T) %>% setorder(location, parameter)

reduction_dt[is.na(sign), sign := "&"]
reduction_dt[, conterfactual_2 := paste( round(counterfactual, 1), sign)]
reduction_dt[, cb_mean :=  round(cb_mean, 1)]
reduction_dt[, delta_mean_pctg := round(delta_mean_pctg)]

write_file(reduction_dt[parameter != "pm25_twenty_four_hourly", c("location","parameter","conterfactual_2","cb_mean", "delta_mean_pctg")], "./plots/reduction_dt_8_1.csv")
