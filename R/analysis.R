source("./R/functions.R")
source("./R/load_data.R")

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
  if (lm_year_dt[parameter == par & location == "national" ]$r.squared > 0.5) {
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

ggsave("plots/compare_last_years_8.pdf", 
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


lm_year_dt [r.squared > 0.5, baseline_type := "predicted"]
lm_year_dt [r.squared < 0.5, baseline_type := "avg_4_yr"]

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



write_file(reduction_dt[parameter != "pm25_twenty_four_hourly", c("location","parameter","conterfactual_2","cb_mean", "delta_mean_pctg")], "./plots/reduction_dt_7.csv")


# correlation between air quality and mobility---------------------------------
# figure 2----

start <- ymd("2020-03-23")
end <- ymd("2020-05-11")

trend_plot_fun <- function(dt_, par, start_ = start, end_  = end) {
  min_ <- dt_[location == "national" &   date >= start_ & date <= end_ & parameter == par]$value %>% min 
  max_ <- dt_[location == "national" &   date >= start_ & date <= end_ & parameter == par]$value %>% max
  range_ <- max_-min_
  dt_[location == "national" &  date >= start_ & date <= end_ & parameter == par] %>%  
    ggplot () +
    # geom_area(aes(datetime, value, fill = parameter)) +
    geom_ribbon(aes(date, ymin = max(min_-range_ * 0.3,0), ymax = value, fill = parameter, color = parameter), alpha = 0.5) +
    geom_vline(xintercept = ymd("2020-04-07"), linetype = 2 ) +
    scale_x_date (expand = c(0,0), date_breaks = "1 weeks", date_labels = "%b %d") +
    scale_y_continuous(expand = c(0,0)) +
    scale_color_manual (name= NULL,
                       labels= parameter_names ,
                       values = color_manual_parameter) +
    scale_fill_manual (name= NULL,
                       labels= parameter_names ,
                       values = color_manual_parameter) +
    mytheme_basic 
}

p1 <- trend_plot_fun(dt_daily, "psi_twenty_four_hourly") +  ylab(bquote(bold( PSI ) )) 
p3 <- trend_plot_fun(dt_daily, "pm25_twenty_four_hourly") +              ylab(bquote(bold( PM[2.5]~(mu*g/m^3) ) ))  
p2 <- trend_plot_fun(dt_daily, "pm10_twenty_four_hourly") +        ylab(bquote(bold( PM[10]~(mu*g/m^3)  ) ))
p5 <- trend_plot_fun(dt_daily, "co_eight_hour_max") +        ylab(bquote(bold( CO~(mg/m^3)  ) ))  
p6 <- trend_plot_fun(dt_daily, "so2_twenty_four_hourly") +        ylab(bquote(bold( SO[2]~(mu*g/m^3)  ) ))  
p4 <- trend_plot_fun(dt_daily, "no2_one_hour_max") +        ylab(bquote(bold( NO[2]~(mu*g/m^3)  ) ))  
p7 <- trend_plot_fun(dt_daily, "o3_eight_hour_max") +        ylab(bquote(bold( O[3]~(mu*g/m^3)  ) ))  

p1+p2+p3+p4+p5+p6+p7 + plot_layout(ncol = 2) & guides(fill = F, color = F) &
   theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank())

pp1 <- p1 + p2+p3+p4+p5+p6+p7 &  # guides(fill = F, color = F) &
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank())

pp1 + p_mobi + guide_area() + plot_layout(ncol = 2, heights = c(1,1,1,2), guides= "collect")

pp1 + p_mobi + plot_layout(ncol = 1, heights = c(1,1,1,1,1,1,1,3))


pp1 <-  p1+p2+p3+p4+p5+p6+p7   &
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank())

pp1 <-  p2+p5+p6   &
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x=element_blank(),
        axis.title.x = element_blank())

p_mobi <- dt_mobi[date >= start & date <= end] %>%
  ggplot () +
  # geom_area(aes(datetime, value, fill = parameter)) +
  geom_line(aes(date, mobi_value, color = item), size = 1.5) +
  geom_vline(xintercept = ymd("2020-04-07"), linetype = 2 ) +
  scale_x_date (name = "Date", expand = c(0,0), date_breaks = "1 weeks", date_labels = "%b %d") +
  scale_y_continuous(name = "Mobility levels", expand = c(0.05,0)) +
  # facet_grid(dataset_source~. ,scales = "free") +
  scale_color_manual (name= NULL,
                      labels= item_names ,
                      values = color_manual_item) +
  mytheme_basic 

pp1 + p4  + plot_layout(ncol = 1, heights = c(1,1,1,2)) + plot_layout(guides = "collect")

ggsave("plots/trend_air_quality_mobility_6.pdf", 
       width = 9, height = 8, useDingbats=FALSE)


# table 2 spearman correlation table -----
spearman_table <- dt_daily[dt_mobi, on = .(date)][date >= start & date <= end , 
                                            {t.results <- cor.test(mobi_value, value, method=c("spearman"))
                                            rho <- t.results$estimate
                                            p <- t.results$p.value
                                            list(rho = rho,
                                                 p.value = p)}, by =.(parameter, item)]

spearman_table[p.value >= 0.05, sign := "NS."]
spearman_table[p.value < 0.05 & p.value >= 0.01, sign := "*"]
spearman_table[p.value < 0.01 & p.value >= 0.001, sign := "**"]
spearman_table[p.value < 0.001, sign := "***"]

spearman_table[, rho_star := paste(round(rho,2), sign)] %>% 
  setorder(item, parameter) %>%
  dcast(parameter ~ item, value.var = "rho_star") %>%
write_file(., "./plots/spearman_table_7.csv")


ggscatter(dt_daily[dt_mobi_g, on = .(date)][date >= ymd("2020-03-20")], x = "value_compare_to_baseline", y = "value", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Value_compare_to_baseline", ylab = "Value", alpha = 0.2) +
  facet_grid(parameter ~ places, scales = "free")


test_3 <- dt_daily[dt_mobi_a, on = .(date)][date >= ymd("2020-03-20"), 
                                            {t.results <- cor.test(value_compare_to_baseline, value, method=c("spearman"))
                                            rho <- t.results$estimate
                                            p <- t.results$p.value
                                            list(rho = rho,
                                                 p.value = p)}, by =.(parameter, transportation_type)]

ggscatter(dt_daily[dt_mobi_a, on = .(date)][date >= ymd("2020-03-20")], x = "value_compare_to_baseline", y = "value", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Value_compare_to_baseline", ylab = "Value", alpha = 0.2) +
  facet_grid(parameter ~ transportation_type, scales = "free")


#compare with two weeks before CB------------------------------------
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
  stat_summary(aes(group = phase), fun = mean, geom= "bar", 
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
  stat_summary(aes(group = phase), fun = mean, geom= "point", shape= 23, size= 2 , 
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
  stat_summary(fun = mean, geom= "point", shape= 23, size= 2 ,
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
  stat_summary(fun = mean, geom= "point", shape= 23, size= 2 , 
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
  stat_summary(fun = mean, geom= "point", shape= 23, size= 2 , 
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
  stat_summary(fun = mean, geom= "point", shape= 23, size= 2 , 
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


  