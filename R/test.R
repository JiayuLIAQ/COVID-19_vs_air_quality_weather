library(ggplot2)
library(dplyr)
library(ggridges)

# Add columns for year, yday and month
dt [, year := year(datetime)]
dt [, yday := yday(datetime)]
dt [, month := month(datetime, label = T)]

dt[parameter == "no2_one_hour_max"] %>%
ggplot(aes(x = value, y = month, height = ..density..)) +
  geom_density_ridges(stat = "density") +
  facet_wrap(vars(year))


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



# load mobility data------------------

path_all <- data.table(path = list.files(no.. = FALSE, full.names = TRUE, recursive = TRUE, pattern = ".csv"))


dt_mobi_g <- fread(path_all[path %like% "Global_Mobility_Report.csv"]$path ) [country_region == "Singapore"] [, c(5:11)] %>% 
  melt(id.vars = "date", variable.name = "places", value.name = "percent_change_from_baseline")
library(stringr)
dt_mobi_g[, places := str_extract(places, ".*(?=_percent_change)")] 
dt_mobi_g[, date := as_date(date)]
dt_mobi_g[, value_compare_to_baseline := (100 + percent_change_from_baseline)/100]

dt_mobi_g %>%
ggplot() +
  geom_line(aes(date, percent_change_from_baseline, color = places)) +
  geom_vline(xintercept = ymd("2020-04-07"),linetype = 2) +
  mytheme_basic

dt_mobi_g %>%
  ggplot() +
  geom_line(aes(date, value_compare_to_baseline, color = places)) +
  geom_vline(xintercept = ymd("2020-04-07"),linetype = 2) +
  mytheme_basic

dt_mobi_a <- fread(path_all[path %like% "applemobilitytrends"]$path )[region == "Singapore"][,c("geo_type","region","alternative_name"):=NULL] %>%
  melt(id.vars = "transportation_type", variable.name = "date", value.name = "value_compare_to_baseline")
dt_mobi_a[, date := as_date(date)] 

dt_mobi_a %>%
  ggplot() +
  geom_line(aes(date, percent_change_from_baseline, color = transportation_type)) +
  geom_vline(xintercept = ymd("2020-04-07"),linetype = 2) +
  mytheme_basic

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




  
test_2 <- dt_daily[dt_mobi_g, on = .(date)][date >= ymd("2020-03-24"), 
                                          {t.results <- cor.test(value_compare_to_baseline, value, method=c("spearman"))
                                                 rho <- t.results$estimate
                                                   p <- t.results$p.value
                                          list(rho = rho,
                                               p.value = p)}, by =.(parameter, places)]

test <- dt_daily[dt_mobi_g, on = .(date)][date >= ymd("2020-04-01")]
t <- test[parameter=="pm25_hourly" & places == "transit_stations"]
tt <- cor.test(t$percent_change_from_baseline, t$value, method=c("spearman"))
tt$p.value
tt$estimate


  rho <- cov(t$percent_change_from_baseline, t$value) / (sd(t$percent_change_from_baseline) * sd(t$value))
t <- test[parameter=="pm25_hourly" & places == "residential"]
cor.test(t$percent_change_from_baseline, t$value, method=c("spearman"))


test_3 <- dt_daily[dt_mobi_a, on = .(date)][date >= ymd("2020-03-01"), 
                                            {t.results <- cor.test(value_compare_to_baseline, value, method=c("spearman"))
                                            rho <- t.results$estimate
                                            p <- t.results$p.value
                                            list(rho = rho,
                                                 p.value = p)}, by =.(parameter, transportation_type)]


ggscatter(dt_daily[dt_mobi_a, on = .(date)][date >= ymd("2020-03-19")], x = "value_compare_to_baseline", y = "value", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Value_compare_to_baseline", ylab = "Value") +
  facet_wrap(parameter ~ transportation_type, scales = "free")


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
glance(t.model)

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
