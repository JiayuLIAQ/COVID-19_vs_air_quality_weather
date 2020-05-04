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

# t test----

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
                     .[, .(cb_mean = mean(value[phase == "cb"]),
                           before_mean = mean(value[phase =="before"]),
                       delta_mean = mean(value[phase == "cb"]) - mean(value[phase =="before"]),
                       delta_mean_prop = 100 * (mean(value[phase == "cb"]) - mean(value[phase =="before"]))/mean(value[phase =="before"])
                       ), by = .(parameter)]

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
         .[, .(cb_mean = mean(value[phase == "cb"]),
               before_mean = mean(value[phase =="before_cb"]),
               delta_mean = mean(value[phase == "cb"]) - mean(value[phase =="before_cb"]),
               delta_mean_prop = 100 * (mean(value[phase == "cb"]) - mean(value[phase =="before_cb"]))/mean(value[phase =="before_cb"])
         ), by = .(parameter,location)]

compare_table_before_two_weeks <- compare_table_before_two_weeks[t_before_two_weeks, on = .(parameter, location)]  %>% setorder(parameter, location)

# compare_table_before_two_weeks %>% ggplot() +
#   geom_col(aes(location, ))

dt[parameter != "psi_three_hourly" & phase != "before" & !parameter %like% "index" & location != "national"]  %>%
  # .[, .(value = mean(value, na.rm = T)
  # ), by = .(parameter,location, phase)] %>% 
  ggplot(aes(location, value, fill = phase)) +
  # geom_col(position = "dodge") +
  # geom_bar(position = "dodge", stat = "identity") +
  stat_summary(aes(group = phase), fun.y = mean, geom= "bar", 
               position = position_dodge(width = 0.5)) +
  stat_compare_means(aes(group = phase), label = "p.signif", method = "wilcox.test") +
  # coord_cartesian(ylim = c(30,55))+
  facet_wrap(vars(parameter_fct), scales = "free", labeller = label_parsed) +
  mytheme_basic
 
symnum.args <- list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "NS."))

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
