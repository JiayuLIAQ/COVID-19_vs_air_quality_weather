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
test <- dt[location == "national" & yday %in% same_period & parameter != "psi_three_hourly"]  %>%
                                                    .[, {t <- wilcox.test(value[phase =="before"], value[phase == "cb"])
                                                         p <- t$p.value
                                                         list(p.value = p)
                                                         }, by = .(parameter)]
test[p.value >= 0.05, sign := "NS."]
test[p.value < 0.05 & p.value >= 0.01, sign := "*"]
test[p.value < 0.01 & p.value >= 0.001, sign := "**"]
test[p.value < 0.001, sign := "***"]



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
