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
