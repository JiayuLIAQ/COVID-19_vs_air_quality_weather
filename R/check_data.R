# 2016-03-10 有两遍重复的
test <- dt [, L:=.N, by = .(datetime, parameter, location)]
test$L %>% unique

t <- test[L==2]

# 看看location里的national是啥-----
dt <- rbind(dt, dt [location != "national", .(value = mean(value, na.rm = T),
                        longitude = 0,
                        latitude = 0,
                        location = "national_2"), by = .(datetime, parameter)] ) %>%
            setorder(datetime)

test <- dt  %>%  dcast(datetime + parameter  ~ location, fun.aggregate = mean) 

# national用的是几个站点的最大值！
