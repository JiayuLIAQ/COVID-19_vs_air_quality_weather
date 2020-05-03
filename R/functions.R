library(data.table)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggsignif)
library(patchwork)


# phase-----
phase_variables <- c("before_cb", 
                           "cb")
phase_labels <- c("Before CB", 
                        "CB")

phase_names <- set_names(phase_labels, phase_variables)

color_manual_phase <- c(
                        before_cb = "#fdc086",
                        cb = "#beaed4"
                      )

# year-----
year_variables <- c("before_cb", 
                     "cb")
year_labels <- c("Before CB", 
                  "CB")

year_names <- set_names(year_labels, year_variables)

color_manual_year <- c(
  "2016" = "#fee8c8",
  "2017" = "#fdd49e",
  "2018" = "#fdbb84",
  "2019" = "#fc8d59",
  "2020" = "#beaed4"
)
                      
# parameter ---------------------------------------------------
parameter_variables <- c("pm10_twenty_four_hourly",  
                         "pm25_twenty_four_hourly",
                         "co_eight_hour_max", 
                         "so2_twenty_four_hourly", 
                         "no2_one_hour_max",         
                         "psi_twenty_four_hourly", 
                         "o3_eight_hour_max", 
                         "psi_three_hourly", 
                         "pm25_hourly")

parameter_labels <- c("PM10 (24-hr)",
                      expression(bold("Indoor")),
                        "co_eight_hour_max", 
                        "so2_twenty_four_hourly",
                        "no2_one_hour_max",      
                        "psi_twenty_four_hourly",
                        "o3_eight_hour_max", 
                        "psi_three_hourly", 
                        "pm25_hourly")

parameter_labels_e <- c(expression(bold("Indoor")),
                          expression(bold("Outdoor"))) 

parameter_names <- set_names(parameter_labels, parameter_variables)

# environment_names <- c( indoor  = "Indoor",
#                         outdoor = "Outdoor"
#                         )

environment_alpha <-c( indoor = 1, 
                       outdoor = 0.2)

colors_manual_environment <- c(  
  indoor = "#00BFC4", 
  outdoor = "#F8766D"
) 



#themes---------------------------------------------
mytheme_basic <- theme(
  plot.title = element_text(size = 12, vjust = 0, face = "bold"),
  axis.text.x = element_text(size = 12, hjust=.5, vjust=1, colour="black"),
  axis.text.y = element_text(size = 12, hjust=1, vjust=.5, colour="black"),
  axis.title.y = element_text(size = 12, color = "black", face = "bold", vjust = 0.5, hjust = 0.5),
  axis.title.x = element_text(size = 12, color = "black", face = "bold", vjust = 0.5, hjust = 0.5),
  axis.line = element_line(color = "black"),
  panel.grid.major=element_blank(),
  panel.grid.minor=element_blank(),
  # panel.background=element_rect(fill='white',colour='black'),
  legend.text = element_text(size = 12),
  legend.key = element_rect(colour = NA, fill = "white"),
  panel.background = element_blank(),
  # legend.position = "bottom",
  # legend.direction = "horizontal",
  # legend.key.size= unit(0.3, "cm"),
  # legend.margin = margin(0,0,0,0,"cm"),
  legend.title = element_text(face = "bold", size = 12),
  strip.background = element_rect(colour= NA, fill=NA),
  strip.text = element_text(face = "bold", size = 12)
)
