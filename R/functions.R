library(data.table)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggsignif)
library(gghalves)
library(patchwork)
library(ggpubr)
library(gginnards)
library(stringr)
library(clifro)
library(forcats)
library(broom)

write_file <- function (d, file, datetimeformat = "ISO") {
  file <- normalizePath(file, mustWork = FALSE)
  if( !(file %>% dirname() %>%dirname() %>% dirname() %>% dir.exists()) ) {
    file %>% dirname() %>%dirname() %>% dirname() %>% dir.create()
  }
  if( !(file %>% dirname() %>% dirname() %>% dir.exists()) ) {
    file %>% dirname() %>% dirname() %>% dir.create()
  }
  if ( !( file %>% dirname() %>% dir.exists()) ) {
    file %>% dirname() %>% dir.create()
  }
  fwrite(d, file, dateTimeAs = datetimeformat)
  file
}



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

# locations-----
location_variables <- c("national", 
                       "central",
                       "east",
                       "west",
                       "south",
                       "north")
location_labels <-c("National", 
                    "Central",
                    "East",
                    "West",
                    "South",
                    "North")

location_names <- set_names(location_labels, location_variables)

# color_manual_location <- c(
#   "2016" = "#fee8c8",
#   "2017" = "#fdd49e",
#   "2018" = "#fdbb84",
#   "2019" = "#fc8d59",
#   "2020" = "#beaed4"
# )
                      
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

parameter_labels_e <- c(expression( bold(24-hr~PM[10]~(mu*g/m^3) )),  
                      expression( bold(24-hr~PM[2.5]~(mu*g/m^3) )),
                      expression( bold(8-hr~CO~(mg/m^3) )),
                      expression( bold(24-hr~SO[2]~(mu*g/m^3) )),
                      expression( bold(1-hr~NO[2]~(mu*g/m^3) )),        
                      expression( bold(24-hr~PSI~index ) ), 
                      expression( bold(8-hr~O[3]~(mu*g/m^3) )),
                      expression( bold(3-hr~PSI~index ) ),
                      expression( bold(1-hr~PM[2.5]~(mu*g/m^3) ))
                      )

parameter_labels <- c(expression( bold(PM[10]*" ("*mu*g/m^3*")")),  
                      expression( bold(PM[2.5]*" ("*mu*g/m^3*") (24 hr)")),
                      expression( bold(CO*" (mg/"*m^3*")")),
                      expression( bold(SO[2]*" ("*mu*g/m^3*")")),
                      expression( bold(NO[2]*" ("*mu*g/m^3*")")),        
                      expression( bold("PSI index") ), 
                      expression( bold(O[3]*" ("*mu*g/m^3*")")),
                      expression( bold("PSI index (3 hr)") ),
                      expression( bold(PM[2.5]*" ("*mu*g/m^3*")"))
)

parameter_names <- set_names(parameter_labels, parameter_variables)

color_manual_parameter <- c("pm10_twenty_four_hourly" = "#e5c494",
                            "pm25_twenty_four_hourly" = "#a6d854",
                            "co_eight_hour_max" =       "#66c2a5",
                            "so2_twenty_four_hourly" =  "#ffd92f",
                            "no2_one_hour_max"   =      "#fc8d62",
                            "psi_twenty_four_hourly" =  "#e78ac3",
                            "o3_eight_hour_max" =       "#8da0cb",
                            "psi_three_hourly" =        "black",
                            "pm25_hourly" =             "#b3b3b3")

# items-------------------------------
item_variables <- c("walking",  
                    "driving",
                    "transit", 
                    "workplaces", 
                    "parks",         
                    "retail_and_recreation", 
                    "transit_stations", 
                    "residential", 
                    "grocery_and_pharmacy")


item_labels_b <- c("Walking",  
                 "Driving",
                 "Public transport", 
                 "Workplaces", 
                 "Parks",         
                 "Retail & recreation", 
                 "Transit stations", 
                 "Residential", 
                 "Grocery & pharmacy")

item_labels  <- c("Walking[A]",  
                   "Driving[A]",
                   "Public transport[A]", 
                   "Workplaces[G]", 
                   "Parks[G]",         
                   "Retail & recreation[G]", 
                   "Transit stations[G]", 
                   "Residential[G]", 
                   "Grocery & pharmacy[G]")

item_names <- set_names(item_labels, item_variables)

color_manual_item_b <- c("walking"= "#ff9500",
                       "driving"= "#ff2d55",
                       "transit"= "#af52de",
                       "workplaces"= "#d56e0c",
                       "parks"      = "#188038",
                       "retail_and_recreation"= "#1967d2",
                       "transit_stations"= "#d01884",
                       "residential"= "#8430ce",
                       "grocery_and_pharmacy"= "#129eaf")

color_manual_item <- c( #"walking"= "#ff9500",
                         "driving"= "#129eaf",
                         "transit"= "#af52de",
                         "workplaces"= "#1967d2",
                         "parks"      = "#188038",
                         # "retail_and_recreation"= "#1967d2",
                         "transit_stations"= "#d01884",
                         "residential"= "#d56e0c"
                         # "grocery_and_pharmacy"= "#ff2d55"
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
