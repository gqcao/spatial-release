#' Extract cmaq data in California from the entire US.
#' 
#' Author:      Guanqun Cao (guanqun.cao@ieee.org) 
#' Last update: Sep 12, 2020
#'

library(dplyr)

subset_grid_data <- function(data){
    bbx = c(41.994307, -124.573983, 32.539034, -114.055564) # Lat and Long on the top left and bottom right
    newdata <- subset(data, Lon < bbx[4] & Lon > bbx[2] & Lat < bbx[1] & Lat > bbx[3], select=c(Lon, Lat, Date, Conc))
    return(newdata)
}

# Download the data from https://ofmpub.epa.gov/rsig/rsigserver?data/FAQSD/inputs/ds_input_cmaq_pm25_2016.zip
DATA_PATH = "../data/"
data <- read.csv(paste(DATA_PATH, 'ds.input.cmaq.pm25.2016.csv', sep=''))
newdata <- subset_grid_data(data)
write.csv(newdata, paste(DATA_PATH, 'ds.input.cmaq.pm25.2016_cal.csv', sep=''), row.names=FALSE)
