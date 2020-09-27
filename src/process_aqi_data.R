#' Model AQI data and make predictions on new data. 
#' 
#' Author:      Guanqun Cao (guanqun.cao@ieee.org) 
#' Last update: Sep 13, 2020
#'

library(dplyr)
library(rgdal)
library(KernSmooth)
library(matrixStats)

DATA_PATH = "/home/gcao/Datasets/air_pollution/"
DATE_LIST <- sapply(seq(as.Date("2016/01/01"), by = "day", length.out = 366), function(a) format(a, "%Y-%m-%d")) # One week in Sept 2016


assign_conc <- function(coords_df, grid_data){
    conc = c()
    curr_date = 0 
    for (row in 1:nrow(coords_df)) {
        print(row)
        if (curr_date != coords_df[row,]$Date.Local){
            grid_data_date <- grid_data[which(grid_data$Date == coords_df[row,]$Date.Local), ]
            curr_date <- coords_df[row,]$Date.Local
        }
        col <- abs(grid_data_date$Lon - coords_df[row,]$Longitude) + abs(grid_data_date$Lat - coords_df[row,]$Latitude)
        conc <- c(conc, grid_data_date[which(col ==min(col)), ]$Conc)
    }
    return(conc)
}

gen_trn_data <- function(){
    coords_df <- read.csv(paste(DATA_PATH, 'daily_88101_2016.csv', sep=''))
    coords_df <- na.omit(coords_df, cols=c("Latitude", "Longitude"))
    coords_df <- coords_df[which(coords_df$State.Name=='California'),]
    coords_df <- coords_df %>% select(Latitude, Longitude, AQI, Date.Local, Local.Site.Name, Address, State.Name, County.Name, City.Name)
    #write.csv(cal_df_subset, paste(DATA_PATH, 'daily_88101_2016_cal.csv', sep=''), row.names=FALSE)
    coords_df <- coords_df[order(coords_df$Date.Local), ]

    grid_data <- read.csv(paste(DATA_PATH, 'ds.input.cmaq.pm25.2016_cal.csv', sep=''))
    Conc <- assign_conc(coords_df, grid_data)
    coords_df <- cbind(coords_df, Conc)
    write.csv(coords_df, paste(DATA_PATH, 'daily_88101_2016_cal_conc.csv', sep=''), row.names=FALSE)
}

preprocess_trn_data <- function(){
    trn_data <- read.csv(paste(DATA_PATH, 'daily_88101_2016_cal_conc.csv', sep=''))
    trn_data <- trn_data[trn_data$Date.Local %in% DATE_LIST, ]
    trn_data$Zone <- determine_zone(trn_data$Longitude)

    # Check zone 10
    longlat <- SpatialPoints(subset(trn_data, Zone==10, select=c(Longitude, Latitude)))
    proj4string(longlat) <- CRS("+proj=longlat +datum=WGS84")
    xy_10 <- data.frame(spTransform(longlat, CRS("+proj=utm +zone=10 ellps=WGS84")))
    colnames(xy_10) <- c('x.coord', 'y.coord')
    
    # Check zone 11
    longlat <- SpatialPoints(subset(trn_data, Zone==11, select=c(Longitude, Latitude)))
    proj4string(longlat) <- CRS("+proj=longlat +datum=WGS84")
    xy_11 <- data.frame(spTransform(longlat, CRS("+proj=utm +zone=11 ellps=WGS84")))
    colnames(xy_11) <- c('x.coord', 'y.coord')

    trn_data <- rbind(cbind(trn_data[which(trn_data$Zone==10), ], xy_10), cbind(trn_data[which(trn_data$Zone==11), ], xy_11))
    trn_data$Sqrt_Conc <- sqrt(trn_data$Conc)
    trn_data$Sqrt_AQI <- sqrt(trn_data$AQI)
    write.csv(trn_data, paste(DATA_PATH, 'trn.csv', sep=''), row.names=FALSE)
}

preprocess_test_data <- function(){
    test_data <- read.csv(paste(DATA_PATH, 'city_full.csv', sep=''))
    test_data$Zone <- determine_zone(test_data$Longitude)

    # Check zone 10
    longlat <- SpatialPoints(subset(test_data, Zone==10, select=c(Longitude, Latitude)))
    proj4string(longlat) <- CRS("+proj=longlat +datum=WGS84")
    xy_10 <- data.frame(spTransform(longlat, CRS("+proj=utm +zone=10 ellps=WGS84")))
    colnames(xy_10) <- c('x.coord', 'y.coord')
    
    # Check zone 11
    longlat <- SpatialPoints(subset(test_data, Zone==11, select=c(Longitude, Latitude)))
    proj4string(longlat) <- CRS("+proj=longlat +datum=WGS84")
    xy_11 <- data.frame(spTransform(longlat, CRS("+proj=utm +zone=11 ellps=WGS84")))
    colnames(xy_11) <- c('x.coord', 'y.coord')

    test_data <- rbind(cbind(test_data[which(test_data$Zone==10), ], xy_10), cbind(test_data[which(test_data$Zone==11), ], xy_11))
    test_data$Sqrt_Conc <- sqrt(test_data$Conc)
    write.csv(test_data, paste(DATA_PATH, 'test.csv', sep=''), row.names=FALSE)
}

gen_test_data <- function(){
    coords_df <- repeat_data()

    grid_data <- read.csv(paste(DATA_PATH, 'ds.input.cmaq.pm25.2016_cal.csv', sep=''))
    Conc <- assign_conc(coords_df, grid_data)
    coords_df <- cbind(coords_df, Conc)
    coords_df <- subset(coords_df, select=-c(Name))
    write.csv(coords_df, paste(DATA_PATH, 'city_full.csv', sep=''), row.names=FALSE)
}

plot_test_data <- function(){
    city_data <- read.csv(paste(DATA_PATH, 'city.csv', sep=''))
    coordinates(city_data) <- c("Longitude", "Latitude")
    writeOGR(city_data, "city_data.geojson", layer="cal", driver="GeoJSON")
}

determine_zone <- function(long){
    return((floor((long + 180)/6) %% 60) + 1)
}

repeat_data <- function(){
    coords_df <- read.csv(paste(DATA_PATH, 'city_geocode.csv', sep=''))
    coords_df$Date.Local <- DATE_LIST[1]
    tmp_coords_df <- data.frame(coords_df)
    for (i in 2:length(DATE_LIST)){
        tmp_coords_df$Date.Local <- DATE_LIST[i]
        coords_df <- rbind(coords_df, tmp_coords_df)
    }
    #write.csv(coords_df, paste(DATA_PATH, 'city_date.csv', sep=''), row.names=FALSE)
    return(coords_df)
}

extract_ca_aqi_data <- function(){
    data_2016 = read.csv(paste(DATA_PATH, 'daily_88101_2016.csv', sep=''))
    colnames(data_2016)
    # Select the data in California
    data_2016 <- na.omit(data_2016, cols=c("Latitude", "Longitude"))
    cal_df = data_2016[which(data_2016$State.Name=='California'),]
    cal_df_subset <- cal_df %>% select(Latitude, Longitude, AQI, Date.Local, Local.Site.Name, Address, State.Name, County.Name, City.Name)
    write.csv(cal_df_subset, paste(DATA_PATH, 'daily_88101_2016_cal.csv', sep=''), row.names=FALSE)
}

plot_locations_to_geojson <- function(input_file, output_file){
    cal_df <- read.csv(paste(DATA_PATH, input_file, sep=''))
    coordinates(cal_df) <- c("Longitude", "Latitude")
    print(class(cal_df)) # [1] "SpatialPointsDataFrame"
    writeOGR(cal_df, paste0(DATA_PATH, output_file), layer="cal", driver="GeoJSON")
}

attach_pred_aqi <- function(){
    cal_aqi_pred = read.csv(paste(DATA_PATH, 'test.csv', sep=''))
    load(paste0(DATA_PATH, 'cal_aqi_pred.RData'))
    pred_aqi = c()
    for (i in 1:length(DATE_LIST) ){
        pred_aqi = c(pred_aqi, rowMedians(pred_all[9*i]$p.y.predictive.samples^2))
    }
    cal_aqi_pred$Pred_AQI <- round(pred_aqi)
    cal_aqi_pred$Pred_AQI[cal_aqi_pred$Pred_AQI > 300] <- 300 
    write.csv(cal_aqi_pred, paste(DATA_PATH, 'cal_aqi_pred.csv', sep=''), row.names=FALSE)
}

#gen_trn_data()
#gen_test_data()
#preprocess_test_data()
#preprocess_trn_data()
#attach_pred_aqi()
plot_locations_to_geojson('ds.input.cmaq.pm25.2016_cal.csv', 'cmaq2016.geojson')
#plot_locations_to_geojson('city_geocode.csv', 'interests.geojson')
#plot_locations_to_geojson('daily_88101_2016_cal.csv', 'monitor_loc.geojson')
