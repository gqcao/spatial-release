# load packages
library(httr)
library(rjson)
library(RCurl)
library(dplyr)

# Example address: 
# http://localhost:7070/reverse?format=geojson&lat=34.582769&lon=-117.409214&zoom=17&addressdetails=1&extratags=1&namedetails=1

rev_geocoding <- function(coords_df, query_format='geojson', server='local'){
    if (server == 'local'){
        base_addr <- "localhost:7070"
    } else {
        base_addr <- "https://nominatim.openstreetmap.org"
    }
    geocode <- data.frame(osm_id=integer(), osm_type=character(), property_category=character(), addresstype=character(), city=character())
    for (row in 1:nrow(coords_df)){
        print(paste0('Row number is ', row))
        url = paste(base_addr, '/reverse?format=', query_format, '&lat=', coords_df[row,]$Latitude, '&lon=', coords_df[row,]$Longitude, '&zoom=17&addressdetails=1&extratags=1&namedetails=1', sep='')
        if (server == 'local'){
            json_df <- rjson::fromJSON(getURL(url))
        } else {
            json_df <- rjson::fromJSON(file=url)
        }
	road <- json_df$features[[1]]$properties$address$road
        if (is.null(road)) road <- NA

	city <- json_df$features[[1]]$properties$address$city
        if (is.null(city)){
            city <- json_df$features[[1]]$properties$address$town
            if (is.null(city)){
                city <- json_df$features[[1]]$properties$address$village
                if (is.null(city)){
                    city <- coords_df[row,]$Name
                }
            }
        }

	county <- json_df$features[[1]]$properties$address$county
        if (is.null(county)) county <- NA

        curr_row <- data.frame(road, city, county)
        geocode <- rbind(geocode, curr_row)
    }
    #print(geocode)
    geocode <- cbind(coords_df, geocode)
    write.csv(geocode, 'city_geocode.csv', row.names=FALSE)
}

DATA_PATH = '/home/gcao/Datasets/air_pollution/'
coords_df = read.csv(paste(DATA_PATH, 'city.csv', sep=''))
coords_df <- coords_df[!duplicated(coords_df[,2:3]),]
rev_geocoding(coords_df, 'geojson', 'local')
