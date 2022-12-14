library(sf)
library(sp)
library(ggplot2)
library(rgeos)
library(maptools)
library(rnaturalearth)
library(rnaturalearthdata)

# Overlaying protest coordinates on shapefile of US urban areas so that US Census population data can be used 
# Protest data from ACLED: https://acleddata.com/data-export-tool/
# US Census data from: https://data.census.gov/cedsci/table?tid=ACSDP5Y2020.DP05
# Shapefile from: https://catalog.data.gov/dataset/tiger-line-shapefile-2017-2010-nation-u-s-2010-census-urban-area-national

							
us.map.sp <- readShapeSpatial("./data/tl_2017_us_uac10/tl_2017_us_uac10.shp",proj4string=CRS("+proj=longlat +ellps=WGS84")) # US urban areas shapefile 
# us.map.sp <- st_read("~/data/tl_2017_us_uac10/tl_2017_us_uac10.shp",proj4string=CRS("+proj=longlat +ellps=WGS84")) # US urban areas shapefile 
acled <- read.csv("./data/ACLED_USA_2020_2022_May20_actors.csv")	# protest data	

# Overlaying protests on urban areas
location <- over(SpatialPoints(data.frame(acled$LONGITUDE,acled$LATITUDE),CRS("+proj=longlat +ellps=WGS84")),us.map.sp)
acled.new <- cbind(acled,location)

# CSV containing ACLED protest data with joined with urban area data
write.csv(acled.new,file="./data/acled_locationMatched.csv")

	
# Creating a CSV file containing all urban areas
us.map <- st_read("./data/tl_2017_us_uac10/tl_2017_us_uac10.shp")
us.data <- data.frame(cbind(as.character(us.map$UACE10),as.character(us.map$GEOID10),as.character(us.map$NAME10),as.character(us.map$NAMELSAD10),as.character(us.map$INTPTLAT10),as.character(us.map$INTPTLON10)))
names(us.data) <- c("UACE10","GEOID10","NAME10","NAMELSAD10","INTPTLAT10","INTPTLON10")
write.csv(us.data,file="./data/USDATA.csv")



 
