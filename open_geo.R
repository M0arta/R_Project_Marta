
library(sf)

library(readr)
library(dplyr)
library(ggplot2)

setwd("~/Downloads/Geo_districts")

#Open mobile access and convert to shape file 

mobile_access = read.csv("mobile.csv")
mobile_access_complete <- mobile_access[complete.cases(mobile_access[c("lat", "lon")]),]
mobile_access_sf <- st_as_sf(mobile_access_complete, coords = c("lat", "lon"))
st_crs(mobile_access_sf) <- 4326
st_write(mobile_access_sf, "Mobile.shp")
mobile_access_shp <- st_read("Mobile.shp")


#Open district geographies and assign crs
gred_shp <- st_read("GRED.shp")
st_crs(gred_shp) <- 4326


ggplot() +
  geom_sf(data = mobile_access_shp) 

plot(mobile_access_shp)

Merged_sf <- st_join(gred_shp, mobile_access_shp, join = st_intersects)

st_write(Merged_sf, "MERGED_shp.shp")

plot(Merged_sf)

merged_shp <- st_read("MERGED_shp.shp")


# Estract data from timestamps 




# Assign treatment in the first way 






# Assign treatment in the second way 





# PLot all Zambia treated and non-treated in the two versions 








# Plot two districts that are near to each others one is treated and one is not 









