```{r, results='asis', echo=FALSE}

library(knitr)
library(readr)
library(sf)
library(readr)
library(dplyr)
library(ggplot2)

#setwd("~/Home/Geo_data")

Zambia_data <- read_csv("645.csv.gz")  

names(Zambia_data)[names(Zambia_data) == "645"] <- "MCC"
names(Zambia_data)[names(Zambia_data) == "GSM"] <- "Radio_type"
names(Zambia_data)[names(Zambia_data) == "3"] <- "MNC"
names(Zambia_data)[names(Zambia_data) == "3282"] <- "CID"
names(Zambia_data)[names(Zambia_data) == "130"] <- "Location_Area_Code"
names(Zambia_data)[names(Zambia_data) == "0...6"] <- "lon"
names(Zambia_data)[names(Zambia_data) == "28.070755004883"] <- "lat"
names(Zambia_data)[names(Zambia_data) == "-14.902267456055"] <- "Range"
names(Zambia_data)[names(Zambia_data) == "1000"] <- "Samples"
names(Zambia_data)[names(Zambia_data) == "1...10"] <- "Changeable=1"
names(Zambia_data)[names(Zambia_data) == "1...11"] <- "Changeable=0"
names(Zambia_data)[names(Zambia_data) == "1459743588...12"] <- "Created"
names(Zambia_data)[names(Zambia_data) == "1459743588...13"] <- "Updated"
names(Zambia_data)[names(Zambia_data) == "0...14"] <- "Average_signal"

# write uncompressed data
write_csv(Zambia_data, "Zambia_data.csv")

#Open mobile access and convert to shape file 

mobile_access = read.csv("Zambia_data.csv")
mobile_access_complete <- mobile_access[complete.cases(mobile_access[c("lat", "lon")]),]
mobile_access_sf <- st_as_sf(mobile_access_complete, coords = c("lat", "lon"))
st_crs(mobile_access_sf) <- 4326
st_write(mobile_access_sf, "MobiLE.shp")
mobile_access_shp <- st_read("MobiLE.shp")


#Open district geographies and assign crs
gred_shp <- st_read("gred.shp")
st_crs(gred_shp) <- 4326


#Try to plot data before merging to see how they look like
ggplot() +
  geom_sf(data = mobile_access_shp) 

plot(mobile_access_shp)


#Merge the files 

mobile_access_selected <- mobile_access_shp %>%
  select(c("Rad_typ", "MNC", "CID", "Lct_A_C", "Range", "Avrg_sg","Created", "Samples", "geometry") )


intersect_sf <- st_intersection(gred_shp, mobile_access_selected)

merged_sf <- st_join(gred.shp, mobile_access_selected, join = st_intersects)


# Simplify the geometries
gred_simple <- st_simplify(gred_shp, TOL = 0.01)
mobile_access_simple <- st_simplify(mobile_access_sf, TOL = 0.01)

# Intersect the two data frames
intersect_sf <- st_intersection(gred_simple, mobile_access_simple)

# Perform the spatial join with left = FALSE to only keep matching features
merged_sf <- st_join(intersect_sf, mobile_access_sf, join = st_intersects, left = FALSE)






st_write(Merged_sf, "merged.shp")

plot(Merged_sf)

merged_shp <- st_read("merged.shp")


#Remove columns with empty values 


merge_select_shp <- merged_shp[,-(6:19)] 

View(merge_select_shp)

plot(merge_select_shp)

# Estract data from timestamps 




# Assign treatment in the first way  






# Assign treatment in the second way 





# PLot all Zambia treated and non-treated in the two versions 








# Plot two districts that are near to each others one is treated and one is not 























