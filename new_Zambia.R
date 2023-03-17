
library(knitr)
library(readr)
library(sf)
library(readr)
library(ggthemes)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Bernardi_Marta/Documents/Geo_data")

Zambia_data <- read_csv("645.csv.gz")


names(Zambia_data)[names(Zambia_data) == "645"] <- "MCC"
names(Zambia_data)[names(Zambia_data) == "GSM"] <- "Radio_type"
names(Zambia_data)[names(Zambia_data) == "3"] <- "MNC"
names(Zambia_data)[names(Zambia_data) == "3282"] <- "CID"
names(Zambia_data)[names(Zambia_data) == "130"] <- "Location_Area_Code"
names(Zambia_data)[names(Zambia_data) == "28.070755004883"] <- "lat"
names(Zambia_data)[names(Zambia_data) == "-14.902267456055"] <- "lon"
names(Zambia_data)[names(Zambia_data) == "1000"] <- "Range"
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
st_write(mobile_access_sf, "thcell.shp")
mobile_access_shp <- st_read("thcell.shp")


#Open district geographies and assign crs
gred_shp <- st_read("gred.shp")
st_crs(gred_shp) <- 4326


#Try to plot data before merging to see how they look like, there should be a series of data points for each column but with no spatial distribution because I still need to asign it

plot(mobile_access_shp)

#plot the shape of Zambia that I would like to put together 

plot(gred_shp)

#Keep only useful columns in mobile phone data 

mobile_access_selected <- mobile_access_shp |>
  select(c("Rad_typ", "MNC", "CID", "Lct_A_C", "Range", "Avrg_sg","Created", "geometry") )

#Plot to see how the variable of interest look like when all the years are taken into consideration together 

plot(mobile_access_selected$CID)

ggplot() + 
  geom_sf(data = mobile_access_selected, aes(fill = CID)) +
  scale_fill_viridis_c() +
  theme_dark()


#Slice year by year to decrease computational request and assign treatment year by year, to slice use the time the data were "Created"
#the timestamp is a Unix timestamp representing the date and time as the number of seconds that have elapsed since January 1, 1970 at 00:00:00 UTC

# Convert the "Created" column to date format
Zambia_data$Created <- as.POSIXct(Zambia_data$Created, origin="1970-01-01", tz="GMT")

#Create a list of years 
year_list <- split(Zambia_data, format(Zambia_data$Created, "%Y"))

# Save each year's data to a separate file
for (year in names(year_list)) {
  filename <- paste0("zambia_mobile_", year, ".csv")
  write.csv(year_list[[year]], file = filename, row.names = FALSE)
}

#I'm interested in elections happened in 2011 and 2016 so I start by using data on this two years looking at how many CID was in each region in these two years 

#Open the 2011 dataset and merge it with the shape of the electoral districts in Zambia from gred_shp

mobile_2011 <- read_csv("zambia_mobile_2011.csv")  #there are only 2 observations 

#Try with other years 

mobile_2016 <- read_csv("zambia_mobile_2016.csv") #over 5000 observations so I'm studying 2016 election using treatment presence and intensity 

#### opening mobile 2016 as a sf object and assigning coordinate reference system 

mobile_2016_complete <- mobile_2016[complete.cases(mobile_2016[c("lat", "lon")]),]
mobile_2016_sf <- st_as_sf(mobile_2016_complete, coords = c("lat", "lon"))
st_crs(mobile_2016_sf) <- 4326
st_write(mobile_2016_sf, "2016.shp")
mobile_2016_shp <- st_read("2016.shp")


# check the CRS of your spatial objects
st_crs(gred_shp)
st_crs(mobile_2016_shp)

# transform the mobile_2016_shp to match the CRS of gred_shp
mobile_2016_transformed <- st_transform(mobile_2016_shp, st_crs(gred_shp))

### Join the shape of the electoral districts with the cells in 2016 

joined_sf <- st_join(gred_shp, mobile_2016_transformed, join = st_intersects)


### write joined data 2016 into a shape file and then use the normal plot to see how the different varibles look like in the space
st_write(joined_sf, "joined_2016.shp")

plot(joined_sf)


## Assign treatment in the first way  using the presence/intensity  of mobile towers
###To do this create a new column with number of CID by electoral district 

joined_sf <- joined_sf |>
  group_by(cst_n) |>
  mutate(treat1 = n_distinct(CID))

###Plot treatment 1 having NA white 

ggplot() + 
  geom_sf(data = joined_sf, aes(fill = treat1)) +
  scale_fill_viridis_c(na.value = "white", limits = c(0, 200), guide = guide_colorsteps(ncol = 50)) +
  theme_dark() +
  labs(fill = "Number of CIDs")


### Load data on elections outcome from the website and select Zambia data in 2016 election round

load("~/Geo_data/elections.RData")

Zambia_election <- clea_lc_20220908 |>
  filter(ctr_n == "Zambia")

Zambia_election_2016 <- Zambia_election |>
  filter(yr == 2016)

### Select only useful columns 

election_2016 <- Zambia_election_2016 |>
  select(c("cst_n", "pty_n", "pty", "pev1", "vv1", "to1", "cvs1", "pv1", "pvs1", "vot1", "ivv1"))

### Write the name of the districts in aproper way before joining 
election_2016$cst_n <- str_to_title(election_2016$cst_n)

### Perform a left join between a spatial object and a normal database
internet_election <- left_join( joined_sf, election_2016, by = "cst_n")

### Look how potential outcome variables are distributed in space 

##Voters Turnout 

ggplot() + 
  geom_sf(data = internet_election, aes(fill = to1)) +
  scale_fill_viridis_c(na.value = "white", limits = c(0, 1), guide = guide_colorsteps(ncol = 50)) +
  theme_dark() +
  labs(fill = "Voters Turnout")

## Plot correlation between presence of mobile cells and voters turnout 

## first try in a  map 

ggplot(internet_election) +
  geom_sf(aes(fill = treat1, color = to1), alpha = 0.5, size = 0.2) +
  scale_fill_viridis_c(name = "Number of Mobile cells ") +
  scale_color_gradient(name = "Voters turnout") +
  labs(title = "Correlation between Presence of Mobile cells and Voters Turnout") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        axis.title = element_text(size = 12),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

## not readable so go with normal scatterplot fitting a non linear trend 

ggplot(internet_election, aes(x = treat1, y = to1)) +
  geom_point(size = 2, alpha = 0.8, color = "#FF5733") +
  geom_smooth(method = "loess", se = FALSE, color = "#0066CC", size = 1) +
  labs(title = "Correlation between Presence of Mobile cells and Voters Turnout",
       x = "Number of Mobile cells ", y = "Voter's turnout") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = "none")

## not very well fitted by eyeballing 


# Assign treatment in the second way using the range of the mobile towers 





# Plot all Zambia treated and non-treated in the two versions 








# Plot two districts that are near to each others one is treated and one is not 





#Perform basic statistical inference 

















