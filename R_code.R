library(stringr)
library(viridis)
library(knitr)
library(readr)
library(sf)
library(readr)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(kableExtra)


setwd("C:/Users/Bernardi_Marta/Downloads/R_Project")

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
st_write(mobile_access_sf, "cell.shp", delete_dsn = TRUE)
mobile_access_shp <- st_read("cell.shp")


#Open district geographies and assign crs
gred_shp <- st_read("gred.shp")
st_crs(gred_shp) <- 4326


#Try to plot data before merging to see how they look like, there should be a series of data points for each column but with no spatial distribution because I still need to asign it
plot(mobile_access)
#png("mobile_access_new.png")

# Plot the shape of Zambia that I would like to put together 

plot(gred_shp)
#png("gred_shp.png")

head(gred_shp)

#Keep only useful columns in mobile phone data 

mobile_access_selected <- mobile_access_shp |>
  select(c("Rad_typ", "MNC", "CID", "Lct_A_C", "Range", "Avrg_sg","Created", "geometry") )

#Plot to see how the variable of interest look like when all the years are taken into consideration together 

ggplot() + 
  geom_sf(data = mobile_access_selected, aes(fill = CID)) +
  scale_fill_viridis_c() +
  theme_dark()
ggsave("presence_cells.png")

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
kable(head(mobile_2011))
#Try with other years 

mobile_2016 <- read_csv("zambia_mobile_2016.csv") #over 5000 observations so I'm studying 2016 election using treatment presence and intensity 
kable(head(mobile_2016))
kable(mobile_2016, format = "png")

#### opening mobile 2016 as a sf object and assigning coordinate reference system 

mobile_2016_complete <- mobile_2016[complete.cases(mobile_2016[c("lat", "lon")]),]
mobile_2016_sf <- st_as_sf(mobile_2016_complete, coords = c("lat", "lon"))
st_crs(mobile_2016_sf) <- 4326
st_write(mobile_2016_sf, "2016.shp", delete_dsn = TRUE)
mobile_2016_shp <- st_read("2016.shp")


# check the CRS of your spatial objects
st_crs(gred_shp)
st_crs(mobile_2016_shp)

# transform the mobile_2016_shp to match the CRS of gred_shp
mobile_2016_transformed <- st_transform(mobile_2016_shp, st_crs(gred_shp))

### Join the shape of the electoral districts with the cells in 2016 

joined_sf <- st_join(gred_shp, mobile_2016_transformed, join = st_intersects)
joined_sf <- na.omit(joined_sf)

### write joined data 2016 into a shape file and then use the normal plot to see how the different varibles look like in the space
st_write(joined_sf, "yjoined_2016.shp", delete_dsn = TRUE)

plot(joined_sf, max.plot = 17)
#png("thejoin.png")


kable(head(joined_sf))

### Rename variables in th merged dataset ad keep only usefull ones

names(joined_sf)[names(joined_sf) == "cst_n"] <- "elec_district_name"
names(joined_sf)[names(joined_sf) == "cst"] <- "elec_district_code"
names(joined_sf)[names(joined_sf) == "Created"] <- "date"

mobile_district <- joined_sf |> 
  select(c('elec_district_name', 'geometry', 'elec_district_code', 'Range', 'CID', 'Rad_typ', 'date'))


## Assign treatment in the first way  using the presence/intensity  of mobile towers
###To do this create a new column with number of CID by electoral district 

mobile_district <- mobile_district |>
  group_by(elec_district_name) |>
  mutate(treat1 = n_distinct(CID))


###Plot treatment 1 having NA white 

ggplot() + 
  geom_sf(data = mobile_district, aes(fill = treat1)) +
  scale_fill_viridis_c(na.value = "white", limits = c(0, 200), guide = guide_colorsteps(ncol = 50)) +
  theme_dark() +
  labs(fill = "Number of CIDs")

ggsave("/images/treat1.png")

### Load data on elections outcome from the website and select Zambia data in 2016 election round


# This commented out part is how i created Zambia_2016 from the huge raw data from the website too big to stay in github
#load("clea_lc_20220908.RData") 
#Zambia_election <- clea_lc_20220908 |>
#filter(ctr_n == "Zambia")

#Zambia_election_2016 <- Zambia_election |>
#filter(yr == 2016)
#write.csv(Zambia_election_2016, "Zambia_election_2016.csv", row.names = FALSE)

### Select only useful columns and rename them 


Zambia_election_2016 <- read.csv("Zambia_election_2016.csv")

election_2016 <- Zambia_election_2016 |>
  select(c("cst_n", "pty_n", "pty", "pev1", "vv1", "to1", "cvs1", "pv1", "pvs1", "vot1", "ivv1"))

names(election_2016)[names(election_2016) == "cst_n"] <- "elec_district_name"
names(election_2016)[names(election_2016) == "to1"] <- "voter_turn"
names(election_2016)[names(election_2016) == "pty_n"] <- "party_name"
names(election_2016)[names(election_2016) == "pty"] <- "party_code"
names(election_2016)[names(election_2016) == "pev1"] <- "n_eligible_voters"
names(election_2016)[names(election_2016) == "pv1"] <- "n_votes"
names(election_2016)[names(election_2016) == "pvs1"] <- "party_vpte_share"
names(election_2016)[names(election_2016) == "vv1"] <- "n_valid_votes"
names(election_2016)[names(election_2016) == "ivv1"] <- "n_invalid_votes"
names(election_2016)[names(election_2016) == "vot1"] <- "vote_cast"
kable(head(election_2016))

### Write the name of the districts in a proper way before joining 

election_2016$elec_district_name <- str_to_title(election_2016$elec_district_name)

### Perform a left join between a spatial object and a normal database
internet_election <- left_join( mobile_district, election_2016, by = "elec_district_name")

### Look how potential outcome variables are distributed in space 

##Voters Turnout 

ggplot() + 
  geom_sf(data = internet_election, aes(fill = voter_turn)) +
  scale_fill_viridis_c(na.value = "white", limits = c(0, 1), guide = guide_colorsteps(ncol = 50)) +
  theme_dark() +
  labs(fill = "Voters Turnout")
ggsave("/images/outcome1.png")



## Distribution of vote shares by parties to see main parties

vote_share <- election_2016 |>
  group_by(party_name) |>
  summarize(total_votes = sum(n_votes, na.rm = TRUE))


party_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#E6007E", "#980000", "#000080", "#E9A3C9", "#A9D18E", "#FFC8A3")


ggplot(vote_share, aes(x = party_name, y = total_votes, fill = party_colors)) + 
  geom_bar(stat = "identity") +
  labs(title = " Number of Votes by Party", x = "Party", y = "Number of votes") +
  scale_fill_identity(guide = "legend", labels = vote_share$party_name) +
  theme_dark() +
  theme(axis.text.x = element_blank())
ggsave("/images/votesharebar.png")



## Look at geographical distribution of the two main parties

##  Vote share by district for National Restoration Party
votes_by_district <- internet_election |>
  filter(party_name == "National Restoration Party")|>
  group_by(elec_district_name) %>% 
  summarise(total_votes = sum(n_votes))


ggplot(votes_by_district, aes(fill = total_votes, color = total_votes)) +
  geom_sf(alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_color_gradient(low = "white", high = "blue") +
  labs(title = "National Restoration Party - Votes distribution") +
  theme_dark() +
  guides(fill = guide_colorbar(title = "Number of Votes"), color = FALSE) +
  coord_sf(crs = st_crs(votes_by_district), lims_method = "geometry_bbox")

ggsave("/images/nrp.png")

## Vote share by district for Multi-Party Democracy
add_district <- internet_election |>
  filter(party_name == "Movement for Multi-Party Democracy")|>
  group_by(elec_district_name)|> 
  summarise(total_votes = sum(n_votes))


ggplot(add_district, aes(fill = total_votes, color = total_votes)) +
  geom_sf(alpha = 0.5) +
  scale_fill_gradient(low = "white", high = "red") +
  scale_color_gradient(low = "white", high = "red") +
  labs(title = "Movement for Multi-Party Democracy - Votes distribution") +
  theme_dark() +
  guides(fill = guide_colorbar(title = "Number of Votes"), color = FALSE) +
  coord_sf(crs = st_crs(add_district), lims_method = "geometry_bbox")

ggsave("/images/add.png")


## Correlation CID and voters turnout 

internet_election_clean <- na.omit(internet_election)

ggplot(internet_election_clean, aes(x = treat1, y = voter_turn)) +
  geom_point() +
  stat_smooth(method = "lm", level = 0.90) +
  labs(x = "Number of internet cells", y = "Voter Turnout")

ggsave("/images/lm.png")

## Correlation CID and valid votes 

ggplot(internet_election_clean, aes(x = treat1, y = n_valid_votes)) +
  geom_point(color = "blue") +
  stat_smooth(method = "lm", level = 0.90, color = "red", se = 0.90) +
  labs(x = "Number of internet cells", y = "Number of Valid votes") +
  theme_classic()
ggsave("/images/comp.png")


## Assign treatment by presence instead of intensity 


internet_election <- internet_election |>
  mutate(treat2 = ifelse(treat1 >= 1, 1, 0))

ggplot() + 
  geom_sf(data = internet_election, aes(fill = factor(treat2))) +
  scale_fill_manual(values = c("0" = "white", "1" = "purple"), 
                    na.value = "white") +
  guides(fill = guide_legend(title = "Presence of mobile towers")) +
  theme_dark()

ggsave('/images/treat2.png')




##Try with a threshold 
#Use 10 as first try 

internet_election <- internet_election |>
  mutate(treat2_1 = ifelse(treat1 >= 10, 1, 0))
ggplot() + 
  geom_sf(data = internet_election, aes(fill = factor(treat2_1))) +
  scale_fill_manual(values = c("0" = "white", "1" = "darkorange"), 
                    na.value = "white") +
  guides(fill = guide_legend(title = "Presence of at least 10 mobile towers")) +
  theme_dark()

ggsave('/images/treat2_1.png')

#Use a higher one: 25 

internet_election <- internet_election |>
  mutate(treat2_2 = ifelse(treat1 >= 25, 1, 0))


ggplot() + 
  geom_sf(data = internet_election, aes(fill = factor(treat2_2))) +
  scale_fill_manual(values = c("0" = "white", "1" = "darkgreen"), 
                    na.value = "white") +
  guides(fill = guide_legend(title = "Presence of at least 25 mobile towers")) +
  theme_dark()

ggsave('/images/treat2_2.png')


#Go up to 50 (I expect this to color populated districts ) 

internet_election <- internet_election |>
  mutate(treat2_3 = ifelse(treat1 >= 50, 1, 0))

ggplot() + 
  geom_sf(data = internet_election, aes(fill = factor(treat2_3))) +
  scale_fill_manual(values = c("0" = "white", "1" = "lightblue"), 
                    na.value = "white") +
  guides(fill = guide_legend(title = "Presence of at least  50 mobile towers")) +
  theme_dark()

ggsave('/images/treat2_3.png')












