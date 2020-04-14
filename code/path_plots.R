###########################################################################################-
###########################################################################################-
##
## path plots ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(tidyverse)
library(lubridate)
library(glue)
library(hms)
library(here)
library(DBI)
library(RSQLite)
library(dbplyr)
library(viridis)
library(fs)
library(ggspatial)
library(ggdark)
library(tictoc)
library(sf)
library(extrafont)

loadfonts("win")

#-----------------------------------------------------------------------------------------#
# Map features
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# LGA bounding box
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

lga_bbox <- 
	tibble(
		longitude = c(-74.232575, -73.516318),
		latitude = c(40.503766, 41.046881)
	)

lga_center <- 
	c(
		longitude = -73.874861, 
		latitude = 40.780347
	)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Expanded bbox
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

lga_bbox_W <- destPointRhumb(p = lga_bbox, b = 270, d = 16000)[1,1]
lga_bbox_E <- destPointRhumb(p = lga_bbox, b = 90, d = 16000)[2,1]

lga_bbox_S <- destPointRhumb(p = lga_bbox, b = 180, d = 16000)[1,2]
lga_bbox_N <- destPointRhumb(p = lga_bbox, b = 0, d = 16000)[2,2]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# CRS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

my_crs <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Map layers
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Downloaded from: https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_500k.zip

tri_state_counties <- 
	shapefile("data/gis/cb_2018_us_county_500k/cb_2018_us_county_500k.shp") %>%
	
	# Pulling out CT, NJ, & NY counties by FIPS code
	
	subset(STATEFP %in% c("09", "34", "36")) %>% 
	st_as_sfc() %>% 
	st_transform(crs = my_crs) %>% 
	df_spatial()


# Downloaded from: https://geodata.lib.berkeley.edu/download/file/stanford-xv279yj9196-shapefile.zip

tri_state_coastline <- 
	shapefile("data/gis/us_coastline_1m/xv279yj9196.shp") %>%
	st_as_sfc() %>% 
	st_transform(crs = my_crs) %>% 
	df_spatial() %>% 
	filter(
		x %>% between(lga_bbox$longitude[1], lga_bbox$longitude[2]),
		y %>% between(lga_bbox$latitude[1], lga_bbox$latitude[2])
	) %>% 
	add_count(feature_id) %>% 
	filter(n > 200) # to get rid of ugly duplicated islands


# Downloaded from: https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=Original

nyc_boro_bounds <- 
	shapefile("data/gis/2020_03_13_nybb_20a/nybb_20a/nybb.shp") %>% 
	st_as_sfc() %>% 
	st_transform(crs = my_crs) %>% 
	df_spatial()


# Downloaded from: https://geodata.library.columbia.edu/download/file/sde-columbia-ntad_2013_runway-shapefile.zip

lga_runways <- 
	shapefile("data/gis/sde-columbia-ntad_2013_runway-shapefile/columbia_ntad_2013_runway.shp") %>%
	
	# Pulling out LGA by IATA code
	
	subset(locid == "LGA") %>% 
	st_as_sfc() %>% 
	st_transform(crs = my_crs) %>% 
	df_spatial()


# Downloaded from: https://geodata.library.columbia.edu/download/file/sde-columbia-ntad_2013_runway-shapefile.zip

central_park <- 
	shapefile("data/gis/Open Space (Parks)/geo_export_ac63f253-9910-4819-8d6c-344836349c03.shp") %>%
	
	# Pulling out LGA by IATA code
	
	subset(park_name == "Central Park") %>% 
	st_as_sfc() %>% 
	st_transform(crs = my_crs) %>% 
	df_spatial()


#=========================================================================================#
# Loading and cleaning data ----
#=========================================================================================#

# Downloaded in "LGA flight history.R"

#-----------------------------------------------------------------------------------------#
# Pulling data
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Connecting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

lga_tracks_db <- dbConnect(SQLite(), "data/lga_tracks_db.sqlite3")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Pulling
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

dates_to_plot <- 
	
	c(
		seq(as_date("2020-03-16"), as_date("2020-03-20"), "day"),
	  	seq(as_date("2020-04-06"), as_date("2020-04-10"), "day")
	) %>% 
	as.numeric()


arrivals_tracks <- 
	lga_tracks_db %>% 
	tbl("arrivals_tracks") %>% 
	filter(flight_date %in% dates_to_plot) %>% 
	collect() %>% 
	
	mutate(
		time = as_datetime(time, tz = "US/Eastern"),
		flight_date = as_date(flight_date),
		week = week(time)
	) %>% 
	drop_na(time, longitude, latitude, unique_flight) %>%
	distinct(time, unique_flight, .keep_all = TRUE) %>% 
	
	# Expanding the bbox, so that the tracks come from off screen
	
	filter(
		longitude %>% between(lga_bbox_W, lga_bbox_E),
		latitude  %>% between(lga_bbox_S, lga_bbox_N)
	) %>% 
	
	mutate()



# dbDisconnect(lga_tracks_db)

n_flights <- n_distinct(arrivals_tracks$unique_flight)

unique_flight <- unique(arrivals_tracks$unique_flight)

set.seed(1)

flight_path_colors <- sample(viridis_pal()(256), length(unique_flight), replace = TRUE)

flight_colors <- tibble(unique_flight, flight_path_colors)

arrivals_tracks <- 
	left_join(
		arrivals_tracks, 
		flight_colors, 
		by = "unique_flight"
	)

flights_count <- 
	arrivals_tracks %>% 
	count(flight_date, unique_flight) %>% 
	count(flight_date) %>% 
	mutate(flights = str_c(n, c(" flights", rep("", nrow(.) - 1))))

#=========================================================================================#
# Plotting ----
#=========================================================================================#

bottom_right_longitude <- max(lga_bbox$longitude) - ((max(lga_bbox$longitude) - min(lga_bbox$longitude)) * 0.15)
bottom_right_latitude <- max(lga_bbox$latitude) - ((max(lga_bbox$latitude) - min(lga_bbox$latitude)) * 0.90)

path_plot <- 
	ggplot() +
	geom_polygon(
		aes(x = x, y = y, group = piece_id),
		data = tri_state_counties,
		color = "black",
		fill = "black",
		size = .5
	) +
	geom_path(
		aes(x = x, y = y, group = piece_id),
		data = tri_state_coastline,
		color = "gray15",
		size = .5
	) +
	geom_polygon(
		aes(x = x, y = y, group = piece_id),
		data = nyc_boro_bounds,
		color = "black",
		fill = "gray15",
		size = .125
	) +
	geom_path(
		aes(x = x, y = y, group = piece_id),
		data = lga_runways,
		color = "white",
		size = .5
	) +
	geom_polygon(
		aes(x = x, y = y, group = piece_id),
		data = central_park,
		color = "gray9",
		fill = "gray9",
		size = .25
	) +
	geom_path(
		aes(x = longitude, y = latitude, group = unique_flight, color = flight_path_colors),
		data = arrivals_tracks,
		# color = "gray50",
		size = .25,
		alpha = .75
	) +
	scale_color_identity() +
	geom_label(
		data = flights_count,
		aes(x = bottom_right_longitude, y = bottom_right_latitude, label = flights),
		color = "black",
		fill = "white", 
		hjust = c(0.7, rep(0.5, nrow(flights_count) - 1)),
		vjust = "middle",
		label.r = unit(0.01, "lines"),
		label.size = 1,
		size = 3,
		family = "DejaVu Sans Mono - Bront"
	) +
	# annotate(geom = "label", x = 2, y = 1, label = flights_count$n, color = "black", fill = "white") +
	dark_theme_bw() +
	coord_quickmap(
		xlim = lga_bbox$longitude,
		ylim = lga_bbox$latitude,
		expand = FALSE
	) +
	# facet_grid(rows = vars(week), cols = vars(wday))
	facet_wrap(vars(flight_date), ncol = 5, labeller = label_bquote(cols = .(format(flight_date, format = "%A, %x")))) +
	theme(
		axis.title = element_blank(),
		axis.text = element_blank(), 
		text = element_text(family = "DejaVu Sans Mono - Bront")
	) +
	labs(title = "Arrivals at LaGuardia airport, 3 weeks apart", caption = "Data scraped from FlightAware.com")

ggsave(
	"plots/path_plot.png",
	path_plot,
	width = 10,
	height = 5,
	dpi = 200,
	bg = "black"
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
