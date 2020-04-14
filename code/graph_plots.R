###########################################################################################-
###########################################################################################-
##
## graph plots ----
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
# Setting map view parameters
#-----------------------------------------------------------------------------------------#

# Bounding box around LGA (big enough to show aproaches from all directions)

lga_bbox <- 
	tibble(
		longitude = c(-74.232575, -73.516318),
		latitude = c(40.503766, 41.046881)
	)

# Where the runways cross

lga_center <- 
	c(
		longitude = -73.874861, 
		latitude = 40.780347
	)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# CRS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

my_crs <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#-----------------------------------------------------------------------------------------#
# Pulling data
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Connecting to database
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
	
	select(
		time,
		longitude,
		latitude,
		unique_flight,
		month,
		day,
		hour,
		wday
	) %>% 
	
	filter(
		longitude %>% between(!!lga_bbox$longitude[1], !!lga_bbox$longitude[2]),
		latitude  %>% between(!!lga_bbox$latitude[1], !!lga_bbox$latitude[2])
	) %>% 

	collect() %>% 
	
	mutate(time = as_datetime(time, tz = "US/Eastern")) %>% 
	
	# Records with missing values are probably not "real" data points, so dropping them
	
	drop_na(time, longitude, latitude, unique_flight) %>% 
	
	group_by(unique_flight) %>% 
	
	filter(time == max(time))
	
	
	# Restricting path data to coordinates inside the bounding box, to reduce unnecessary processing and 
	# 	memory overhead, and also so that the eventual summary statistics correspond to paths that
	# 	are visible on the map


min_max_seq_df <- 
	tibble(
		time = 
			seq(
				update(min(arrivals_tracks$time), hours = 0, minutes = 0, seconds = 0), 
				update(max(arrivals_tracks$time), hours = 24, minutes = 0, seconds = 0),
				"1 hour"
			)
		
	) %>% 
	
	filter(as_date(time) %in% dates_to_plot)


arrivals_tracks_summarized <- 
	
	arrivals_tracks %>% 
	
	ungroup() %>% 
	
	mutate(floor_time = floor_date(time, unit = "1 hour")) %>% 
	
	distinct(unique_flight, floor_time, .keep_all = TRUE) %>% 
	
	count(floor_time, name = "flights_count") %>% 
	
	left_join(
		min_max_seq_df, 
		., 
		by = c("time" = "floor_time")
	) %>% 
	
	mutate(
		month = month(time),
		day = day(time),
		hour = hour(time),
		wday = wday(time),
		date = as_date(time)
	) %>% 
	
	mutate(flights_count = replace_na(flights_count, 0))


daily_average <- 
	arrivals_tracks_summarized %>% 
	group_by(date) %>% 
	summarise(
		flights_mean = mean(flights_count, na.rm = TRUE),
		flights_count = sum(flights_count, na.rm = TRUE)
	) %>% 
	mutate(flights_count = str_c(flights_count, c(" flights", rep("", nrow(.) - 1))))


#=========================================================================================#
# Graph ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Drawing graphs
#-----------------------------------------------------------------------------------------#

flights_per_hour <- 
	
	arrivals_tracks_summarized %>% 
	
	ggplot() +
	
	# 0-point
	
	geom_hline(yintercept = 0, color = "gray20", linetype = 1, size = 1) +
	
	# Daily average
	
	geom_hline(
		data = daily_average,
		aes(
			yintercept = flights_mean,
			group = 1,
			colour = "Daily average"
		),
		size = 1
	) +
	
	# Count in each minute
	
	geom_path(
		aes(
			x = hour,
			y = flights_count,
			group = 1,
			colour = "Each hour"
		),
		size = 1
	) +
	
	geom_label(
		data = daily_average,
		aes(x = 22, y = 45, label = flights_count),
		color = "black",
		fill = "white", 
		hjust = c(0.8, rep(0.5, nrow(daily_average) - 1)),
		vjust = "middle",
		label.r = unit(0.01, "lines"),
		label.size = 1,
		size = 3,
		family = "DejaVu Sans Mono - Bront"
	) +
	
	scale_colour_manual(
		name = NULL,
		breaks = c("Daily average", "Each hour"),
		values = c("#424186", "#FDE725"),
		guide = guide_legend(direction = "vertical", override.aes = aes(size = 1))
	) +
	
	# Plot display specs
	
	scale_y_continuous(name = "Flights / Hour", breaks = seq(0, 50, 10)) +
	
	scale_x_continuous(name = "Hour", breaks = seq(3, 21, 3)) +
	
	coord_cartesian(
		xlim = c(0, 24), 
		ylim = c(0, 50),
		expand = FALSE
	) +
	dark_theme_gray() +
	
	# Tweaking the plot display
	
	theme(
		legend.justification = c(0, 1),
		legend.position = c(0, 1),
		legend.background = element_rect(fill = NA, color = NA),
		legend.title = element_blank(),
		legend.margin = margin(t = 0, r = 0, b = 0, l = 3),
		legend.key = element_rect(fill = NA),
		text = element_text(size = 12, family = "DejaVu Sans Mono - Bront"),
		legend.text = element_text(size = 9),
		panel.grid.minor = element_blank()
	) +
	
	facet_wrap(vars(date), ncol = 5, labeller = label_bquote(cols = .(format(date, format = "%A, %x")))) +
	
	labs(title = "Arrivals at LaGuardia airport, 3 weeks apart", caption = "Data scraped from FlightAware.com")
	
ggsave(
	"plots/flights_per_hour.png",
	flights_per_hour,
	width = 15,
	height = 5,
	dpi = 200
)
