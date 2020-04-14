###########################################################################################-
###########################################################################################-
##
## Plotting flights ----
##
###########################################################################################-
###########################################################################################-

# TODO: Give the script a flight number (or numbers), and derive everything else (e.g., 
# 	airport codes)

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------------------------------------#

library(rvest)
library(jsonlite)
library(maps)
library(tidyverse)
library(lubridate)
library(glue)
library(fs)
library(janitor)
library(geosphere)
library(hms)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#-----------------------------------------------------------------------------------------#
# Getting airpots ----
#-----------------------------------------------------------------------------------------#

airports <- 
	read_csv(
		"https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", 
		col_names = 
			c(
				"airport_id",
				"name",
				"city",
				"country",
				"iata",
				"icao",
				"latitude",
				"longitude",
				"altitude",
				"tzone",
				"dst",
				"tzone_name",
				"type",
				"source"
			), 
		col_types = cols(.default = col_guess())
	) %>% 
	mutate(iata = na_if(iata, "\\N"))


orig_dest <- 
	airports %>% 
	filter(iata %in% c("HKG", "JFK")) %>% 
	arrange(-longitude)


#-----------------------------------------------------------------------------------------#
# Loading map data
#-----------------------------------------------------------------------------------------#

world_countries <-
	ne_countries(scale = "medium", returnclass = "sf") %>% 
	df_spatial() %>% 
	rename(longitude = x, latitude = y)

world_coastline <-
	ne_coastline(scale = "medium", returnclass = "sf") %>% 
	df_spatial() %>% 
	rename(longitude = x, latitude = y)

world_capitals <- 
	world.cities %>% 
	filter(capital == 1) %>% 
	as_tibble() %>% 
	rename(longitude = long, latitude = lat)


#=========================================================================================#
# Scraping data ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# reading in ----
#-----------------------------------------------------------------------------------------#

# parameterize systematically

base_url <- "https://flightaware.com/live/flight/"

flight_date <- str_remove_all("2020-03-02", "-")

flight_number <- c("CPA846", "CPA845")
flight_time   <- c("1155Z", "0545Z")
flight_orig_dest_icao <- c("VHHH/KJFK", "KJFK/VHHH")
flight_orig_dest_iata <- c("HKG > JFK", "JFK > HKG")


flights <- tibble()

for (i in 1:length(flight_number)) {
	
	flights <- 
		read_html(
			glue(
				base_url, 
				"{flight_number[i]}/history/{flight_date}/{flight_time[i]}/{flight_orig_dest_icao[i]}/tracklog"
			)
		) %>% 
		html_nodes("table.prettyTable") %>% 
		html_table(header = FALSE) %>% 
		flatten_dfc() %>% 
		set_names(nm = c("time", "latitude", "longitude", "course", "kts", "mph", "feet", "rate", "reporting_facility")) %>% 
		slice(-(1:3)) %>% 
		type_convert(col_types = cols(.default = col_guess())) %>% 
		drop_na(reporting_facility, latitude, longitude) %>% 
		mutate(
			latitude = latitude %>% str_sub(start = 1, end = 7) %>% as.numeric(),
			longitude = longitude %>% str_sub(start = 1, end = 7) %>% as.numeric(),
			time = time %>% str_sub(start = 1, end = 12),
			flight_date = as_date(flight_date[i]),
			flight_date_chr = as.character(flight_date),
			start_end = flight_orig_dest_iata[i]
		) %>% 
		drop_na(latitude, longitude) %>% 
		bind_rows(flights, .)
	
}


#-----------------------------------------------------------------------------------------#
# Midpoints(ish) of paths
#-----------------------------------------------------------------------------------------#

flights_mid <- 
	flights %>% 
	group_by(start_end) %>% 
	group_modify( ~ slice(.x, 1:round(nrow(.x)/2))) %>%
	select(start_end, longitude, latitude)


#=========================================================================================#
# Computing lines to plot ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Computing great circle between origin and destination
#-----------------------------------------------------------------------------------------#

great_circle <-
	gcIntermediate(
		p1 = c(head(orig_dest$longitude, 1), head(orig_dest$latitude, 1)),
		p2 = c(tail(orig_dest$longitude, 1), tail(orig_dest$latitude, 1)),
		n = 100
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)

great_circle_mid <- 
	great_circle %>% 
	slice(round(nrow(.)/2)) %>% 
	select(longitude, latitude)


#-----------------------------------------------------------------------------------------#
# Computing great circle for outline of polygon covering visible half of Earth
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Generating points at visible edge of Earth, in the cardinal directions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# 40007863 is polar circumference of the Earth in meters

n_point <- 
	destPoint(
		p = c(great_circle_mid$longitude, great_circle_mid$latitude),
		b = 360,
		d = 40007863/4/1.01
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)

e_point <- 
	destPoint(
		p = c(great_circle_mid$longitude, great_circle_mid$latitude),
		b = 90,
		d = 40007863/4/1.01
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)

s_point <- 
	destPoint(
		p = c(great_circle_mid$longitude, great_circle_mid$latitude),
		b = 180,
		d = 40007863/4/1.01
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)

w_point <- 
	destPoint(
		p = c(great_circle_mid$longitude, great_circle_mid$latitude),
		b = 270,
		d = 40007863/4/1.01
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Computing great circle segments between adjacent cardinal points
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

ne_gc <- 
	gcIntermediate(
		c(n_point$longitude, n_point$latitude), 
		c(e_point$longitude, e_point$latitude), 
		n = 50,
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat) %>% 
	mutate(latitude_n = latitude + 2)

es_gc <- 
	gcIntermediate(
		c(e_point$longitude, e_point$latitude), 
		c(s_point$longitude, s_point$latitude), 
		n = 50,
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat) %>% 
	mutate(latitude_n = latitude + 2)

sw_gc <- 
	gcIntermediate(
		c(s_point$longitude, s_point$latitude), 
		c(w_point$longitude, w_point$latitude), 
		n = 50,
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat) %>% 
	mutate(latitude_n = latitude + 2)

wn_gc <- 
	gcIntermediate(
		c(w_point$longitude, w_point$latitude), 
		c(n_point$longitude, n_point$latitude), 
		n = 50,
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat) %>% 
	mutate(latitude_n = latitude + 2)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Computing great circle segments between cardinal points and midpoint of great circle connecting origin and destination
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# north-east

ne_gc_mid_intermediate_e <- 
	gcIntermediate(
		c(first(ne_gc$longitude), first(ne_gc$latitude)), 
		c(great_circle_mid$longitude, great_circle_mid$latitude), 
		n = 50, 
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)

ne_gc_mid_intermediate_n <- 
	gcIntermediate(
		c(last(ne_gc$longitude), last(ne_gc$latitude)), 
		c(great_circle_mid$longitude, great_circle_mid$latitude), 
		n = 50,
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)


# east-south

es_gc_mid_intermediate_e <- 
	gcIntermediate(
		c(first(es_gc$longitude), first(es_gc$latitude)), 
		c(great_circle_mid$longitude, great_circle_mid$latitude), 
		n = 50, 
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)

es_gc_mid_intermediate_s <- 
	gcIntermediate(
		c(last(es_gc$longitude), last(es_gc$latitude)), 
		c(great_circle_mid$longitude, great_circle_mid$latitude), 
		n = 50,
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)


# south-west

sw_gc_mid_intermediate_w <- 
	gcIntermediate(
		c(first(sw_gc$longitude), first(sw_gc$latitude)), 
		c(great_circle_mid$longitude, great_circle_mid$latitude), 
		n = 50, 
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)

sw_gc_mid_intermediate_s <- 
	gcIntermediate(
		c(last(sw_gc$longitude), last(sw_gc$latitude)), 
		c(great_circle_mid$longitude, great_circle_mid$latitude), 
		n = 50,
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)


# west-north

wn_gc_mid_intermediate_n <- 
	gcIntermediate(
		c(first(wn_gc$longitude), first(wn_gc$latitude)), 
		c(great_circle_mid$longitude, great_circle_mid$latitude), 
		n = 50, 
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)

wn_gc_mid_intermediate_w <- 
	gcIntermediate(
		c(last(wn_gc$longitude), last(wn_gc$latitude)), 
		c(great_circle_mid$longitude, great_circle_mid$latitude), 
		n = 50,
		addStartEnd = TRUE
	) %>%
	as_tibble() %>%
	rename(longitude = lon, latitude = lat)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Combining great circle segments into a 1/4 circle polygons
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

ne_poly <- 
	bind_rows(
		great_circle_mid,
		ne_gc_mid_intermediate_n,
		ne_gc,
		ne_gc_mid_intermediate_e,
		great_circle_mid
	)

es_poly <- 
	bind_rows(
		great_circle_mid,
		es_gc_mid_intermediate_e,
		es_gc,
		es_gc_mid_intermediate_s,
		great_circle_mid
	)

sw_poly <- 
	bind_rows(
		great_circle_mid,
		sw_gc_mid_intermediate_s,
		sw_gc,
		sw_gc_mid_intermediate_w,
		great_circle_mid
	)

wn_poly <- 
	bind_rows(
		great_circle_mid,
		wn_gc_mid_intermediate_w,
		wn_gc,
		wn_gc_mid_intermediate_n,
		great_circle_mid
	)


#-----------------------------------------------------------------------------------------#
# 2 degree lat/lon lines
#-----------------------------------------------------------------------------------------#

lon_groups_2 <- 
	tibble(longitude = seq(180, -180, -2)) %>% 
	mutate(lon_group = 1:nrow(.))

lat_groups_2 <- 
	tibble(latitude = seq(80, -80, -2)) %>% 
	mutate(lat_group = 1:nrow(.))

lat_lon_lines_2 <- 
	expand_grid(
		longitude = seq(180, -180, -2), 
		latitude = seq(80, -80, -2)
	) %>% 
	left_join(., lon_groups_2) %>% 
	left_join(., lat_groups_2)


#-----------------------------------------------------------------------------------------#
# 10 degree lat/lon lines
#-----------------------------------------------------------------------------------------#

lon_groups_10 <- 
	tibble(longitude = seq(180, -180, -10)) %>% 
	mutate(lon_group = 1:nrow(.))

lat_groups_10 <- 
	tibble(latitude = seq(80, -80, -10)) %>% 
	mutate(lat_group = 1:nrow(.))

lat_lon_lines_10 <- 
	expand_grid(
		longitude = seq(180, -180, -10), 
		latitude = seq(80, -80, -10)
	) %>% 
	left_join(., lon_groups_10) %>% 
	left_join(., lat_groups_10)


#=========================================================================================#
# Plotting ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Constructing plot
#-----------------------------------------------------------------------------------------#

flights_plot <- 
	
	flights %>% 
	
	ggplot(aes(longitude, latitude)) +
	
	
	# Blue polygons
	
	annotate(
		"polygon",
		x = en_poly$longitude,
		y = en_poly$latitude,
		fill = "gray70"
	) +
	
	annotate(
		"polygon",
		x = es_poly$longitude,
		y = es_poly$latitude,
		fill = "gray70"
	) +
	
	annotate(
		"polygon",
		x = ws_poly$longitude,
		y = ws_poly$latitude,
		fill = "gray70"
	) +
	
	annotate(
		"polygon",
		x = nw_poly$longitude,
		y = nw_poly$latitude,
		fill = "gray70"
	) +
	
	
	# lat/lon lines
	# (can change the direction of the lines based on the grouping)
	
	geom_path(
		data = lat_lon_lines_2,
		aes(longitude, latitude, group = lon_group),
		inherit.aes = FALSE,
		color = "gray55",
		size = 0.25
	) +
	geom_path(
		data = lat_lon_lines_2,
		aes(longitude, latitude, group = lat_group),
		inherit.aes = FALSE,
		color = "gray55",
		size = 0.25
	) +
	geom_path(
		data = lat_lon_lines_10,
		aes(longitude, latitude, group = lon_group),
		inherit.aes = FALSE,
		color = "black",
		size = 0.25
	) +
	geom_path(
		data = lat_lon_lines_10,
		aes(longitude, latitude, group = lat_group),
		inherit.aes = FALSE,
		color = "black",
		size = 0.25
	) +
	
	
	# World country boundaries
	
	geom_polygon(
		data = world_countries,
		aes(longitude, latitude, group = piece_id),
		fill = "gray20",
		color = "gray30"
	) +
	
	# World coastline

	geom_path(
		data = world_coastline,
		aes(longitude, latitude, group = piece_id),
		color = "gray20"
	) +

	# # World capitals pop > 1 million ("\u2605" is a black star)
	# 
	# geom_point(
	# 	data = world_capitals %>% filter(pop > 1e6),
	# 	aes(longitude, latitude),
	# 	shape = "\u2605",
	# 	color = "white",
	# 	size = 3
	# ) +
	
	
	# Drawing great circle between origin and destination
	
	annotate(
		"path",
		great_circle$longitude,
		great_circle$latitude,
		color = "white",
		size = 1
	) +
	
	# Drawing origin and destination points
	
	geom_point(
		data = orig_dest,
		aes(longitude, latitude),
		color = "white",
		size = 4,
		shape = 16
	) +
	
	
	# Adding origin and destination labels
	
	geom_label_repel(
		data = orig_dest,
		aes(longitude, latitude, label = iata),
		fill = "white",
		color = "black",
		label.r = 0, 
		direction = "x",
		point.padding = unit(0.02, "npc")
	) +
	
	
	# Paths between origin and destination, color-coded based on direction
	
	geom_path(
		aes(color = start_end),
		size = 1
	) +

	# labs(title = glue("Flights between {}")) +
	
	
	geom_path(
		data = flights_mid,
		aes(longitude, latitude, color = start_end),
		arrow = 
			arrow(
				angle = 25, 
				length = unit(0.04, "npc"), 
				ends = "last", 
				type = "closed"
			),
		alpha = .6
	) +
	
	scale_color_manual(values = c("red", "blue")) +
	
	guides(color = "none") +

	
	# Defining projection, orientation, and plot boundaries
	
	coord_map(
		projection = "orthographic",
		orientation =
			c(
				great_circle_mid$latitude,
				great_circle_mid$longitude,
				0
			)
	) +

	theme_bw() +
	
	theme(
		axis.title = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank(),
		panel.border = element_blank(),
		panel.grid = element_blank(),
		legend.position = "bottom"
	)


#-----------------------------------------------------------------------------------------#
# Saving plot ----
#-----------------------------------------------------------------------------------------#

ggsave(
	glue(
		"plots/", 
		str_c(str_replace(flight_orig_dest_iata, pattern = "<|>", "-"), collapse = ", "),
		".png"
	), 
	plot = flights_plot,
	width = 10, 
	height = 10
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
