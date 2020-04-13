###########################################################################################-
###########################################################################################-
##
## Mapping LGA flight history ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

date_to_map     <- lubridate::as_date("2020-04-06")
start_time      <- hms::as_hms("00:00:01")
end_time        <- hms::as_hms("23:59:59")

#-----------------------------------------------------------------------------------------#
# Loading libraries
#-----------------------------------------------------------------------------------------#

library(magrittr)
library(moveVis)
library(move)
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
library(iterators)

#-----------------------------------------------------------------------------------------#
# Parsing dates for filtering data "server" side
#-----------------------------------------------------------------------------------------#

month_to_map <- month(date_to_map)
day_to_map   <- day(date_to_map)

start_hour   <- hour(start_time)
start_minute <- minute(start_time)

end_hour   <- hour(end_time)
end_minute <- minute(end_time)

#-----------------------------------------------------------------------------------------#
# Creating frames folder
#-----------------------------------------------------------------------------------------#

frames_folder <- here(glue("plots/map_frames/{date_to_map}/30_sec"))

dir_create(frames_folder)


#-----------------------------------------------------------------------------------------#
# moveVis options
#-----------------------------------------------------------------------------------------#

use_multicore(n_cores = 3)
use_disk(n_memory_frames = 100)

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

arrivals_tracks <- 
	lga_tracks_db %>% 
	tbl("arrivals_tracks") %>% 
	
	filter(
		month == month_to_map, 
		day   == day_to_map,
		
		hour   %>% between(start_hour, end_hour),
		minute %>% between(start_minute, end_minute)
		
	) %>% 
	
	collect() %>% 
	mutate(time = as_datetime(time, tz = "US/Eastern")) %>% 
	drop_na(time, longitude, latitude, unique_flight) %>%
	distinct(time, unique_flight, .keep_all = TRUE) %>% 
	
	# Expanding the bbox, so that the tracks come from off screen
	
	filter(
		longitude %>% between(lga_bbox_W, lga_bbox_E),
		latitude  %>% between(lga_bbox_S, lga_bbox_N)
	)

dbDisconnect(lga_tracks_db)

# I want a frame for each half-minute of the day, but it's likely that not every half-minute interval will be
# 	present in the data, which means it will not be in the "move" object, which will mess up the frames.
# 	To avoid this, I'll add a whole day of fake data, corresponding to a persistent point that is placed 
# 	behind the time label, by using the same formula I use to position the label.

date_min <-
	as_datetime(str_c(date_to_map, start_time, sep = " "), tz = "US/Eastern") %>% 
	floor_date(unit = "30 seconds")

date_max <-
	as_datetime(str_c(date_to_map, end_time, sep = " "), tz = "US/Eastern") %>% 
	ceiling_date(unit = "30 seconds")

arrivals_tracks <-
	bind_rows(
		tibble(
			time = seq(date_min, date_max, "30 secs"),
			longitude = mean(lga_bbox$longitude),
			latitude = max(lga_bbox$latitude) - ((max(lga_bbox$latitude) - min(lga_bbox$latitude)) * 0.05),
			unique_flight = "fake_flight"
		),
		arrivals_tracks
	)


#-----------------------------------------------------------------------------------------#
# Converting to moveStack
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Converting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

arrivals_move <- 
	df2move(
		arrivals_tracks,
		proj = my_crs, 
		x = "longitude", 
		y = "latitude", 
		time = "time", 
		track_id = "unique_flight"
	)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Aligning
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

arrivals_move_aligned <- 
	align_move(
		arrivals_move, 
		res = 30, 
		digit = 0, 
		unit = "secs"
	)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Cleaning up
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# rm(arrivals_move)
# rm(arrivals_tracks)

gc()

#-----------------------------------------------------------------------------------------#
# data.frame of distinct times
#-----------------------------------------------------------------------------------------#

# along with coordinates of where a time label should be plotted

distinct_times <- 
	tibble(
		time = seq(date_min, date_max, "30 secs")
	) %>% 
	mutate(
		time_chr = format(time, format = "%a %m/%d - %R"),
		longitude = mean(lga_bbox$longitude), 
		latitude = max(lga_bbox$latitude) - ((max(lga_bbox$latitude) - min(lga_bbox$latitude)) * 0.05)
	)


#=========================================================================================#
# Constructing visualization ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Map ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Setting up
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Getting number of distinct flights, for "n" argument to `sample()`

n_flights <- n_distinct(arrivals_move_aligned@trackId)

# Setting seed to reproduce sampled colors from viridis_pal

set.seed(1)

# Sampling colors

flight_path_colors <- sample(viridis_pal()(256), n_flights, replace = TRUE)

# Empty raster background

empty_raster <- list(`crs<-`(x = raster(), value = my_crs))

# sequential file naming iterator

filename_iter <- icount()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Mapping
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Timing the run

format(now(), "%r"); tic()

map_frames <- 
	frames_spatial(
		arrivals_move_aligned,
		map_service = NA, 
		map_type = NULL, 
		r_list = empty_raster,
		r_times = arrivals_move_aligned$time,
		ext = lga_bbox %>% as.matrix() %>% bbox() %>% extent(),
		margin_factor = 1.5,
		path_size = 4,
		tail_size = 0.25,
		path_alpha = 1,
		path_colours = flight_path_colors,
		path_end = "round",
		trace_show = TRUE,
		tail_colour = "gray50",
		trace_colour = "gray50",
		maxpixels = 1600^2,
		equidistant = FALSE,
		path_legend = FALSE
	) %>% 
	add_labels(x = NULL, y = NULL) %>%
	add_gg(
		frames = .,
		gg = expr(
			geom_label(
				data = data,
				aes(x = longitude, y = latitude, label = time_chr),
				size = 4
			)
		),
		data = distinct_times %>% group_split(row_number())
	) %>%
	add_gg(
		gg = expr(dark_theme_bw())
	) %>% 
	
	
	# Any layers added below "frames_spatial" will be added on top of the layers added by that function,
	# 	which includes the paths. I can instead modify the "layers" element of each ggplot object (i.e., frame)
	# 	to add the map geoms below all other layers 
	
	map(
		~ modify_at(
			
			# This ".x" refers to the frame/ggplot object, because it's passed from "map":
			
			.x, 
			"layers", 
			~ c(
				geom_polygon(
					aes(x = x, y = y, group = piece_id),
					data = tri_state_counties,
					color = "black",
					fill = "black",
					size = .5
				), 
				geom_path(
					aes(x = x, y = y, group = piece_id),
					data = tri_state_coastline,
					color = "gray15",
					size = .5
				), 
				geom_polygon(
					aes(x = x, y = y, group = piece_id),
					data = nyc_boro_bounds,
					color = "black",
					fill = "gray15",
					size = .125
				), 
				geom_path(
					aes(x = x, y = y, group = piece_id),
					data = lga_runways,
					color = "gold",
					size = 1
				),
				geom_polygon(
					aes(x = x, y = y, group = piece_id),
					data = central_park,
					color = "gray9",
					fill = "gray9",
					size = .25
				),
				
				# This ".x" refers to the "layers" element of the ggplot object, because it's passed from "modify_at":
				
				.x
			)
		)
	) %>% 
	
	
	# Saving the frames as we go (because there's a frame every minute, I can derive the frame number from the time)
	# 	This massively reduces the memory overhead of saving frames after assigning them to a list.
	
	add_gg(
		frames = .,
		gg = expr(
			suppressMessages(
				ggsave(
					# filename =
					# 	here(
					# 		glue(
					# 			"plots/map_frames/{date_to_map}/map_frame_",
					# 			# "{attr(data, which = 'time') %>% map_dbl( ~ ((hour(.x) * 60) + minute(.x)))}",
					# 			"{
					# 				attr(data, which = 'time') %>% 
					# 				map_chr( ~ ((hour(.x) * 60) + minute(.x) + (second(.x)/60)) %>% format(nsmall = 1))
					# 			}",
					# 			".png"
					# 		)
					# 	),
					filename =
						glue(
							"{frames_folder}/map_frame_{nextElem(filename_iter)}.png"
						),
					plot = data,
					width = 8,
					height = 8,
					units = "in",
					dpi = 200,
					bg = "black" # making sure that there are no white bands
				)
			)
		),
		data = .
	) # %>% 
	
	# To give clusterApplyLB a way of accurately naming frames in order
	
	# imap( ~ .x %>% inset("frame", .y))


format(now(), "%r"); toc()

gc()


#-----------------------------------------------------------------------------------------#
# Saving in separate step ----
#-----------------------------------------------------------------------------------------#

# Creating frames folder

# dir_create(here(glue("plots/map_frames/{date_to_map}")))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Setting up parallel saivng of graphs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# cl <- makeSOCKcluster(2)
# 
# clusterExport(cl, c("date_to_map", "ggsave", "here", "glue"))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Running in parallel
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# format(now(), "%r"); tic()
# 
# clusterApplyLB(
# 	cl = cl,
# 	x = map_frames,
# 	fun = function(map_frame) {
# 		
# 		suppressMessages(
# 			ggsave(
# 				filename = here(glue("plots/map_frames/{date_to_map}/map_frame_{map_frame$frame}.png")),
# 				plot = map_frame,
# 				width = 8,
# 				height = 8,
# 				units = "in",
# 				dpi = 200
# 			)
# 		)
# 		
# 	}
# )
# 
# format(now(), "%r"); toc()
# 
# stopCluster(cl)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Running in series ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Creating frames folder

# dir_create(here(glue("plots/map_frames/{date_to_map}")))
# 
# # Saving frames
# 
# format(now(), "%r"); tic()
# 
# for (i in 1:length(map_frames)) {
# 
# 	# Printing progress
# 
# 	# if (i == 1) cat(length(map_frames), " total\n", sep = "")
# 
# 	# cat(i, ",", sep = "")
# 
# 	suppressMessages(
# 		ggsave(
# 			filename = here(glue("plots/map_frames/{date_to_map}/map_frame_{map_frames[[i]]$frame}.png")),
# 			plot = map_frames[[i]],
# 			width = 8,
# 			height = 8,
# 			units = "in",
# 			dpi = 200
# 		)
# 	)
# 
# }
# 
# format(now(), "%r"); toc()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Cleaning up ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

rm(map_frames)

gc()

.rs.restartR()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
