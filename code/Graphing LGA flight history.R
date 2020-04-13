###########################################################################################-
###########################################################################################-
##
## Graphing LGA flight history ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

run_in_parallel <- FALSE
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
library(ggdark)
library(doSNOW)
library(parallel)
library(doParallel)
library(tictoc)

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

frames_folder <- here(glue("plots/graph_frames/{date_to_map}/30_sec"))

dir_create(frames_folder)

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

#=========================================================================================#
# Loading and cleaning data ----
#=========================================================================================#

# Downloaded in "LGA flight history.R"

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
	
	# Records with missing values are probably not "real" data points, so dropping them
	
	drop_na(time, longitude, latitude, unique_flight) %>%
	
	# Some of the records are doubled, which makes moveVis mad, so keeping only one
	
	distinct(time, unique_flight, .keep_all = TRUE) %>% 
	
	# Restricting path data to coordinates inside the bounding box, to reduce unnecessary processing and 
	# 	memory overhead, and also so that the eventual summary statistics correspond to paths that
	# 	are visible on the map
	
	filter(
		longitude %>% between(lga_bbox$longitude[1], lga_bbox$longitude[2]),
		latitude  %>% between(lga_bbox$latitude[1], lga_bbox$latitude[2])
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
# Converting for moveVis
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Turning the data.frame into a MoveStack
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

# To get accurate positions at each half-minute, this aligns the timestamps by interpolating the 
# 	position of each flight at the given half-minute interval

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

# Removing intermediate objects, because rapidly saving many ggplots (often in parallel) can
# 	be memory intensive

# rm(arrivals_move)
# rm(arrivals_tracks)

gc()

#-----------------------------------------------------------------------------------------#
# data.frame of distinct times and time label coords
#-----------------------------------------------------------------------------------------#

# This will be fed to "geom_label"

distinct_times <- 
	tibble(
		time = seq(date_min, date_max, "30 secs")
	) %>% 
	mutate(
		time_chr = format(time, format = "%a %m/%e - %R"),
		longitude = mean(lga_bbox$longitude), 
		latitude = max(lga_bbox$latitude) - ((max(lga_bbox$latitude) - min(lga_bbox$latitude)) * 0.05)
	)


#-----------------------------------------------------------------------------------------#
# Extracting data from the aligned MoveStack
#-----------------------------------------------------------------------------------------#

arrivals_move_aligned_trimmed <- 
	arrivals_move_aligned@data %>% 
	as_tibble(rownames = "row_names") %>% 

	# Removing "fake_flight" from the count
	
	filter(!row_names %>% str_detect("fake")) %>% 
	
	# Re-creating unique_flight
	
	mutate(unique_flight = row_names %>% str_remove_all("\\..+")) %>% 
	
	# There shouldn't be any flight coords outside the bbox, but just to make sure...
	
	filter(
		x %>% between(lga_bbox$longitude[1], lga_bbox$longitude[2]),
		y %>% between(lga_bbox$latitude[1], lga_bbox$latitude[2])
	)

#-----------------------------------------------------------------------------------------#
# Summarizing
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# For each minute
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

arrivals_move_aligned_trimmed_summarized <- 
	arrivals_move_aligned_trimmed %>% 
	
	# The y-axis makes sense as "Flights / Minute", so keeping "60 secs" here. Joining with
	# 	"min_max_seq_df" by "hour" and "minute" will replicate this count for each 30 sec frame
	
	mutate(floor_time = floor_date(time, unit = "60 secs")) %>% 
	
	# Records will be doubled, as there are two 30-second segments within each 60 second segment,
	# 	so here I'm removing the duplicates
	
	distinct(unique_flight, floor_time, .keep_all = TRUE) %>% 
	
	count(floor_time, name = "flights_count") %>% 
	mutate(
		day = day(floor_time),
		hour = hour(floor_time),
		minute = minute(floor_time)
	) %>%
	
	# Times are rounded to different resolutions, which means "time" will not match, so I'm removing it
	
	select(-floor_time)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Complete summarized data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# 30 second grid with fake coords

min_max_seq_df <-
	tibble(
		time = seq(date_min, date_max, "30 secs"),
		longitude = 0,
		latitude = 0,
		unique_flight = "none"
	) %>% 
	mutate(
		day = day(time),
		hour = hour(time), 
		minute = minute(time)
	)

# Expanding the summarized data to have a record for each 30 second interval

arrivals_summarized <- 
	full_join(
		min_max_seq_df,
		arrivals_move_aligned_trimmed_summarized,
		by = c("day", "hour", "minute")
	) %>%
	mutate(
		flights_count = replace_na(flights_count, 0),
		moving_flights_count = movingFun(flights_count, 120, "mean", na.rm = TRUE)
	)


#=========================================================================================#
# Graph ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Drawing graphs
#-----------------------------------------------------------------------------------------#

# To construct a manual legend, specifying `group = 1` and `colour = [text]` for the label

graph_frames <-
	distinct_times$time %>%
	map(
		~ arrivals_summarized %>% 
			ggplot() +
			
			# 0-point
			
			geom_hline(yintercept = 0, color = "gray20", linetype = 2) +
			
			# Daily average
			
			geom_hline(
				aes(
					yintercept = mean(flights_count),
					group = 1,
					colour = "Daily average"
				),
				size = .75
			) +
			
			# Count in each minute
			
			geom_line(
				aes(
					x = time,
					y = flights_count,
					group = 1,
					colour = "Each minute"
				),
				size = .5
			) +
			
			# A loess smooth
			
			geom_smooth(
				aes(
					x = time,
					y = moving_flights_count,
					group = 1,
					colour = "loess (span = 0.5)"
				),
				method = "loess",
				formula = y ~ x,
				span = .5, 
				se = FALSE,
				size = 1
			) +
			
			# A moving average over a window of 1 hour
			
			geom_line(
				aes(
					x = time,
					y = moving_flights_count,
					group = 1,
					colour = "Moving average (window = 1 hour)"
				),
				size = 1
			) +
			
			# Vertical line scanning with time
			
			geom_vline(aes(xintercept = .x), color = "white") +
			
			# Constructing manual legend and setting colors of lines (on the graph and in the legend)
			
			scale_colour_manual(
				name = NULL,
				breaks = c("Each minute", "Moving average (window = 1 hour)", "loess (span = 0.5)", "Daily average"),
				values = c("gray25", "#FDE725", "#424186", "#2AB07F"),
				guide = guide_legend(direction = "vertical", override.aes = aes(size = 1.5))
			) +
			
			# Plot display specs
			
			scale_y_continuous(name = "Flights / Minute", breaks = seq(0, 10, 2)) +
			scale_x_datetime(
				name = "Time",
				date_labels = "%k:%M",
				date_breaks = "2 hours"
			) +
			coord_cartesian(
				xlim = c(date_min, date_max), 
				ylim = c(0, 11)
			) +
			dark_theme_gray() +
			
			# Tweaking the plot display
			
			theme(
				legend.justification = c(0, 1),                          # Upper left
				legend.position = c(0, 1),                               # Upper left
				legend.background = element_rect(fill = NA, color = NA), # No background
				legend.title = element_blank(),                          # Mo title
				legend.margin = margin(t = 0, r = 10, b = 5, l = 10),
				legend.key = element_rect(fill = NA),                    # No fill
				text = element_text(size = 12)                           # Make it big
			) 
			
	) %>% 
	
	# To give clusterApplyLB a way of accurately naming frames in order, if running in parallel
	
	imap( ~ `attr<-`(.x, which = "frame", .y))
			

gc()


#-----------------------------------------------------------------------------------------#
# Saving graph frames ----
#-----------------------------------------------------------------------------------------#

if (run_in_parallel == TRUE) {
	
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	# Setting up parallel saivng of graphs
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	
	cl <- makeSOCKcluster(3)
	
	clusterExport(cl, c("frames_folder", "ggsave", "here", "glue"))
	
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	# Running in parallel
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	
	format(now(), "%r"); tic()
	
	clusterApplyLB(
		cl = cl,
		x = graph_frames,
		fun = function(graph_frame) {
			
			if (attr(graph_frame, which = 'frame') == 1) cat(length(graph_frames), " total\n", sep = "")
			
			suppressMessages(
				ggsave(
					filename = glue("{frames_folder}/graph_frame_{attr(graph_frame, which = 'frame')}.png"),
					plot = graph_frame,
					width = 8,
					height = 3.5,
					units = "in",
					dpi = 200
				)
			)
			
			if (attr(graph_frame, which = 'frame') %% 10 == 0) cat(attr(graph_frame, which = 'frame'), ",", sep = "")
			
		}
	)
	
	format(now(), "%r"); toc()
	
	stopCluster(cl)
	
} else if (run_in_parallel == FALSE) {
	
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	# Running in series
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	
	format(now(), "%r"); tic()
	
	graph_frames %>%
		iwalk(
			~ {
				# Printing progress
				
				if (.y == 1) cat(length(graph_frames), " total\n", sep = "")
				if (.y %% 10 == 0) cat(.y, ",", sep = "")
				
				suppressMessages(
					ggsave(
						filename = glue("{frames_folder}/graph_frame_{.y}.png"),
						plot = .x,
						width = 8,
						height = 3.5,
						units = "in",
						dpi = 200
					)
				)
				
				# Collecting garbage every 100 frames
				
				if (.y %% 100 == 0) gc()
			}
		)
	
	format(now(), "%r"); toc()
	
}

rm(graph_frames)

gc()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
