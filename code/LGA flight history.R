###########################################################################################-
###########################################################################################-
##
## LGA flight history ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------------------------------------#

library(rvest)
library(tidyverse)
library(httr)
library(lubridate)
library(glue)
library(fs)
library(hms)
library(iterators)
library(janitor)
library(here)
library(DBI)
library(RSQLite)
library(dbplyr)
library(keyring)


#=========================================================================================#
# Scraping data ----
#=========================================================================================#

base_url <- "https://flightaware.com/live/airport/KLGA"

#-----------------------------------------------------------------------------------------#
# Flights ----
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Arrivals ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

arrivals_tbl <- tibble()

offset_rows <- 20

offset_inf <- icount()

repeat {
	
	offset_iter <- nextElem(offset_inf)
	
	if (offset_iter %% 5 == 0) cat(offset_iter, ": ", (offset_iter - 1) * offset_rows, sep = "")
	
	arrivals_prettyTable <- 
		read_html(
			glue("{base_url}/arrivals/all?;offset={(offset_iter - 1) * offset_rows};order=actualarrivaltime;sort=DESC")
		) %>% 
		html_nodes("table.prettyTable")
	
	# Extracting links to individual flights
	
	arrivals_links <- 
		arrivals_prettyTable %>% 
		html_nodes("tr") %>% 
		html_nodes("a[href^='/live/flight']") %>% 
		html_attrs() %>% 
		flatten_chr()
	
	
	# Formatted table
	
	arrivals_tbl_temp <- 
		arrivals_prettyTable %>% 
		html_table(header = FALSE, fill = TRUE) %>% 
		flatten_dfc() %>% 
		select(1:5) %>% 
		set_names(nm = slice(., 2)) %>% 
		clean_names() %>% 
		filter(!ident %>% str_detect("Sorry|Arrivals|Ident")) %>% 
		mutate(flight_links = str_c("https://flightaware.com", arrivals_links))
	
	
	# Stopping if no actual records
	
	if (nrow(arrivals_tbl_temp) == 0) break
	
	
	# Binding rows
	
	arrivals_tbl <- 
		bind_rows(
			arrivals_tbl,
			arrivals_tbl_temp
		)
	
	Sys.sleep(runif(n = 1, min = 3, max = 5))
	
}


# Saving tracklog

write_rds(arrivals_tbl, here("data/arrivals_tbl_2020-4-11.rds"))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Departures ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

departures_tbl <- tibble()

offset_rows <- 20

offset_inf <- icount()

repeat {
	
	offset_iter <- nextElem(offset_inf)
	
	if (offset_iter %% 5 == 0) cat(offset_iter, ": ", (offset_iter - 1) * offset_rows, sep = "")
	
	departures_prettyTable <- 
		read_html(
			glue("{base_url}/departures/all?;offset={(offset_iter - 1) * offset_rows};order=actualdeparturetime;sort=DESC")
		) %>% 
		html_nodes("table.prettyTable")
	
	# Extracting links to individual flights
	
	departures_links <- 
		departures_prettyTable %>% 
		html_nodes("tr") %>% 
		html_nodes("a[href^='/live/flight']") %>% 
		html_attrs() %>% 
		flatten_chr()
	
	
	# Formatted table
	
	departures_tbl_temp <- 
		departures_prettyTable %>% 
		html_table(header = FALSE, fill = TRUE) %>% 
		flatten_dfc() %>% 
		select(1:5) %>% 
		set_names(nm = slice(., 2)) %>% 
		clean_names() %>% 
		filter(!ident %>% str_detect("Sorry|Departures|Ident")) %>% 
		mutate(flight_links = str_c("https://flightaware.com", departures_links))
	
	
	# Stopping if no actual records
	
	if (nrow(departures_tbl_temp) == 0) break
	
	
	# Binding rows
	
	departures_tbl <- 
		bind_rows(
			departures_tbl,
			departures_tbl_temp
		)
	
	Sys.sleep(runif(n = 1, min = 3, max = 5))
	
}


# Saving tracklog

write_rds(departures_tbl, here("data/departures_tbl_2020-4-11.rds"))


#-----------------------------------------------------------------------------------------#
# Track logs ----
#-----------------------------------------------------------------------------------------#

lga_tracks_db <- dbConnect(SQLite(), "data/lga_tracks_db.sqlite3")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Arrivals ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

arrivals_tbl <- read_rds(here("data/arrivals_tbl.rds"))

# source("code/FlightAware token.R")

for (i in 1:nrow(arrivals_tbl)) {
	
	if (i == 1) cat(as.character(now()), "\n", nrow(arrivals_tbl), "total arrivals\n")
	
	if (i %% 10 == 0) cat(i, ",", sep = "")
	
	# Link from the flights table redirects to the actual link. This gets the redirected URL.
	
	real_arrivals_flight_link <- GET(arrivals_tbl$flight_links[i]) %>% pluck("url")
	
	Sys.sleep(runif(n = 1, min = 0.5, max = 1.5))
	
	arrivals_tracks_raw <- 
		read_html(str_c(real_arrivals_flight_link, "/tracklog")) %>% 
		html_nodes("table.prettyTable") %>% 
		html_table(header = FALSE) %>% 
		flatten_dfc()
	
	
	if (nrow(arrivals_tracks_raw) == 0) {
		
		cat(glue("*ERROR* ({i}), "), sep = "")
		
		next
	}
	
	arrivals_tracks <- 
		arrivals_tracks_raw %>% 
		set_names(nm = c("time", "latitude", "longitude", "course", "kts", "mph", "feet", "rate", "reporting_facility")) %>% 
		filter(
			!time %>% str_detect("Time|Airline|FlightAware|Taxi"),
			!longitude %>% str_detect("Mon|Tue|Wed|Thu|Fri|Sat|Sun")
		) %>% 
		type_convert(col_types = cols(.default = col_guess())) %>% 
		drop_na(reporting_facility, latitude, longitude) %>% 
		mutate(
				
				link = real_arrivals_flight_link,
				
				latitude = 
					case_when(
						latitude %>% str_detect("-") ~ latitude %>% str_sub(start = 1, end = 8) %>% as.numeric(),
						TRUE ~ latitude %>% str_sub(start = 1, end = 7) %>% as.numeric()
					),
				longitude = 
					case_when(
						longitude %>% str_detect("-") ~ longitude %>% str_sub(start = 1, end = 8) %>% as.numeric(),
						TRUE ~ longitude %>% str_sub(start = 1, end = 7) %>% as.numeric()
					),
				
				origin_code = link %>% str_split("/") %>% map_chr(~nth(.x, -2L)),
				
				flight_number = link %>% str_split("/") %>% map_chr(~nth(.x, -6L)),
				
				flight_date = 
					link %>% 
					str_split("/") %>% 
					map_chr(~nth(.x, -4L)) %>% 
					parse_date_time(orders = "Ymd", tz = "US/Eastern") %>% 
					as_date(),
				
				flight_time = link %>% str_split("/") %>% map_chr(~nth(.x, -3L)),
				
				unique_flight = str_c(flight_number, "_", str_remove_all(flight_date, "-"), "_", flight_time),
				
				just_time = time %>% str_sub(start = 1, end = 15) %>% parse_date_time("aT") %>% as_hms(),
				time = str_c(flight_date, just_time, sep = " ") %>% as_datetime() %>% force_tz("US/Eastern"),
				
				origin = arrivals_tbl$origin[i],
				
				# If the timestamp of the first waypoint is greater than the timestamp of the last
				# 	waypoint, then the timestamps of the flight tracks cross midnight, and therefore
				# 	the date of the timestamps before midnight is incorrect
				
				crosses_midnight = first(hour(time)) > last(hour(time)),
				
				# Figure out which row has the first timestamp after midnight, and create a variable
				# 	that calls all the rows before that "before midnight"
				
				before_midnight = row_number() < (which(hour(time) == 0)[1]),
				
				# Correct the time
				
				time = 
					if_else(
						condition = crosses_midnight == TRUE & before_midnight == TRUE, 
						true = time - days(1), 
						false = time
					),
				
				# compute datetime components on the corrected time
				
				year = year(time),
				month = month(time),
				day = day(time),
				hour = hour(time),
				minute = minute(time),
				wday = wday(time)
			
		) %>% 
		
		select(-just_time) %>% 
		
		drop_na(latitude, longitude)
	
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	# writing to database
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	
	dbWriteTable(
		lga_tracks_db,
		"arrivals_tracks_411",
		value = arrivals_tracks,
		append = TRUE,
		temporary = FALSE
	)

	Sys.sleep(runif(n = 1, min = 3, max = 5))
	
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Departures ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

departures_tbl <- read_rds(here("data/departures_tbl_2020-4-11.rds"))

# source("code/FlightAware token.R")

for (i in 2:nrow(departures_tbl)) {
	
	if (i == 1) cat(as.character(now()), "\n", nrow(departures_tbl), "total departures\n")
	
	if (i %% 10 == 0) cat(i, ",", sep = "")
	
	# Link from the flights table redirects to the actual link. This gets the redirected URL.
	
	real_departures_flight_link <- GET(departures_tbl$flight_links[i]) %>% pluck("url")
	
	Sys.sleep(runif(n = 1, min = 0.5, max = 1.5))
	
	departures_tracks_raw <- 
		read_html(str_c(real_departures_flight_link, "/tracklog")) %>% 
		html_nodes("table.prettyTable") %>% 
		html_table(header = FALSE) %>% 
		flatten_dfc()
	
	
	if (nrow(departures_tracks_raw) == 0) {
		
		cat(glue("*ERROR* ({i}), "), sep = "")
		
		next
	}
	
	departures_tracks <- 
		departures_tracks_raw %>% 
		set_names(nm = c("time", "latitude", "longitude", "course", "kts", "mph", "feet", "rate", "reporting_facility")) %>% 
		filter(
			!time %>% str_detect("Time|Airline|FlightAware|Taxi"),
			!longitude %>% str_detect("Mon|Tue|Wed|Thu|Fri|Sat|Sun")
		) %>% 
		type_convert(col_types = cols(.default = col_guess())) %>% 
		drop_na(reporting_facility, latitude, longitude) %>% 
		mutate(
			
			link = real_departures_flight_link,
			
			latitude = 
				case_when(
					latitude %>% str_detect("-") ~ latitude %>% str_sub(start = 1, end = 8) %>% as.numeric(),
					TRUE ~ latitude %>% str_sub(start = 1, end = 7) %>% as.numeric()
				),
			longitude = 
				case_when(
					longitude %>% str_detect("-") ~ longitude %>% str_sub(start = 1, end = 8) %>% as.numeric(),
					TRUE ~ longitude %>% str_sub(start = 1, end = 7) %>% as.numeric()
				),
			
			destination_code = link %>% str_split("/") %>% map_chr(~nth(.x, -1L)),
			
			flight_number = link %>% str_split("/") %>% map_chr(~nth(.x, -6L)),
			
			flight_date = 
				link %>% 
				str_split("/") %>% 
				map_chr(~nth(.x, -4L)) %>% 
				parse_date_time(orders = "Ymd", tz = "US/Eastern") %>% 
				as_date(),
			
			flight_time = link %>% str_split("/") %>% map_chr(~nth(.x, -3L)),
			
			unique_flight = str_c(flight_number, "_", str_remove_all(flight_date, "-"), "_", flight_time),
			
			just_time = time %>% str_sub(start = 1, end = 15) %>% parse_date_time("aT") %>% as_hms(),
			time = str_c(flight_date, just_time, sep = " ") %>% as_datetime() %>% force_tz("US/Eastern"),
			
			destination = departures_tbl$destination[i],
			
			# If the timestamp hour of the first waypoint is greater than the timestamp hour of the last
			# 	waypoint, then the timestamps of the flight tracks cross midnight, and therefore
			# 	the date of the timestamps before midnight is incorrect
			
			crosses_midnight = first(hour(time)) > last(hour(time)),
			
			# Figure out which row has the first timestamp after midnight, and create a variable
			# 	that calls all the rows before that "before midnight"
			
			before_midnight = row_number() < (which(hour(time) == 0)[1]),
			
			# Correct the time
			
			time = 
				if_else(
					condition = crosses_midnight == TRUE & before_midnight == TRUE, 
					true = time - days(1), 
					false = time
				),
			
			# compute datetime components on the corrected time
			
			year = year(time),
			month = month(time),
			day = day(time),
			hour = hour(time),
			minute = minute(time),
			wday = wday(time)
			
		) %>% 
		
		select(-just_time) %>% 
		
		drop_na(latitude, longitude)
	
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	# writing to database
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	
	dbWriteTable(
		lga_tracks_db,
		"departures_tracks_411",
		value = departures_tracks,
		append = TRUE,
		temporary = FALSE
	)
	
	Sys.sleep(runif(n = 1, min = 3, max = 5))
	
}

dbDisconnect(lga_tracks_db)


#=========================================================================================#
# Creating indexes ----
#=========================================================================================#

lga_tracks_db <- dbConnect(SQLite(), "data/lga_tracks_db.sqlite3")

index_tbl <- 
	lga_tracks_db %>%
	dbGetQuery("SELECT * FROM sqlite_master WHERE type = 'index'") %>%
	as_tibble()


if (nrow(index_tbl) == 0) {
	
	#-----------------------------------------------------------------------------------------#
	# Creating indexes
	#-----------------------------------------------------------------------------------------#
	
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	# Arrivals
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	
	lga_tracks_db %>% db_create_index("arrivals_tracks", "flight_date")
	lga_tracks_db %>% db_create_index("arrivals_tracks", "year")
	lga_tracks_db %>% db_create_index("arrivals_tracks", "month")
	lga_tracks_db %>% db_create_index("arrivals_tracks", "day")
	lga_tracks_db %>% db_create_index("arrivals_tracks", "hour")
	lga_tracks_db %>% db_create_index("arrivals_tracks", "minute")
	lga_tracks_db %>% db_create_index("arrivals_tracks", "wday")
	lga_tracks_db %>% db_create_index("arrivals_tracks", "origin_code")
	lga_tracks_db %>% db_create_index("arrivals_tracks", "flight_number")
	lga_tracks_db %>% db_create_index("arrivals_tracks", "unique_flight")
	
	
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	# Departures
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	
	lga_tracks_db %>% db_create_index("departures_tracks", "flight_date")
	lga_tracks_db %>% db_create_index("departures_tracks", "year")
	lga_tracks_db %>% db_create_index("departures_tracks", "month")
	lga_tracks_db %>% db_create_index("departures_tracks", "day")
	lga_tracks_db %>% db_create_index("departures_tracks", "hour")
	lga_tracks_db %>% db_create_index("departures_tracks", "minute")
	lga_tracks_db %>% db_create_index("departures_tracks", "wday")
	lga_tracks_db %>% db_create_index("departures_tracks", "destination_code")
	lga_tracks_db %>% db_create_index("departures_tracks", "flight_number")
	lga_tracks_db %>% db_create_index("departures_tracks", "unique_flight")
	
	
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	# vacuuming
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	
	lga_tracks_db %>% dbExecute( "VACUUM")
	
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	# checking indexes
	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
	
	lga_tracks_db %>%
		dbGetQuery("SELECT * FROM sqlite_master WHERE type = 'index'") %>%
		as_tibble()
	
}


#-----------------------------------------------------------------------------------------#
# disconnecting from database
#-----------------------------------------------------------------------------------------#

dbDisconnect(lga_tracks_db)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
