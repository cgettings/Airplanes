###########################################################################################-
###########################################################################################-
##
## Animating LGA flight history ----
##
###########################################################################################-
###########################################################################################-

#=========================================================================================#
# Setting up ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Loading libraries ----
#-----------------------------------------------------------------------------------------#

library(tidyverse)
library(lubridate)
library(glue)

#-----------------------------------------------------------------------------------------#
# Which date are we mapping?
#-----------------------------------------------------------------------------------------#

date_to_map <- "2020-04-06" %>% as_date()

#=========================================================================================#
# Animating ----
#=========================================================================================#

# Using ffmpeg, because with `filter_complex` I can specify the two frames folders as inputs,
# 	stack them on top of each other, and then animate those stacked frames

system(
	glue(
		'ffmpeg -hide_banner -y',
		
		'-framerate 30',
		'-start_number 1',
		'-i "plots/map_frames/{date_to_map}/30_sec/map_frame_%d.png"',
		
		'-framerate 30',
		'-start_number 1',
		'-i "plots/graph_frames/{date_to_map}/30_sec/graph_frame_%d.png"',
		
		# simple way:
		'-filter_complex vstack=inputs=2',
		
		# complicated way:
		# '-filter_complex [0][1]vstack[out] -map [out]',
		
		'-c:v libx265',
		'-x265-params range=full:qp=20',
		'-pix_fmt yuvj420p',
		
		'plots/LGA_Arrivals_30_sec_{date_to_map}.mp4',
		.sep = " "
	)
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
