###########################################################################################-
###########################################################################################-
##
## Making an example GIF of LGA flight history ----
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
library(av)
library(gifski)
library(hms)
library(glue)
library(fs)

#-----------------------------------------------------------------------------------------#
# Which dates are we combining?
#-----------------------------------------------------------------------------------------#

video_date_1 <- "2020-03-16" %>% as_date()
video_date_2 <- "2020-04-06" %>% as_date()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# What times are we subsetting?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

start_time_real <- hms(0, 0, 18)
end_time_real   <- hms(0, 0, 19)

# 2 frames per minute. First getting number of minutes, then doubling to get frame number

start_frame <- ((hour(start_time_real) * 60) + minute(start_time_real)) * 2
end_frame <- ((hour(end_time_real) * 60) + minute(end_time_real)) * 2

# 30 fps, so frame_number/30 give the nuber of seconds

start_time_video <- start_frame/30
end_time_video   <- end_frame/30

video_duration <- end_time_video - start_time_video

"ffmpeg -ss [start] -i in.mp4 -t [duration] -c copy out.mp4"


#-----------------------------------------------------------------------------------------#
# Getting video paths
#-----------------------------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Paths
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

video_files <- dir_ls("plots/videos")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Choosing among potential options
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

#==== Video 1 ====# 

video_1_choices <- 
	video_files %>% 
	str_subset(as.character(video_date_1))


if (length(video_1_choices) > 1) {
	
	video_1 <- 
		menu(
			choices = video_1_choices, 
			graphics = TRUE,
			title = "Which video do you want to use for video_1?"
		) %>% 
		pluck(video_1_choices, .)
	
} else {
	
	video_1 <- video_1_choices
	
}


#==== Video 2 ====# 

video_2_choices <- 
	video_files %>% 
	str_subset(as.character(video_date_2))


if (length(video_2_choices) > 1) {
	
	video_2 <- 
		menu(
			choices = video_2_choices, 
			graphics = TRUE,
			title = "Which video do you want to use for video_2?"
		) %>% 
		pluck(video_2_choices, .)
	
} else {
	
	video_2 <- video_2_choices
	
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Root of the video name
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

video_name_root <- 
	video_1 %>% 
	path_file() %>% 
	path_ext_remove() %>% 
	str_remove(as.character(video_date_1))



#=========================================================================================#
# Stacking ----
#=========================================================================================#

video_path <- 
	glue(
		"plots/videos/",
		"{video_name_root}-",
		"_{video_date_1}_vs._{video_date_2}",
		"_{start_time_video}sec-{end_time_video}sec.mp4"
	)

system(
	glue(
		"ffmpeg -hide_banner -y",
		
		"-ss {start_time_video}",
		"-i {video_1}",
		"-t {video_duration}",
		
		"-ss {start_time_video}",
		"-i {video_2}",
		"-t {video_duration}",
		
		"-filter_complex hstack=inputs=2",
		"-c:v libx265",
		"-x265-params range=full:qp=20",
		"-pix_fmt yuvj420p",
		"{video_path}",
		.sep = " "
	)
)


#=========================================================================================#
# Converting short video to GIF ----
#=========================================================================================#

#-----------------------------------------------------------------------------------------#
# Reading video as images
#-----------------------------------------------------------------------------------------#

video_images <- av_video_images(video_path, format = "png")

gifski(
	png_files = video_images, 
	gif_file = path_ext_set(video_path, "gif"),
	width = 1200,
	height = (2300/3200)*1200,
	delay = 1/25,
	progress = TRUE
)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
