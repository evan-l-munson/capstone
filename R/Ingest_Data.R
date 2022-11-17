
# Data Source -------------------------------------------------------------

# data was downloaded from: 
#   https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2
#   on 16NOV2022

# Libraries ---------------------------------------------------------------

library(tictoc)
library(data.table)
library(arrow)

# Ingest ------------------------------------------------------------------

tictoc::tic()

# read in this big file ~1.7GB
raw_crime <- data.table::fread(
  file = "data/Crimes_-_2001_to_Present.csv")

tictoc::toc()

# # Split -------------------------------------------------------------------
# 
# # determine an even split line of the data set
# split_num <- nrow(raw_crime) / 2
# split_num_2 <- (nrow(raw_crime) / 2) + 1
# 
# # split data evenly in the middle
# top_half <- raw_crime[1:split_num]
# bot_half <- raw_crime[split_num_2:nrow(raw_crime)]
# 
# # Compress ----------------------------------------------------------------
# 
# # compress top half of data as a parquet file
# # the parquet format appears to compress this data well
# # one limitation of ARC is its 200mb per file upload limit 
# #   with the parquet compression we can split into 2 files
# # save the files in my project "data" folder
# arrow::write_parquet(
#   x = top_half, 
#   sink = "data/top_compressed", 
#   compression = "gzip")
# 
# # compress bottom half of data
# arrow::write_parquet(
#   x = bot_half, 
#   sink = "data/bot_compressed", 
#   compression = "gzip")
# 
# # Read --------------------------------------------------------------------
# 
# # read the parquet files back in once they have been uploaded to your computer 
# #   or into ARC
# # in my case my files are saved in my projects "data" folder
# top_par <- arrow::read_parquet(
#   file = "data/top_compressed")
# 
# bot_par <- arrow::read_parquet(
#   file = "data/bot_compressed")
# 
# # Combine -----------------------------------------------------------------
# 
# # rbind the two files back together
# total_crime <- rbind(top_par, bot_par)
# 
# # Check -------------------------------------------------------------------
# 
# # check that the raw_crime and total_crime df's are the same
# all.equal(
#   target = total_crime, 
#   current = raw_crime)
# 
# # the two df's are equal
# 
# # Compress Deuce -----------------------------------------------------------
# 
# # determine an even split line of the data set
# split_top_third <- floor(nrow(raw_crime) / 3)
# split_mid_third <- split_top_third * 2
# split_bot_third <- as.numeric(nrow(raw_crime))
# 
# # split data evenly in the middle
# top_third <- raw_crime[1:split_top_third]
# mid_third <- raw_crime[(split_top_third+1):split_mid_third]
# bot_third <- raw_crime[(split_mid_third+1):split_bot_third]
# 
# # Compress Deuce -----------------------------------------------------------
# 
# # compress the files into an .rda vs parquet
# save(top_third, file = "data/compressed_top.rda")
# save(mid_third, file = "data/compressed_mid.rda")
# save(bot_third, file = "data/compressed_bot.rda")
# 
# # Read Deuce ---------------------------------------------------------------
# 
# # load and read the files back in
# load(file = "data/compressed_top.rda")
# load(file = "data/compressed_mid.rda")
# load(file = "data/compressed_bot.rda")
# 
# # Combine Deuce ------------------------------------------------------------
# 
# # smash it all back together
# total_crime_deuce <- rbind(top_third, mid_third, bot_third)
# 
# # Check Deuce --------------------------------------------------------------
# 
# # check that the raw_crime and total_crime df's are the same
# all.equal(
#   target = total_crime_deuce, 
#   current = raw_crime)
# 
# # the two df's are equal
# 
# 
# 
# 
