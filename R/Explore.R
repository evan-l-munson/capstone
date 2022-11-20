
# data is from the Chicago Data Portal
#  https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2

# Libraries ---------------------------------------------------------------

library(janitor)
library(tictoc)
library(dplyr)
library(lubridate)
# library(dtplyr)
library(ggplot2)
library(formattable)
library(leaflet)
library(leaflet.extras)

# Explore -----------------------------------------------------------------

# # combine the 3 compressed files
# raw_crime <- rbind(top_third, mid_third, bot_third)
# 
# num_split <- 100000
# 
# # reduce the dataframe for easier manipulation
# small_crime <- raw_crime[1:num_split] %>% 
#   janitor::clean_names()

# cleanup data and configure the columns for further analysis
prep_crime <- raw_crime %>% 
  janitor::clean_names() %>%
  dplyr::mutate(
    date = lubridate::as_datetime(lubridate::mdy_hms(x = date)),
    updated_on = lubridate::as_datetime(
      x = lubridate::mdy_hms(x = updated_on))) %>% 
  tibble::as_tibble()

# Total Crime -------------------------------------------------------------

# total number of crimes
explore_crime_chart <- prep_crime %>% 
  dplyr::group_by(primary_type) %>% 
  dplyr::count() %>% 
  tibble::as_tibble() %>% 
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(primary_type, n),
    y = n)) +
  ggplot2::geom_bar(stat = "identity") + 
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Type of Crime Commited in Chicago",
    subtitle = paste0(
      "Total number of crimes: ", 
      formatC(x =  nrow(raw_crime), 
              format = "f", 
              big.mark = ",", 
              digits=0)),
    x = "Crime Type",
    y = "Number of Crimes")

# generalized location of crime
location_crime_chart <- prep_crime %>% 
  dplyr::group_by(location_description) %>% 
  dplyr::count() %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(n)) %>%
  dplyr::slice_max(
    order_by = n,
    n = 20) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(location_description, n), 
    y = n)) +
  ggplot2::geom_bar(stat = "identity") + 
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::labs(
    title = "Where Crimes occured in Chicago",
    subtitle = paste0(
      "Total number of crimes: ", 
      formatC(x =  nrow(raw_crime), 
              format = "f", 
              big.mark = ",", 
              digits=0)),
    x = "Crime Location",
    y = "Number of Crimes")

# Crime by year -----------------------------------------------------------

# all crime events
all_crime_chart <- prep_crime %>% 
  dplyr::mutate(
    date_less_time = lubridate::as_date(date)) %>% 
  dplyr::group_by(date_less_time) %>% 
  dplyr::count() %>% 
  dplyr::mutate(
    month = lubridate::year(date_less_time)) %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      x = date_less_time, 
      y = n)) +
  ggplot2::geom_line()+
  ggplot2::geom_smooth(
    color = "red") +
  ggplot2::labs(
    title = "Chicago Crime by Year",
    x = "Year",
    y = "Crime events") +
  ggplot2::theme_bw()

# total crime displayed in a facet wrap 
multi_chart_chart <- prep_crime %>% 
  dplyr::mutate(
    date_less_time = lubridate::as_date(date)) %>% 
  dplyr::group_by(date_less_time) %>% 
  dplyr::count() %>% 
  dplyr::mutate(
    month = lubridate::year(date_less_time)) %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        x = date_less_time, 
        y = n)) +
  ggplot2::geom_line()+
  ggplot2::geom_smooth(
    color = "red") +
  ggplot2::facet_wrap(
    facets = "month", 
    scales = "free_x") +
  ggplot2::labs(
    title = "Chicago Crime by Year",
    x = "Month",
    y = "Crime events") +
  ggplot2::theme_bw()


# Monthly Crime Spikes ----------------------------------------------------

# the all_crime chart seems to show large cyclic spikes at regular intervals
#   furthermore the multi_chart also shows a large spike at the begining of each 
#   year also.
# what crimes cause these spikes generally at the beginning of the year?
#   are these new years eve/day crimes?
#   are they generally a specific kind of crime?

# start by looking at the spikes at the beginning of the year
#   pull out the date of the max crimes for year
spikes_max <- prep_crime %>% 
  dplyr::mutate(
    date_less_time = lubridate::as_date(date)) %>% 
  dplyr::group_by(date_less_time) %>% 
  dplyr::count() %>% 
  dplyr::mutate(
    year = lubridate::year(date_less_time),
    month = lubridate::month(date_less_time, label = TRUE)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::slice(which.max(n))

# for curiosity stake, when were the min crime days?
spikes_min <- prep_crime %>% 
  dplyr::mutate(
    date_less_time = lubridate::as_date(date)) %>% 
  dplyr::group_by(date_less_time) %>% 
  dplyr::count() %>% 
  dplyr::mutate(
    year = lubridate::year(date_less_time),
    month = lubridate::month(date_less_time, label = TRUE)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::slice(which.min(n))

# combine the all crime chart with the spike locations
all_crime_spikes_chart <- prep_crime %>% 
  dplyr::mutate(
    date_less_time = lubridate::as_date(date)) %>% 
  dplyr::group_by(date_less_time) %>% 
  dplyr::count() %>% 
  dplyr::mutate(
    month = lubridate::year(date_less_time)) %>% 
  ggplot2::ggplot(
    ggplot2::aes(
      x = date_less_time, 
      y = n)) +
  ggplot2::geom_line(
    ggplot2::aes(
      color = "Crimes"),) +
  ggplot2::geom_smooth(
    ggplot2::aes(
      color = "Trend")) +
  ggplot2::geom_point(
    data = spikes_max,
    ggplot2::aes(
      x = spikes_max$date_less_time, 
      y = spikes_max$n, 
      color = "Highest Yearly Day"), 
    size = 2,
    shape = 4) +
  ggplot2::geom_point(
    data = spikes_min,
    ggplot2::aes(
      x = spikes_min$date_less_time, 
      y = spikes_min$n, 
      color = "Lowest Yearly Day"), 
    size = 2,
    shape = 3) +
  ggplot2::labs(
    title = "Chicago Crime by Year",
    x = "Year",
    y = "Crime Events") +
  ggplot2::scale_color_manual("",
    values = c("darkgrey", "red", "blue", "black")) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom") 

# which month per year had the most criminal events?
spikes_max_chart <- spikes_max %>% 
  dplyr::group_by(month) %>% 
  dplyr::count() %>% 
  ggplot2::ggplot(ggplot2::aes(x = reorder(month, n), y = n, label = n)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_bw() +
  ggplot2::geom_text(nudge_y = 0.4) + 
  ggplot2::labs(
    title = "Month per year with the highest number of Criminal Events",
    subtitle = paste(
      "Times per year, a month had the highest number of Criminal Events"),
    x = "Month",
    y = "# of Times") +
  ggplot2::coord_flip()

# which month per year had the least criminal events?
spikes_min_chart <- spikes_min %>% 
  dplyr::group_by(month) %>% 
  dplyr::count() %>% 
  ggplot2::ggplot(ggplot2::aes(x = reorder(month, n), y = n, label = n)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_bw() +
  ggplot2::geom_text(nudge_y = 0.4) + 
  ggplot2::labs(
    title = "Month per year with the least number of Criminal Events",
    subtitle = paste(
      "Times per year, a month had the lowest number of Criminal Events"),
    x = "Month",
    y = "# of Times") +
  ggplot2::coord_flip()

# now that we know in which month and specifically which day the largest number
#   of criminal events are happening, now lets investigate what criminal events
#   are happening.
max_crimes_chart <- prep_crime %>% 
  dplyr::mutate(
    date_less_time = lubridate::as_date(date)) %>% 
  dplyr::filter(date_less_time %in% spikes_max$date_less_time) %>% 
  dplyr::group_by(year, primary_type) %>% 
  dplyr::count() %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = n, fill = primary_type)) +
  ggplot2::geom_bar(
    position = "stack", 
    stat = "identity") +
  ggplot2::theme_bw() +
  ggplot2::scale_x_continuous(n.breaks = 22) +
  ggplot2::labs(
    title = "Types of Crime on the Yearly Peak Day",
    x = "Year",
    y = "Number of Crimes") +
  ggplot2::theme(
    legend.position = "bottom") 

# the above chart is hard to interpret due to the amount of criminal events
#   reduce this chart to show only the top 5 events that happen in a given year
max_crimes_top5_chart <- prep_crime %>% 
  dplyr::mutate(
    date_less_time = lubridate::as_date(date)) %>% 
  dplyr::filter(date_less_time %in% spikes_max$date_less_time) %>% 
  dplyr::group_by(year, primary_type) %>% 
  dplyr::count() %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::slice(1:5) %>% 
  ggplot2::ggplot(ggplot2::aes(x = year, y = n, fill = primary_type)) +
  ggplot2::geom_bar(
    position = "stack", 
    stat = "identity") +
  ggplot2::theme_bw() +
  ggplot2::scale_x_continuous(n.breaks = 22) +
  ggplot2::labs(
    title = "Top Five Types of Crime on the Yearly Peak Day",
    x = "Year",
    y = "Number of Crimes") +
ggplot2::theme(
  legend.position = "bottom") 


# Heatmap -----------------------------------------------------------------

# display where all the crime events happened on the worst day in 2003
worst <- spikes_max %>% 
  dplyr::filter(n == max(spikes_max$n)) %>% 
  # dplyr::mutate(date_less_time = lubridate::as_date(date_less_time)) %>% 
  dplyr::pull(date_less_time)


# worst_day <- prep_crime %>%
#   dplyr::mutate(
#     date_less_time = lubridate::as_date(date)) %>%
#   dplyr::filter(date_less_time == worst) %>%
#   leaflet::leaflet() %>%
#   leaflet::addTiles() %>%
#   leaflet::setView(
#     lng = "-87.71665",
#     lat = "41.93741",
#     zoom = 9) %>%
#     # leaflet::addProviderTiles(providers$Stamen.Toner) %>%
#     # leaflet.extras::addHeatmap(
#     #   lng = prep_crime$longitude,
#     #   lat = prep_crime$latitude)
# # leaflet.extras::addHeatmap(
# #   lng = ~longitude,
# #   lat = ~latitude)
#     leaflet::addCircles(
#       lng = prep_crime$longitude,
#       lat = prep_crime$latitude,
#       popup = prep_crime$primary_type)




# head_crime <- head(prep_crime) %>% 
#   tibble::as_tibble()
# 
# # whole lot of circles and locations
# location_crime_plot <- prep_crime %>% 
#   dplyr::as_tibble() %>%
#   # dplyr::filter(!is.na(latitude) &
#   #                 !is.na(longitude) &
#   #                 !is.null(latitude) &
#   #                 !is.null(longitude)) %>% 
#   # dplyr::filter_at(
#   #   dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.)))
#   leaflet::leaflet() %>% 
#   leaflet::addTiles() %>% 
#   leaflet::setView(
#     lng = "-87.71665", 
#     lat = "41.93741", 
#     zoom = 9) %>% 
#   # leaflet::addProviderTiles(providers$Stamen.Toner) %>% 
#   addCircles(
#     lng = prep_crime$longitude, 
#     lat = prep_crime$latitude, 
#     popup = prep_crime$primary_type)
# 
# 
# # small selection
# head_crime %>% 
#   dplyr::filter_at(
#     dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.))) %>%
#   leaflet::leaflet() %>% 
#   leaflet::leaflet() %>% 
#     leaflet::addTiles() %>% 
#     leaflet::setView(
#       lng = "-87.71665", 
#       lat = "41.93741", 
#       zoom = 9) %>% 
#     # leaflet::addProviderTiles(providers$Stamen.Toner) %>% 
#     leaflet::addCircles(
#       lng = head_crime$longitude, 
#       lat = head_crime$latitude, 
#       popup = head_crime$primary_type)
# 
# # bigger selection
# bigger <- prep_crime %>% 
#   tibble::as_tibble() %>% 
#   dplyr::top_frac(0.001)
# 
# bigger %>% 
#   dplyr::filter_at(
#     dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.))) %>%
#   leaflet::leaflet() %>% 
#   leaflet::leaflet() %>% 
#   leaflet::addTiles() %>% 
#   leaflet::setView(
#     lng = "-87.71665", 
#     lat = "41.93741", 
#     zoom = 9) %>% 
#   # leaflet::addProviderTiles(providers$Stamen.Toner) %>% 
#   leaflet::addCircles(
#     lng = bigger$longitude, 
#     lat = bigger$latitude, 
#     popup = bigger$primary_type)
#   
# # bigger selection heatmap
# bigger %>% 
#   dplyr::filter_at(
#     dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.))) %>% 
#   leaflet::leaflet() %>% 
#   leaflet::addTiles() %>% 
#   leaflet::setView(
#     lng = "-87.71665", 
#     lat = "41.93741", 
#     zoom = 9) %>% 
#   leaflet.extras::addHeatmap(
#     lng = bigger$longitude, 
#     lat = bigger$latitude,
#     blur = 1, 
#     max = 1, 
#     radius = 5)
# 
# # all selection heatmap
# prep_crime %>% 
#   dplyr::filter_at(
#     dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.))) %>% 
#   leaflet::leaflet() %>% 
#   leaflet::addTiles() %>% 
#   leaflet::setView(
#     lng = "-87.71665", 
#     lat = "41.93741", 
#     zoom = 9) %>% 
#   leaflet.extras::addHeatmap(
#     lng = prep_crime$longitude, 
#     lat = prep_crime$latitude,
#     blur = 1, 
#     max = 1, 
#     radius = 5)
# 
# 
# 
# 
# xx <- head_crime %>% 
#   dplyr::filter_at(
#     dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.)))
# 
# xxx <- 5
# 
# validateCoords(lng = xx$longitude[xxx], lat = xx$latitude[xxx])
# 
# 
# 
# 
# num_na <- prep_crime %>% 
#   group_by(primary_type) %>% 
#   count() %>% 
#   as_tibble()









