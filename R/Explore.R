

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

num_split <- 100000

# reduce the dataframe for easier manipulation
small_crime <- raw_crime[1:num_split]

# cleanup data and configure the columns for further analysis
prep_crime <- raw_crime %>% 
  janitor::clean_names() %>%
  dplyr::mutate(
    date = lubridate::as_datetime(lubridate::mdy_hms(x = date)),
    updated_on = lubridate::as_datetime(lubridate::mdy_hms(x = updated_on)),
    add = latitude + longitude)

# total number of crimes
explore_crime <- prep_crime %>% 
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
location_crime <- prep_crime %>% 
  dplyr::group_by(location_description) %>% 
  dplyr::count() %>% 
  dplyr::slice_max(
    order_by = n, 
    n = 20) %>% 
  tibble::as_tibble() %>% 
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


# Heatmap -----------------------------------------------------------------

head_crime <- head(prep_crime) %>% 
  tibble::as_tibble()

# whole lot of circles and locations
location_crime_plot <- prep_crime %>% 
  dplyr::as_tibble() %>%
  # dplyr::filter(!is.na(latitude) &
  #                 !is.na(longitude) &
  #                 !is.null(latitude) &
  #                 !is.null(longitude)) %>% 
  # dplyr::filter_at(
  #   dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.)))
  leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::setView(
    lng = "-87.71665", 
    lat = "41.93741", 
    zoom = 9) %>% 
  # leaflet::addProviderTiles(providers$Stamen.Toner) %>% 
  addCircles(
    lng = prep_crime$longitude, 
    lat = prep_crime$latitude, 
    popup = prep_crime$primary_type)


# small selection
head_crime %>% 
  dplyr::filter_at(
    dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.))) %>%
  leaflet::leaflet() %>% 
  leaflet::leaflet() %>% 
    leaflet::addTiles() %>% 
    leaflet::setView(
      lng = "-87.71665", 
      lat = "41.93741", 
      zoom = 9) %>% 
    # leaflet::addProviderTiles(providers$Stamen.Toner) %>% 
    leaflet::addCircles(
      lng = head_crime$longitude, 
      lat = head_crime$latitude, 
      popup = head_crime$primary_type)

# bigger selection
bigger <- prep_crime %>% 
  tibble::as_tibble() %>% 
  dplyr::top_frac(0.001)

bigger %>% 
  dplyr::filter_at(
    dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.))) %>%
  leaflet::leaflet() %>% 
  leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::setView(
    lng = "-87.71665", 
    lat = "41.93741", 
    zoom = 9) %>% 
  # leaflet::addProviderTiles(providers$Stamen.Toner) %>% 
  leaflet::addCircles(
    lng = bigger$longitude, 
    lat = bigger$latitude, 
    popup = bigger$primary_type)
  
# bigger selection heatmap
bigger %>% 
  dplyr::filter_at(
    dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.))) %>% 
  leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::setView(
    lng = "-87.71665", 
    lat = "41.93741", 
    zoom = 9) %>% 
  leaflet.extras::addHeatmap(
    lng = bigger$longitude, 
    lat = bigger$latitude,
    blur = 1, 
    max = 1, 
    radius = 5)

# all selection heatmap
prep_crime %>% 
  dplyr::filter_at(
    dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.))) %>% 
  leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::setView(
    lng = "-87.71665", 
    lat = "41.93741", 
    zoom = 9) %>% 
  leaflet.extras::addHeatmap(
    lng = prep_crime$longitude, 
    lat = prep_crime$latitude,
    blur = 1, 
    max = 1, 
    radius = 5)




xx <- head_crime %>% 
  dplyr::filter_at(
    dplyr::vars(latitude, longitude), dplyr::all_vars(!is.na(.)))

xxx <- 5

validateCoords(lng = xx$longitude[xxx], lat = xx$latitude[xxx])




num_na <- prep_crime %>% 
  group_by(primary_type) %>% 
  count() %>% 
  as_tibble()









