
# https://thatdatatho.com/web-scraping-indeed-jobs-r-rvest/

library(tidyverse)
library(rvest)
library(xml2)

url <- "https://www.indeed.ca/Data-Scientist-jobs-in-Vancouver%2C+BC"
url <- "https://www.indeed.com/jobs?q&l=12816&sort=date&vjk=d1ce741b378aaeff"
page <- xml2::read_html(url)

str(page)
page$node
page$doc

# get company location
page %>% 
  rvest::html_nodes("span") %>% 
  rvest::html_nodes(xpath = '//*[@class="location"]')%>% 
  rvest::html_text() %>%
  stringi::stri_trim_both()

# get company name
page %>% 
  rvest::html_nodes("span")  %>% 
  rvest::html_nodes(xpath = '//*[@class="company"]')  %>% 
  rvest::html_text() %>%
  stringi::stri_trim_both()


# install.packages('googleway')
# https://developers.google.com/maps/documentation/places/web-service/supported_types
library(googleway)
tmp <- google_places(search_string = 'mexican food', location=c(35.77936902425637, -78.63995745574967), radius=3218, key=GGMAP_GOOGLE_API_KEY)
str(tmp)
names(tmp)
as_tibble(tmp)

tmp2 <- google_places(search_string = 'restaurant', location=c(43.027846,-73.3841795), radius=10000, key=GGMAP_GOOGLE_API_KEY)
names(tmp2)

tmp2 <- google_places(location=c(43.027846,-73.3841795), rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
tmp2 <- google_places(search_string = 'establishment', location=c(43.027846,-73.3841795), radius = 1000, key=GGMAP_GOOGLE_API_KEY)
tmp2 <- google_places(location=c(43.027846,-73.3841795), radius = 1000, key=GGMAP_GOOGLE_API_KEY)
# we don't get formatted_address unless we use a search_string, I think


tmp2 <- google_places(location=c(43.027846,-73.3841795), radius = 1000, place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
names(tmp2)

df <- as_tibble(tmp2$results)
glimpse(df)
df$name
df |> select(name, formatted_address)


# types
# accounting
# airport
# amusement_park
# aquarium
# art_gallery
# atm
# bakery
# bank
# bar
# beauty_salon
# bicycle_store
# book_store
# bowling_alley
# bus_station
# cafe
# campground
# car_dealer
# car_rental
# car_repair
# car_wash
# casino
# cemetery
# church
# city_hall
# clothing_store
# convenience_store
# courthouse
# dentist
# department_store
# doctor
# drugstore
# electrician
# electronics_store
# embassy
# fire_station
# florist
# funeral_home
# furniture_store
# gas_station
# gym
# hair_care
# hardware_store
# hindu_temple
# home_goods_store
# hospital
# insurance_agency
# jewelry_store
# laundry
# lawyer
# library
# light_rail_station
# liquor_store
# local_government_office
# locksmith
# lodging
# meal_delivery
# meal_takeaway
# mosque
# movie_rental
# movie_theater
# moving_company
# museum
# night_club
# painter
# park
# parking
# pet_store
# pharmacy
# physiotherapist
# plumber
# police
# post_office
# primary_school
# real_estate_agency
# restaurant
# roofing_contractor
# rv_park
# school
# secondary_school
# shoe_store
# shopping_mall
# spa
# stadium
# storage
# store
# subway_station
# supermarket
# synagogue
# taxi_stand
# tourist_attraction
# train_station
# transit_station
# travel_agency
# university
# veterinary_care
# zoo


library(OpenStreetMap)
library(osmdata)

