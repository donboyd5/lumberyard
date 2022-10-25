

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
# devtools::install_github("hrecht/censusapi")
library(censusapi)
library(googleway)

# https://github.com/hrecht/censusapi
# https://www.hrecht.com/censusapi/


# functions ---------------------------------------------------------------
source(here::here("r", "functions_utility.r"))


# constants ---------------------------------------------------------------
GGMAP_GOOGLE_API_KEY = "AIzaSyDFWxscpciI2HDziBJ3CIhqxKWEqbaD6VM"
westmain9 <- c(43.02798717413951, -73.38200152883553)

# lefttop <- c(43.03101449699313, -73.38699043734978)
# leftbottom <- c(43.024771460431126, -73.3904236647144)
# righttop <- c(43.03088117244588, -73.37337554508187)
# bbx <- c(leftbottom, righttop)


# define a bounding box (sw and ne corners) that includes 9 west main ------
# approx bounding box from looking at osm
# https://www.openstreetmap.org/export#map=15/43.0278/-73.3782
# each point is c(lng, lat) i.e., c(x, y)
# nw <- c(43.0404, -73.4053)
# se <- c(43.0152, -73.3511)
# sw <- c(lng=43.0152, lat=-73.4053)
# ne <- c(lng=43.0404, lat=-73.3511)

# sw <- c(43.0152, -73.4053)
# ne <- c(43.0404, -73.3511)

sw <- c(43.010982, -73.409032)
ne <- c(43.041031, -73.351261)

f_bbox <- function(sw, ne){
  # they really should be named ws and en
  cnames <- c("lng", "lat")
  names(sw) <- cnames
  names(ne) <- cnames
  se <- c(ne["lng"], sw["lat"])
  nw <- c(sw["lng"], ne["lat"])
  matrix(c(sw, ne, se, nw),
         ncol=2, byrow = TRUE,
         dimnames = list(c("sw", "ne", "se", "nw"), c("lng", "lat"))) |> 
    as_tibble(rownames="label")
}
df <- f_bbox(sw, ne)
df

# define points to use for getting establishments -------------------------
# make a list of points to cycle through
nx <- 10
ny <- 10
lngseq <- seq(min(df$lng), max(df$lng), length.out=nx+2)[2:2:(nx+1)]
latseq <- seq(min(df$lat), max(df$lat), length.out=ny+2)[2:2:(ny+1)]

search_points <- expand_grid(lng = lngseq, lat = latseq) |> 
  mutate(pointnum=row_number()) |> 
  select(pointnum, lng, lat) |> 
  rowwise() |> 
  mutate(point=list(c(lng=lng, lat=lat)))

# get establishments near each point --------------------------------------

get_estabs <- function(point){
  # point is c(lng, lat)
  # we can get 3 pages for each search
  print(point)
  page1 <- google_places(location=point, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
  print(page1$status)
  Sys.sleep(2)
  page2 <- google_places(page_token=page1$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
  print(page2$status)
  Sys.sleep(2)
  page3 <- google_places(page_token=page2$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
  print(page3$status)
  f1 <- function(page){
    plist <- get(page)$results |> pull(place_id)
    tibble(placeid=plist, item=page)
  }
  places_df <- map_dfr(paste0("page", 1:3), f1)
  places_df
}


places_df <- map_dfr(search_points$point, get_estabs)
saveRDS(places_df, here::here("data", "places_df.rds"))


length(places_df$placeid)
length(unique(places_df$placeid)) # wow, 239 out of 900

uplaces <- unique(places_df$placeid)

# now get details for each place identified
f_details <- function(placeid){
  print(placeid)
  google_place_details(
    place_id=placeid,
    language = NULL,
    simplify = TRUE,
    curl_proxy = NULL,
    key = GGMAP_GOOGLE_API_KEY)
}

placelist <- purrr::map(uplaces, f_details)
saveRDS(placelist, here::here("data", "placelist.rds"))


placelist <- readRDS(here::here("data", "placelist.rds"))

f_extract_details <- function(i){
  p <- placelist[[i]]$result
  gl <- p$geometry$location
  tibble(placeid=p$place_id,
         name=p$name,
         lat=gl$lat, 
         lng=gl$lng,
         address=p$formatted_address,
         phone=p$formatted_phone_number,
         status=p$business_status,
         vicinity=p$vicinity,
         website=p$website,
         nratings=p$user_ratings_total,
         rating=p$rating,
         types=list(p$types),
         reviews=list(p$reviews))
}
f_extract_details(1)

place_details <- map_dfr(1:length(placelist), f_extract_details)
saveRDS(place_details, here::here("data", "place_details.rds"))

glimpse(place_details)


tmp <- place_details |> arrange(lng)


# get establishments and save as xlsx -------------------------------------


placedf <- readRDS(here::here("data", "place_details.rds"))
writexl::write_xlsx(placedf, here::here("nyforward", "results", "ggmaps_establishments.xlsx"))


plong <- placedf |> 
  unnest(cols = c(types))
ptypes <- count(plong, types)
ptypes |> 
  arrange(desc(n))

placedf |> 
  filter(str_detect(types, "health"))

type <- "store"
plong |> 
  mutate(
    local({ids <- plong |> filter(types==type) |> pull(placeid)
    tibble(ids=list(ids))})) |> 
  filter(placeid %in% unlist(ids), !types %in% c(type, "establishment", "point_of_interest")) |> 
  select(placeid, name, address, types) |> 
  count(types, sort=TRUE)

plong |> 
  group_by(placeid) |> 
  mutate(n=n()) |> 
  ungroup() |> 
  filter(n==2) |> 
  count(types)

utypes <- plong |> 
  group_by(placeid) |> 
  mutate(n=n()) |> 
  filter(types!="point_of_interest") |> 
  filter(!(n > 2 & type=="establishment")) |> 
  ungroup() |> 
  filter(status=="OPERATIONAL")

utypes |> count(types, sort = TRUE)

utypes |> filter(types=="car_repair") |> 
  select(placeid, name, address, types)


plong |> 
  group_by(placeid) |> 
  summarise(n=n()) |> 
  count(n)

plong |> 
  filter(types=="health") |> 
  select(placeid, name, address, types)

# selected ids and primary type (my judgment)
# food is one type
id <- "ChIJEangFa-c4IkRfmrH8ADUseI" # dollar general; store
id <- "ChIJ1yRvUkuD4IkRh7xBNWWH3h0" # Kings; bakery
id <- "ChIJw-8TRsGD4IkRwf5KqL2r-1w" # West End Deli Works; food

# health is one type
id <- "ChIJBapZ-hqD4IkRRP6IpYY-5ws" # Lakota's NO OTHER TYPE
id <-  "ChIJ___PkVSD4IkRROpFIZ4kVHM" #  Jason Goodspeed; pharmacy (he's the Walgreens pharmacist) 
id <- "ChIJofEUZrac4IkRIh7ginzjuLw" # Turning Point; 


plong |> 
  filter(placeid==id) |> 
  select(placeid, name, address, types)




df <- tibble(a=1:5, b=5:1)



df  |> 
  mutate(local({
    n <- nrow(cur_data())
    tibble(nvals=n)
  }))



plong |> 
  {
  filter(types=="store")
  } |> 
  select(placeid, types, name, address)

plong %>%
  {
    n = nrow(.)
    filter(., types=="store") |> 
      mutate(nvals=n)
  } %>% 
  select(placeid, types, name, address, nvals)



{ n = nrow(.) 
  gather(., var, value, -Grp) %>%
    mutate(newval = value * n) } 

plong2 <- plong
  







# old below here ----




bbx1 <- osmdata::getbb("cambridge ny")
# min       max
# x -73.39920 -73.36409
# y  43.01776  43.03894
bbx1
class(bbx1)
dim(bbx1)

(diffs <- matrix(c(-.01, .01,
                   -.01, .01), 
                 nrow=2, byrow=TRUE))
bbx2 <- bbx1 + diffs

(diffs <- matrix(c(-.01, .01,
                   -.01, .01), 
                 nrow=2, byrow=TRUE))

(diffbase <- matrix(c(-1, 1,
                   -1, 1), 
                 nrow=2, byrow=TRUE))

bbx2 <- bbx1 + diffbase * .0005


bbx1
diff <- .005
(bbx3 <- matrix(c(t(westmain9) - diff, t(westmain9) + diff), nrow=2))
          

westmain9 - diff

# check a bounding box ----

cmap <- get_map(bbx2, zoom = 14, maptype = "toner") # roadmap toner-background
ggmap(cmap)


## Notes from Google: ----

# https://developers.google.com/maps/documentation/places/web-service/policies

# https://developers.google.com/maps/documentation/javascript/places#overview
# https://developers.google.com/maps/documentation/javascript/places#place_searches
# https://developers.google.com/maps/documentation/javascript/supported_types
# https://developers.google.com/maps/documentation/javascript/reference/coordinates#LatLngBounds

# When you first load the API, you are allocated an initial quota of requests.
# Once you use this quota, the API enforces rate limits on additional requests
# on a per-second basis. If too many requests are made within a certain time
# period, the API returns an OVER_QUERY_LIMIT response code. The per-session
# rate limit prevents the use of client-side services for batch requests. For
# batch requests, use our web service APIs.


# places (defined in this API as establishments, geographic locations, or prominent points of interest)
# Nearby Search returns a list of nearby places based on a user's location.
# Text Search returns a list of nearby places based on a search string, eg. "Pizza".
# Place Details requests return more detailed information about a specific place, including user reviews.

# The information returned can include establishments — such as restaurants,
# stores, and offices — as well as 'geocode' results, which indicate addresses,
# political areas such as towns and cities, and other points of interest. ##

# Constructs a rectangle from the points at its south-west and north-east corners.

# Nearby Search ----

# A Nearby Search lets you search for places within a specified area by keyword
# or type. A Nearby Search must always include a location, which can be
# specified in one of two ways:
#
# a LatLngBounds. a circular area defined as the combination of the location
# property — specifying the center of the circle as a LatLng object — and a
# radius, measured in meters.


# https://developers.google.com/maps/documentation/javascript/places#PlaceSearchPaging

# each place search returns up to 20 results per query. However, each search can return as many as 60 results, split across three pages. Additional pages are available via the PlaceSearchPagination object. In order to access additional pages you must capture the PlaceSearchPagination object via a callback function. The PlaceSearchPagination object is defined as:
#   
#   hasNextPage a boolean property that indicates if further results are available. true when there is an additional results page.
# nextPage() a function that will return the next set of results. After executing a search, you must wait two seconds before the next page of results will be available.
# To see the next set of results, call nextPage


library(googleway)
# use OSM to help pick bounds or points for the searches
# https://www.openstreetmap.org/export#map=15/43.0290/-73.3834

westmain9 <- c(43.02798717413951, -73.38200152883553)
# get places near 9 west main - but won't be able to get details with this kind of query
item1 <- google_places(location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
item1$status
item2 <- google_places(page_token=item1$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
item2$status
item3 <- google_places(page_token=item2$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
item3$status
item4 <- google_places(page_token=item3$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
item4$status
item5 <- google_places(page_token=item4$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
item5$status
item6 <- google_places(page_token=item5$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
item6$status
item7 <- google_places(page_token=item6$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
item7$status
item8 <- google_places(page_token=item7$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
item8$status
item9 <- google_places(page_token=item8$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
item9$status
item10 <- google_places(page_token=item9$next_page_token, location=westmain9, rankby = "distance", place_type="establishment", key=GGMAP_GOOGLE_API_KEY)
item10$status


ilist <- paste0("item", 1:10)

f1 <- function(itemname){
  print(itemname)
  plist <- get(itemname)$results |> pull(place_id)
  tibble(placeid=plist, item=itemname)
}
# f1(ilist[2])
places_df <- map_dfr(ilist, f1)
duplicated(places_df$placeid) # duplicated after 60th place (item 3)

# now get details for each place identified
f <- function(placeid){
  print(placeid)
  google_place_details(
    place_id=placeid,
    language = NULL,
    simplify = TRUE,
    curl_proxy = NULL,
    key = GGMAP_GOOGLE_API_KEY)
}

placelist <- purrr::map(places_df$placeid[1:60], f)
saveRDS(placelist, here::here("data", "placelist.rds"))

str(placelist[[1]]$result)
class(placelist[[1]]$result)

placelist[[1]]$result

f2 <- function(i){
  p <- placelist[[i]]$result
  gl <- p$geometry$location
  tibble(placeid=p$place_id,
         name=p$name,
         lat=gl$lat, 
         lng=gl$lng,
         address=p$formatted_address,
         phone=p$formatted_phone_number,
         status=p$business_status,
         vicinity=p$vicinity,
         website=p$website,
         nratings=p$user_ratings_total,
         rating=p$rating,
         types=list(p$types),
         reviews=list(p$reviews))
}
f2(1)

df <- map_dfr(1:60, f2)
sort(df$name)

df |> filter(str_detect(name, "BellaVino"))

tmp <- df |> arrange(lng)
count(tmp, status)


# only can get 60; maybe issue 4 separate calls at 4 points on the perimeter of the bounding box (middle of each segment)

names(placelist)
i <- 5
# names(placelist[[i]]$result)
# placelist[[i]]$result
placelist[[i]]$result$place_id
placelist[[i]]$result$name
placelist[[i]]$result$international_phone_number
placelist[[i]]$result$business_status
placelist[[i]]$result$formatted_address
placelist[[i]]$result$types
placelist[[i]]$result$user_ratings_total
placelist[[i]]$result$rating
placelist[[i]]$result$website


set.seed(123)
library(zoo)
(A <- cumsum(rnorm(200)))
(Date <- as.Date("2021-06-29") + cumsum(sample(5, 200, replace = TRUE)))
data <- data.frame(Date, A) %>% mutate(Year_Month = as.yearmon(Date))
data

# returns vector of indexes to prior 12 months
# ix is row number (scalar); ym is yearmon vector.
indexes <- function(ix, ym, x = ym[ix]) which(ym >= x-1 & ym <= x - 1/12) 
indexes

data %>%
  mutate(local({
    indexList <- lapply(1:nrow(cur_data()), indexes, .$Year_Month)
    data.frame(B = sapply(indexList, function(x) mean(A[x])), n = lengths(indexList))
  }))

data  |> 
  mutate(local({
    n <- 5
    tibble(nvals=n)
  }))



library(pipeR)
library(dplyr)
library(tidyr)
df %>>% 
  (~ n  = nrow(.)) %>% 
  gather(., var, value, -Grp) %>%
  mutate(newval = value * n)


df <- tibble(a=1:5, b=5:1)

df  |> 
  mutate(local({
    n <- nrow(cur_data())
    tibble(nvals=n)
  }))

df  |> 
  mutate(local({n <- 7}))
  mutate(nvals=n)





