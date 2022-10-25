
# bbx should be wsen

# library(ggmap)
library(osmdata)

# https://rspatialdata.github.io/osm.html

# remotes::install_github('ropensci/osmdata')
# Unlike the OpenStreetMap package, which facilitates the download of raster
# tiles, osmdata provides access to the vector data underlying OSM.

# overpass query lbrtbb
westmain9 <- c(43.02798717413951, -73.38200152883553)
lefttop <- c(43.03101449699313, -73.38699043734978)
leftbottom <- c(43.024771460431126, -73.3904236647144)
righttop <- c(43.03088117244588, -73.37337554508187)
bbx <- c(leftbottom, righttop)

# lat, lon, lat, lon
# lb, 

# q <- opq(bbox = c(51.1, 0.1, 51.2, 0.2))
q1 <- opq(bbox = bbx)
q <- q1 |> 
  add_osm_feature(key = 'highway', value = 'motorway')
head(available_features())
feat <- available_features()
feat
tags <- available_tags(feature="denomination")
tags
str_subset(feat, "name")
available_tags(feature="shop")
available_tags(feature="shop")

f <- function(feature){
  print(feature)
  tags <- available_tags(feature)
  df <- tibble(tag=tags, feature=feature)
  df
}
# f("shop")
featags <- map_dfr(feat, f)
featags <- featags |> select(feature, tag=tags)
saveRDS(featags, here::here("data", "osm_tags.rds"))

featags |> filter(feature=="shop")


q <- opq ("portsmouth usa") %>%
  add_osm_feature (
    key = "amenity",
    value = "restaurant"
  )

x <- q |> osmdata_sf()
x
x$osm_points
x$osm_polygons

psmouth <- opq ("portsmouth usa") %>%
  add_osm_feature (key = "amenity") |> 
  osmdata_sf()
psmouth




# approx bounding box
# https://www.openstreetmap.org/export#map=15/43.0278/-73.3782
ul <- c(43.0404, -73.4053)
ur <- c(43.0404, -73.3511)
ll <- c(43.0152, -73.4053)
lr <- c(43.0152, -73.3511)
bbx <- c(ll, ur)
bbx

# https://rstudio-pubs-static.s3.amazonaws.com/788558_f89a58ebb83c4c529cae55ed2026fdc5.html
library(osmplotr)

bbx <- getbb("portsmouth nh")
bbx <- getbb("cambridge ny")
dat_B <- extract_osm_objects(key = "building", bbox = bbx)
dat_B



bbx <- "cambridge, ny"
westmain9 <- c(43.02798717413951, -73.38200152883553)
lefttop <- c(43.03101449699313, -73.38699043734978)
leftbottom <- c(43.024771460431126, -73.3904236647144)
righttop <- c(43.03088117244588, -73.37337554508187)
diff <- .02
bbx <- c(westmain9 -diff, westmain9 + diff)
bbx
# 43.0135509,-70.8229994,43.0996118,-70.7279298 # portsmouth lb, rt
count(featags, feature)
featags |> filter(feature=="boundary")
bbx <- getbb("cambridge ny")
camb1 <- opq(bbox = bbx) |> 
  # add_osm_feature (key = "boundary", value="administrative") |>
  add_osm_feature (key = "building") |> 
  osmdata_sf ()
camb1

library(sf)
df <- camb1 |> 
  st_drop_geometry()

camb1$osm_polygons$name
str(tmp)
tmp$name
camb1$osm_points$geometry

# bbx should be wsen (swne)
 
library(ggmap)
cmap <- get_map(getbb("Cambridge ny"), maptype = "toner-background")
ggmap(cmap, extent = "normal")
ggmap(cmap, extent = "device")

hdf <- get_map("houston, texas")
ggmap(hdf, extent = "normal")

ggplot() +
  geom_sf(data = lagos_hospitals$osm_polygons)

ggplot() + geom_sf(aes(x))


lonbbx <- c(51.1, 0.1, 51.2, 0.2)
london <- opq(bbox = lonbbx) %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_sf ()

opq(bbox = 'greater london uk') %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_sf ()

# https://github.com/dkahle/ggmap/issues/117
mpa1 <- get_openstreetmap(bbox=bkill_bb)



centers <- c(mean(bkbb$left, bkbb$right), mean(bkbb$top, bkbb$bottom))
# # -73.57  43.24
# centers <- c(-73.5, 43.2)
#   
(batt_smap <- get_map(location=lbrtbb,
                      source="stamen",
                      force=TRUE,
                      zoom=10,  # default 10, must be integer
                      maptype="toner-lines"))

diffs <- c(-.01, -.01, .01, .01)
(batt_smap <- get_stamenmap(bbox =lbrtbb + diffs,
                            force=TRUE,
                            zoom=10,  # default 10, must be integer
                            # color="color",
                            maptype="toner-background"))


ggmap(batt_smap)
str(batt_smap)

batt_unk <- get_map(bbox=lbrtbb + diffs, maptype="toner-background", force=TRUE)
str(batt_unk)
ggmap(batt_unk)

# c(xmin, ymin, xmax, ymax) 
q <- opq(bbox = lbrtbb + diffs) %>%
  add_osm_feature(key = 'natural', value = 'water')

(batt_gmap <- get_googlemap(center=centers + c(.375, 0),
                            zoom=10,  # default 10, must be integer
                            maptype="roadmap"))

ggmap(batt_gmap)
ggmap(batt_smap)

base <- batt_smap
base <- batt_gmap
base <- batt_unk
class(base) # ggmap, raster
str(base)

ggmap(base, extent = "panel") +
  geom_point(aes(lon, lat, size=hbi), 
             data=bkill %>% dplyr::select(lat=latitude, lon=longitude, hbi),
             colour="blue",
             position = position_jitter(seed = 2, width=.0035, height=.0035)
  )



bkill

# batt_map <- get_map(location=lbrtbb, 
#                     source="google",
#                     force=TRUE,
#                     crop=TRUE,
#                     maptype="roadmap",
#                     scale=3,
#                     zoom="auto")

# batt_map <- get_map(location=lbrtbb, 
#                     source="stamen",
#                     force=TRUE,
#                     zoom=10)

#.. get special features ----
available_tags("water")  # river, stream_pool (?)
q1 <- lbrtbb %>%
  opq() %>%
  add_osm_feature("water", "river")
str(q1)

q2 <- lbrtbb %>%
  opq() %>%
  add_osm_feature("water", "stream_pool")
str(q2)


battwater <- osmdata_sf(q1)
battpools <- osmdata_sf(q2)  # nothing

#.. display with special features ----
batt_map %>%
  ggmap() +
  geom_sf(data = battwater$osm_polygons,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 4,
          shape = 21) 


# (map <- get_googlemap(c(-97.14667, 31.5493)))
# ggmap(map)
# 
# centers <- c(mean(bkbb$left, bkbb$right), mean(bkbb$top, bkbb$bottom))
# # -73.57  43.24
# centers <- c(-73.5, 43.2)
#   
# (map <- get_googlemap(center=centers,
#                       zoom=10,  # default 10, must be integer
#                       maptyp="terrain"))
# ggmap(map)

# (map <- get_googlemap(center=centers,
#                       zoom=10,  # default 10, must be integer
#                       maptyp="terrain"))
# ggmap(map)


# looks like I shouldn't use OSM ----
# https://github.com/dkahle/ggmap/issues/117
battmap_osm <- get_openstreetmap(bbox=bkill_bb)

# get_openstreetmap(bbox = c(left = -95.80204, bottom = 29.38048, right =
# -94.92313, top = 30.14344), scale = 606250, format = c("png", "jpeg", "svg",
# "pdf", "ps"), messaging = FALSE, urlonly = FALSE, filename = NULL, color =
# c("color", "bw"), ...)


q0 <- bkill_bb %>%
  opq()

q1 <- bkill_bb %>%
  opq() %>%
  add_osm_feature("water", "river")
str(q1)



battriver <- osmdata_sf(q1)
saveRDS(battriver_)
battriver
ggmap(battriver)

# business filings ----
# https://data.ny.gov/browse?category=Economic+Development&utf8=%E2%9C%93
# https://data.ny.gov/Economic-Development/Department-of-State-Business-Filings-Beginning-199/m7i3-tv6j
# https://data.ny.gov/Economic-Development/Active-Corporations-Beginning-1800/n9v6-gdp6
# https://data.ny.gov/widgets/n9v6-gdp6

# get all/most places in a bounding box or similar area ----

# end ----
  





tmp <- google_places(search_string = 'mexican food', location=westmain9, radius=3218, key=GGMAP_GOOGLE_API_KEY)
str(tmp)
names(tmp)
as_tibble(tmp)

tmp2 <- google_places(search_string = 'restaurant', location=c(43.027846,-73.3841795), radius=10000, key=GGMAP_GOOGLE_API_KEY)
names(tmp2)




