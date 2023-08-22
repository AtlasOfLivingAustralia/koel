library(tidyverse)
library(sf)
library(sfheaders)
library(ozmaps)

aus <- ozmap_data(data = "states")

# shapefiles downloaded from https://d28rz98at9flks.cloudfront.net/144571/144571_01_0.zip

EEZ_treaty <- st_read("path/to/file.shp")[-c(1, 9),] |>
  st_make_valid() |>
  st_union() |>
  st_as_sf()
TS <- st_read("path/to/file.shp")[-c(38:40),] |>
  st_make_valid() |>
  st_union() |>
  st_as_sf()
CZ <- st_read("path/to/file.shp")[-c(20:21),] |>
  st_make_valid() |>
  st_union() |>
  st_as_sf()

aus_territory <- rbind(EEZ_treaty, TS, CZ) |>
  st_union() |>
  st_as_sf() |>
  sf_remove_holes()

usethis::use_data(aus_territory, overwrite = TRUE)

