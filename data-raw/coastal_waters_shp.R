library(tidyverse)
library(sf)
library(sfheaders)

CWA <- st_read("C:/Users/WAI045/OneDrive - CSIRO/ALA/Biosecurity/shapefiles/CWA1980_zones/Coastal_Waters_AMB2020_Areas.shp")

coastal_waters_shp <- CWA |>
  rbind(
    CWA |>
      filter(COMMENT == "New South Wales") |>
      mutate(geometry = st_difference(sf_remove_holes(geometry),
                                      geometry),
             COMMENT = "Australian Capital Territory",
             NAME = "Coastal Waters (State Powers) Act 1980 - AMB2020 - Area - Australian Capital Territory",
             OBJNAM = NA,
             MRN = NA,
             Shape_Leng = st_length(geometry), #doesn't work for now
             Shape_Area = st_area(geometry)) #different units to rest of the column
  ) |>
  dplyr::select(COMMENT, geometry) |>
  rename(state_long = COMMENT) |>
  mutate(state_abbr = c("QLD", "NT", "WA", "SA", "NSW", "VIC", "TAS", "ACT")) |>
  relocate(state_abbr, 1)

usethis::use_data(coastal_waters_shp, overwrite = TRUE)
