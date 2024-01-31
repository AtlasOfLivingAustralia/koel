library(tidyverse)
library(sf)
library(sfheaders)

# shapefiles downloaded from https://d28rz98at9flks.cloudfront.net/144567/144567_01_0.zip
CWA <- st_read("path/to/CWA_file.shp")
TS <- st_read("path/to/TS_file.shp")


coastal_waters <- CWA |>
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

territorial_seas <- TS |>
  mutate(state_long = case_when(
    OBJNAM == "AMB273175" ~ "Heard and McDonald Islands",
    OBJNAM == "AMB273173" ~ "Elizabeth Reef",
    OBJNAM == "AMB273172" ~ "Middleton Reef",
    OBJNAM == "AMB273171" ~ "Norfolk Island",
    OBJNAM == "AMB273144" ~ "Christmas Island",
    OBJNAM == "AMB273145" ~ "Cocos (Keeling) Islands",
    OBJNAM == "AMB273146" ~ "Ashmore Island",
    OBJNAM == "AMB273147" ~ "Cartier Island",
    .default = NA
  )) |>
  filter(!is.na(state_long)) |>
  mutate(geometry = sf_remove_holes(geometry)) |>
  mutate(state_abbr = state_long) |>
  select(state_abbr, state_long, geometry)

coastal_waters_shp <- rbind(coastal_waters, territorial_seas)

usethis::use_data(coastal_waters_shp, overwrite = TRUE)
