#' internal function to build maps
#'
#' use basemaps from OpenStreetMaps, called via `maptiles`
#'
#' @importFrom maptiles get_tiles
#' @export

build_map_thumbnail <- function(list_row, cache_path){

  # need to add defensive programming + check for existence of the maps directory
  box_size <- 0.15
  x <- list_row |> st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = "WGS84")
  x_box <- st_bbox(c(
    xmin = list_row$decimalLongitude - box_size,
    xmax = list_row$decimalLongitude + box_size,
    ymin = list_row$decimalLatitude - box_size,
    ymax = list_row$decimalLatitude + box_size),
    crs = "WGS84"
  )
  y <- get_tiles(x_box, zoom = 10, crop = TRUE)
  png(filename = paste0(cache_path, "maps/", list_row$recordID, ".png"))
  plot_tiles(y)
  plot(x, col = "black", cex = 5, pch = 16, add = TRUE) # errors here
  dev.off()
}
