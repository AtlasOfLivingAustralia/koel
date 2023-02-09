#' internal function to build maps
#'
#' use basemaps from OpenStreetMaps, called via `maptiles`
#'
#' @importFrom maptiles get_tiles
#' @export

map_plot <- function(a){
  box_size <- 0.15
  x <- a |> st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = "WGS84")
  x_box <- st_bbox(c(
    xmin = a$decimalLongitude - box_size,
    xmax = a$decimalLongitude + box_size,
    ymin = a$decimalLatitude - box_size,
    ymax = a$decimalLatitude + box_size),
    crs = "WGS84"
  )
  y <- get_tiles(x_box, zoom = 10, crop = TRUE)
  png(filename = paste0("cache/maps/", a$recordID, ".png"))
  plot_tiles(y)
  plot(x, col = "black", cex = 5, pch = 16, add = TRUE) # errors here
  dev.off()
}
