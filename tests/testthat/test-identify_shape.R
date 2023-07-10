# testing for identify_shape()

# check that incorrect inputs are flagged and corrected
test_that("identify_shape() takes correct input arguments", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c("AUS"),
    lga = c("some_LGA"),
    shape = c("shape1"),
    list1 = c(TRUE)
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "03-10-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start)
  shapes_path <- paste0(withr::local_tempdir(), "/")
  dir.create(paste0(shapes_path, "lord_howe"))

  # species_records
  expect_error(identify_shape(5))
  expect_error(identify_shape(species_records |> dplyr::select(-cl10923)))
  # common_names
  expect_error(identify_shape(species_records, 5))
  expect_error(identify_shape(species_records, c("a", "b")))
  expect_error(identify_shape(species_records, "./dummy_path"))
  expect_error(identify_shape(species_records, "./dummy_path/"))
})

# check that the function works properly for various inputs
# Namely: no records, no shp files, no SHAPE_NAME col, everything
test_that("identify_shape() behaves as intended for various inputs", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus"),
    provided_name = c("Onychoprion fuscatus"),
    search_term = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern"),
    state = c(NA),
    lga = c(NA),
    shape = c("lord_howe"),
    list1 = c(TRUE)
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus"),
    common_name = c("Sooty Tern")
  )
  event_date_start <- "01-09-2022"
  event_date_end <- "30-11-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  {
    shapes_path <- paste0(withr::local_tempdir(), "/")
    dir.create(paste0(shapes_path, "lord_howe"))
    data.frame(lat = c(-31.469668, -31.953327, -31.763202, -31.324314, -31.469668),
               lng = c(158.826619, 159.249608, 159.485811,  159.142469, 158.826619)) |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4283") |>
      dplyr::summarise(geometry = sf::st_combine(geometry)) |>
      sf::st_cast("POLYGON") |>
      mutate(SHAPE_NAME = "Lord Howe Island") |>
      sf::st_write(paste0(shapes_path, "/lord_howe/lord_howe.shp"),
                   append = FALSE)
  }

  # check the intended use case
  is_output <- identify_shape(species_records, shapes_path)
  expect_s3_class(is_output, "data.frame")
  expect_equal(144, nrow(is_output |> filter(shape_feature == "Lord Howe Island")))
  expect_equal(0, nrow(is_output |> filter(shape_feature == "lord_howe")))
  expect_equal(510, nrow(is_output |> filter(is.na(shape_feature))))


  # check the case where the shp file has no SHAPE_NAME field
  {
    data.frame(lat = c(-31.469668, -31.953327, -31.763202, -31.324314, -31.469668),
             lng = c(158.826619, 159.249608, 159.485811,  159.142469, 158.826619)) |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4283") |>
      dplyr::summarise(geometry = sf::st_combine(geometry)) |>
      sf::st_cast("POLYGON") |>
      sf::st_write(paste0(shapes_path, "/lord_howe/lord_howe.shp"),
                   append = FALSE)
  }
  is_output <- identify_shape(species_records, shapes_path)
  expect_equal(0, nrow(is_output |> filter(shape_feature == "Lord Howe Island")))
  expect_equal(144, nrow(is_output |> filter(shape_feature == "lord_howe")))
  expect_equal(510, nrow(is_output |> filter(is.na(shape_feature))))

  # check the case where no shp file is provided
  species_records$shape <- NA
  is_output <- identify_shape(species_records, shapes_path)
  expect_true(all(is.na(is_output$shape_feature)))

  # check the case where no records are provided
  event_date_start <- "07-03-2023"
  event_date_end <- "09-03-2023"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)
  is_output <- identify_shape(species_records, shapes_path)

  expect_equal(0, nrow(is_output))
  expect_true("shape_feature" %in% colnames(is_output))
})

# check that the function can support multiple different shapefiles
test_that("identify_shape() behaves as intended for various inputs", {
  # set up correct arguments
  species_list <- data.frame(
    correct_name = c("Onychoprion fuscatus", "Thalassarche bulleri"),
    provided_name = c("Onychoprion fuscatus", "Thalassarche bulleri"),
    search_term = c("Onychoprion fuscatus", "Thalassarche bulleri"),
    common_name = c("Sooty Tern", "Buller's Albatross"),
    state = c(NA),
    lga = c(NA),
    shape = c("lord_howe", "tasmania"),
    list1 = c(TRUE)
  )
  common_names <- tibble(
    correct_name = c("Onychoprion fuscatus", "Thalassarche bulleri"),
    common_name = c("Sooty Tern", "Buller's Albatross")
  )
  event_date_start <- "01-09-2022"
  event_date_end <- "30-11-2022"
  species_records <- search_occurrences(species_list, common_names, event_date_start, event_date_end)

  {
    shapes_path <- paste0(withr::local_tempdir(), "/")
    dir.create(paste0(shapes_path, "lord_howe"))
    data.frame(lat = c(-31.469668, -31.953327, -31.763202, -31.324314, -31.469668),
               lng = c(158.826619, 159.249608, 159.485811,  159.142469, 158.826619)) |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4283") |>
      dplyr::summarise(geometry = sf::st_combine(geometry)) |>
      sf::st_cast("POLYGON") |>
      mutate(SHAPE_NAME = "Lord Howe Island") |>
      sf::st_write(paste0(shapes_path, "/lord_howe/lord_howe.shp"),
                   append = FALSE)
    dir.create(paste0(shapes_path, "tasmania"))
    data.frame(lat = c(-44, -42, -42, -44, -44),
               lng = c(149, 149, 145,  145, 149)) |>
      sf::st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4283") |>
      dplyr::summarise(geometry = sf::st_combine(geometry)) |>
      sf::st_cast("POLYGON") |>
      mutate(SHAPE_NAME = "Tasmania") |>
      sf::st_write(paste0(shapes_path, "/tasmania/tasmania.shp"),
                   append = FALSE)
  }
  # check the intended use case
  is_output <- identify_shape(species_records, shapes_path)
  expect_equal(144, nrow(is_output |> filter(shape_feature == "Lord Howe Island")))
  expect_equal(62, nrow(is_output |> filter(shape_feature == "Tasmania")))
  expect_equal(585, nrow(is_output |> filter(is.na(shape_feature))))

})
