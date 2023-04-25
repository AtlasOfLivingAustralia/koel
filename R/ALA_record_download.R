#' Download ALA records identified in count search from species_list
#'
#' Redoing Martin's function minus lapply and to hopefully run smoother
#'
#' @param species_list A `data.frame` containing columns of 'correct_name', 'search_term', 'common_name' for species, and T/F columns for each list
#' @param filter_df A `list` of ALA query conditions
#' @importFrom galah atlas_counts
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom dplyr mutate
#' @export

ALA_record_download <- function(species_list) {
  if (nrow(species_list) > 0) {
    cat(paste0("Downloading records for ", n_spp, " species\n"))

    galah_config(
      email = "callumwaite2000@gmail.com",
      run_checks = FALSE,
      verbose = TRUE)

    # download occurrences
    occ_list <- map(
      .x = species_list$search_term,
      .f = function(search_term) {
        cat(search_term)
        filter_tmp <- filter_df
        filter_tmp$query[4] <- sub("none", search_term, filter_df$query[4])
        select_tmp <- galah_select(
          raw_scientificName,
          cl22,
          group = c("basic", "media"))
        atlas_occurrences(
          filter = filter_tmp,
          select = select_tmp)
        }
    ) |>
      list_rbind() |>
      search_media() |>
      filter(!duplicated(recordID)) |> # i.e. one image per record
      collect_media(
        path = "cache/species_images",
        type = "thumbnail") |>
        # n = 1 - is this working? I.e. are we getting as many images as records?
        # does show_all_media need an `n` arg?
      # join back with original ('correct') names
      left_join(species_list,
                by = c("verbatimScientificName" = "search_term"))
  }
}
