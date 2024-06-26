% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_species_lists.R
\name{get_species_lists}
\alias{get_species_lists}
\title{Import and tidy species lists}
\usage{
get_species_lists(lists_path, list_suffix = "_list", synonym_delimiter = ",")
}
\arguments{
\item{lists_path}{Filepath to the directory where lists are saved, ending in
`"/"`.}

\item{list_suffix}{String between the name of the list and the file
extension. Case insensitive. Defaults to "_list".}

\item{synonym_delimiter}{Optional string specifying the delimiter used for
multiple synonyms in the synonym column of the lists. Defaults to ",".}
}
\value{
A data.frame with 8 columns and a row for every unique combination
  of values in the "correct_name", "provided_name", "synonyms", and
  "common_name" columns. If not present in the provided species lists,
  "state", "lga", and "shape" columns will be added. The "list_name" column
  is added to indicate the name of the original provided species list.
  This data.frame may be passed to `assign_common_names()` and
  `search_occurrences()`.
}
\description{
This function imports species lists saved as csv files in a directory and
combines them into a single data.frame. Names in the lists are cleaned by
removing whitespace, punctuation, and unnecessary text to make them suitable
for use with the galah package.
}
