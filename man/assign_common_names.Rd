% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assign_common_names.R
\name{assign_common_names}
\alias{assign_common_names}
\title{Assign single common name per taxon}
\usage{
assign_common_names(species_names)
}
\arguments{
\item{species_names}{A data.frame containing taxon information, with columns
named "correct_name" and "common_name".}
}
\value{
A data.frame with two columns, named "correct_name" and
  "common_name". The number of unique values in "correct_name" will be the
  same as that supplied in "species_names". Each value in "correct_name" will
  only have a single corresponding match in "common_name". This may be passed
  to `search_occurrences()`.
}
\description{
In cases where taxa have been supplied with multiple common names, this
function selects and assigns a single common name to each taxon. If a taxon
has not been supplied with a common name, it is assigned "[Common Name
Unknown]".
}
