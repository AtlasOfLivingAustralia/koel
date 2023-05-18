#' Coastal Waters Act .shp files
#'
#' A data-frame containing the multipolygon objects that describe the maritime
#'    and terrestrial regions of jurisdiction for the eight Australian states
#'    and territories (QLD, NSW, VIC, TAS, SA, WA, ACT, NT). The jurisdictions
#'    for all states and the NT are defined by and sourced from the Coastal
#'    Waters Act 1980. The ACT is not assigned maritime jurisdiction under any
#'    iteration of the act, however the marine territory of Jervis Bay has been
#'    allocated to the ACT given it is not allocated to NSW.
#'
#' @format ## `coastal_waters_shp`
#' A data frame with 8 rows and 3 columns:
#' \describe{
#'   \item{state_abbr}{2 or 3 letter Australian state and territory
#'                      abbreviations}
#'   \item{state_long}{full Australian state and territory names }
#'   \item{geometry}{MULTIPOLYGONs outlining the state and territory
#'                    jurisdictions under the Coastal Waters Act 1980}
#' }
#' @source <https://ecat.ga.gov.au/geonetwork/ofmj3/api/records/fb266c57-4d77-4449-9e6b-d45afff4f083>
"coastal_waters_shp"
