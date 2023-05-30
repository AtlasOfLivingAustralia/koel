# koel

R package for sending email alerts for biodiversity queries

------------------------------------------------------------------------

`koel` is an R package created to facilitate the producing and sending of email alerts for biosecurity (and other) purposes pertaining to biodiversity data hosted by 'living atlases' such as the Atlas of Living Australia (ALA). It is structured around the use of the R package `galah`, which enables direct interfaces with these biodiversity databases and their records. Currently, `koel` is only supported to interact with the ALA. Like `galah`, `koel` is produced and maintained by the Science & Decision Support Team at the ALA.

`koel` comprises a small number of functions that interact within a clear workflow to take a folder containing .csv files of species lists and produce/send informative emails summarising occurrences of species on these lists from some pre-determined time-frame. These functions may be utilised externally of the recommended/intended workflow, however due to the rigid nature of taxonomic conventions and output from `galah` it is suggested that the workflow is followed until a good understanding of the inputs and outputs is obtained.

------------------------------------------------------------------------

### Installation

`koel` is not currently on CRAN and so must be installed from github. This can be done as follows:

```{r}
install.packages("remotes")
remotes::install_github("atlasoflivingaustralia/koel")
```

------------------------------------------------------------------------

### Workflow

The general workflow of the functions in `koel` will be described here along with inputs an outputs, however more detailed descriptions are provided in individual function documentations.

To begin, the user must possess at least one list of species of interest compiled as a .csv with exactly five key columns, namely `"correct_name", "provided_name", "synonyms", "common_name", "jurisdiction"`. Each row represents a single species. Further information regarding the formatting of these columns is provided below. The names of the list .csv files are important and should be unique, as they form the basis of the workflow formatting.

The function workflow from .csv list files through to sending of emails is summarised by the below diagram.

```{r}
install.packages("DiagrammeR")
library(DiagrammeR)

nodes <- create_node_df(
  n = 10,
  type = "lower",
  label = c("lists csv folder", "collate_lists()", "get_species_lists2()",
            "assign_common_names()", "lookup_species_counts()",
            "download_records()", "build_email()",
            "build_gt_tables()", "build_map_thumbnail()", "send_email()")
)
```

The path to the folder containing all the list .csv files is the argument for the first function in the workflow, `collate_lists()`, which summarises the list names and paths for import and tidying with `get_species_lists2()`. `assign_common_names()` summarises duplicate common names and the outputs pass through to `lookup_species_count()`, which identifies species occurrences in the timeframe, and then `download_records()`, which downloads occurrence data and media. `build_email()` is the final function called in the workflow, and it facilitates the creation and sending of biosecurity alert emails to and from pre-specified email addresses.

An example workflow may look

#### Email sending

Emails are sent using the R package `{emayili}` and require the provision of an email address and password in the `build_email()` and `send_email()` functions. Currently there is only support for the use an official ALA biosecurity alerts email given the requirement to provide server host and port information.

#### List Formatting

-   `"correct_name"` is the accepted scientific name of a species, for example `"Urodynamis taitensis"`. The correct name should not contain authorities, commas, or double spaces.

-   `"provided_name"` is the verbatim name of the species as provided for the list, e.g. `"Urodynamis taitensis (Sparrman, 1787)"`. It may be identical to the `"correct_name"` or `"common_name"`. It is not used for any searching of occurrence records.

-   `"synonyms"` is a comma-delimited (`", "`) list of additional search terms for each species e.g. `"Urodynamis taitensis belli, Cuculus taitensis, Eudynamis taitensis"`. The naming standard for `synonyms` is identical to that described for `correct_name`. If no taxonomic synonyms exist then this may be left blank.

-   `"common_name"` is the accepted common name of the species as recognised by the list provided/creator e.g. `"Long-Tailed Koel"`. If no common name exists or is known then this may be left blank.

-   `"jurisdiction"` is a comma-delimited (`", "`) list of Australian state and/or territory jurisdictions to filter recorded occurrences by for each species e.g. `"QLD, SA, NT"`. Australian state and territory abbreviations (QLD, NSW, VIC, SA, WA, NT, ACT) are used, while for Australia-wide alerts (including island territories such as Norfolk Island, Christmas Island etc.) the abbreviation should be provided as simply `"AUS"`. If the jurisdiction entry is left blank for a species, it will default to `"AUS"`.
