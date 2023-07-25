# koel

`koel` provides a workflow to facilitate the process of searching for certain taxa within spatial and temporal constraints, summarising this information in a html table, and sending the table as an email. `koel` uses the [`galah`](https://galah.ala.org.au) package to query biodiversity data within the [Atlas of Living Australia](https://www.ala.org.au) (ALA), and the [`emayili`](https://github.com/datawookie/emayili/) package to send emails from R. `koel` is developed and being maintained by the [Science and Decision Support Team](https://labs.ala.org.au/about.html) at the ALA. 

The package is named after the Eastern Koel (*Eudynamys orientalis*), a large, canopy-dwelling cuckoo with [distinctive calls](https://xeno-canto.org/explore?query=Eudynamys%20orientalis). 


## Installation

Install the package from GitHub:

``` r
install.packages("remotes")
remotes::install_github("atlasoflivingaustralia/koel")
```


## Workflow

The figure below provides an overview of the suggested workflow when using `koel`. 

<img src="man/figures/koel_workflow.svg" align="left"/>

#### 1. Providing taxon names
`koel` requires one or more csv files of taxon names be provided as input, in this format: 
- The following four columns must be provided: `"correct_name"`, `"provided_name"`, `"synonyms"`, `"common_name"`
  
  - `"correct_name"` is the accepted scientific name of a species e.g. `"Urodynamis taitensis"`. The correct name should not contain authorities, commas, or double spaces.
    
  - `"provided_name"` is the verbatim name of the species as provided for the list e.g. `"Urodynamis taitensis (Sparrman, 1787)"`. It is not used when searching for species occurrences and may be identical to `"correct_name"` or `"common_name"`.
    
  -  `"synonyms"` is a comma-delimited (`", "`) string of additional search terms for each species e.g. `"Urodynamis taitensis belli, Cuculus taitensis, Eudynamis taitensis"`. The naming convention for `synonyms` is identical to that described for `correct_name`. This field may be left blank.
    
  -  `"common_name"` is the common name of the species e.g. `"Long-Tailed Koel"`. This field may be left blank.

- The following three columns are optional, and are used for spatial filtering: `"state"`, `"lga"`, `"shape"`
  
  - `"state"` is a comma-delimited (`", "`) string of Australian states and/or territories e.g. `"QLD, SA, NT"`. Standard upper case initialisms, abbreviations, and contractions (QLD, NSW, VIC, SA, WA, NT, ACT) are used. If the abbreviation `"AUS"` is provided, or if this field is left blank, no state-based spatial filters will be applied. This may be useful if it is necessary to search for records from island territories e.g. Norfolk Island, Christmas Island.
  
  -  `"lga"` is a comma-delimited (`", "`) string of Australian local government areas (LGAs) e.g. `"CITY OF PERTH, KANGAROO ISLAND COUNCIL"`. Names should be provided in upper case and the full list of LGA names is available through the ALA as a contextual layer (cl10923). This may be accessed through the [spatial portal](https://spatial.ala.org.au) or using the `galah` package (see below). If this field is left blank, no spatial filters at the level of LGA will be applied.
 
``` r
# view LGA names
galah::search_fields("cl10923") |>
  galah::show_values() |>
```

  - `"shape"` is an optional column to provide the name of a supplied shape (.shp) file folder, which can be used to filter recorded occurrences that lie within that shape file for each species. The column entry should match verbatim the name of the folder and spatial files. If multiple feature shapes are provided in the one shape file then the name of the feature an occurrence sits in will be provided in the email table output. If the `"shape"` column is left blank for a species or not provided for an entire list, then it will default to `NA`.



- Each taxon should be listed on a separate row
- If multiple csv files are to be provided, all files should be located in the same directory









The path to the folder containing all the list .csv files is the argument for the first function in the workflow, `collate_lists()`, which summarises the list names and paths for import and tidying with `get_species_lists2()`. `assign_common_names()` summarises duplicate common names and the outputs pass through to `lookup_species_count()`, which identifies species occurrences in the timeframe, and then `download_records()`, which downloads occurrence data and media. `build_email()` is the final function called in the workflow, and it facilitates the creation and sending of biosecurity alert emails to and from pre-specified email addresses.



#### Email sending

Emails are sent using the R package `{emayili}` and require the provision of an email address and password in the `build_email()` and `send_email()` functions. Currently there is only support for emails such as the official ALA biosecurity alerts email which can be interfaced with the `emayili::server()` function by way of the `host` and `port` arguments.

The email sending functions utilise an R Markdown template to create a summary document of all species occurrences. This template should be created and saved by the user in the working directory prior to use of the function. `build_email()` requests an argument specifying the path to the template and renders + saves the .Rmd file in-line. The object provided to the template is a data.frame named `table_df` and we recommended using package `{gt}` to render the dataframe.

`table_df` consists of one row per row occurrence, and four columns (`species`, `observation`, `location`, `image`) of html code referencing data and media related to each occurrence. A recommended output style within the markdown using `{gt}` may look something like this:

``` r
table_df |>
  gt::gt() |>
  gt::cols_label(
    species = "Species",
    observation = "Observation",
    location = "Location",
    image = "Image"
  ) |>
  gt::cols_align(align = c("left"), columns = everything()) |>
  gt::tab_options(table.width = pct(90))
```

`build_email()` requires the provision of a data.frame for the `email_list` argument. However, if the user wishes to simply save the .html occurrence tables than the `email_list` argument can be provided as an empty dataframe with the necessary columns i.e. `email_list <- data.frame(email = character(), list = character())`. By then specifying an output folder path (default value of `output_path` argument is `NULL`, the rendered markdown files will be saved to that directory without sending any emails.

