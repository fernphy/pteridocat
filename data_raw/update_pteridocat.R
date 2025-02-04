# Update pteridocat with new names since last release

library(devtools)
library(dwctaxon)

load_all()

dct_options(
  valid_tax_status = "accepted, ambiguous synonym, provisionally accepted, synonym",
  check_mapping_parent = FALSE,
  check_sci_name = FALSE)

pteridocat_new <-
  pteridocat |>
  dct_add_row(
    scientificName = "Ceratopteris baguangensis F.G.Wang & Zi Xiang Li",
    taxonomicStatus = "accepted",
    genericName = "Ceratopteris",
    specificEpithet = "baguangensis",
    scientificNameAuthorship = "F.G.Wang & Zi Xiang Li",
    namePublishedIn = "10.11646/phytotaxa.658.2.8"
  ) |>
  dct_validate()

pteridocat <- pteridocat_new

usethis::use_data(pteridocat, overwrite = TRUE)
