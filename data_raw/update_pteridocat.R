# Update pteridocat with new names since last release

library(devtools)
library(dwctaxon)

load_all()

pteridocat <-
  pteridocat |>
  dct_add_row(
    scientificName = "Asplenium pseudocapillipes Sang Hee Park, Jung Sung Kim & H.T.Kim",
    scientificNameAuthorship = "Sang Hee Park, Jung Sung Kim & H.T.Kim",
    taxonRank = "species",
    genericName = "Asplenium",
    specificEpithet = "pseudocapillipes",
    references = "Plants 2022 Nov; 11(22): 3089",
    taxonomicStatus = "accepted",
    stamp_modified = TRUE,
    strict = FALSE
  ) |>
  dct_validate(
    check_mapping_parent = FALSE,
    check_sci_name = FALSE,
    valid_tax_status = "accepted, ambiguous synonym, provisionally accepted, synonym, variant" # nolint
  )

usethis::use_data(pteridocat, overwrite = TRUE)
