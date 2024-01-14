# Update pteridocat with new names since last release

library(devtools)
library(dwctaxon)

load_all()

dct_options(
  valid_tax_status = "accepted, ambiguous synonym, provisionally accepted, synonym",
  check_mapping_parent = FALSE,
  check_sci_name = FALSE)

pteridocat <-
  pteridocat |>
  # Change Alsophila corcovadensis Fée to accepted from synonym of
  # Cyathea glaziovii Domin
  dct_modify_row(
    scientificName = "Alsophila corcovadensis Fée",
    taxonomicStatus = "accepted",
    taxonRemarks =
    "https://www.ncbi.nlm.nih.gov/nuccore/KP337975 blasts to Alsophila"
  ) |>
  dct_validate()

usethis::use_data(pteridocat, overwrite = TRUE)
