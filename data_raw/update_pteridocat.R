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
  dct_modify_row(
    scientificName = "Teratophyllum williamsii (Underw.) Holttum",
    taxonomicStatus = "accepted"
  ) |>
  dct_validate()

pteridocat <- pteridocat_new

usethis::use_data(pteridocat, overwrite = TRUE)
