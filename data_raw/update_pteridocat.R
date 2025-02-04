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
  # Swap status of Teratophyllum leptocarpum and Lomariopsis leptocarpa
  dct_modify_row(
    scientificName = "Teratophyllum leptocarpum (Fée) Holttum",
    taxonomicStatus = "accepted"
  ) |>
  dct_modify_row(
    scientificName = "Lomariopsis leptocarpa Fée",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Teratophyllum leptocarpum (Fée) Holttum"
  ) |>
  dct_validate()

pteridocat <- pteridocat_new

usethis::use_data(pteridocat, overwrite = TRUE)
