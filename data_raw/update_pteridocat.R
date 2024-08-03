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
  # Add new name: Dryopteris jinpingensis
  dct_add_row(
    scientificName = "Dryopteris jinpingensis Z.Y. Zuo, J. Mei Lu & D.Z.", # nolint
    taxonomicStatus = "accepted",
    namePublishedIn = "PhytoKeys 239: 195-204. https://doi.org/10.3897/phytokeys.239.118655"
  ) |>
  dct_validate()

usethis::use_data(pteridocat, overwrite = TRUE)
