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
  # Change D. giganteum (Baker) Ching to accepted from synonym of D. maximum
  dct_modify_row(
    scientificName = "Diplazium giganteum (Baker) Ching",
    taxonomicStatus = "accepted",
    nameAccordingTo = "Wei R, Schneider H, Zhang XC (2013) Taxon 62:441-457"
  ) |>
  dct_validate()

usethis::use_data(pteridocat, overwrite = TRUE)
