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
  dct_add_row(new_dat = read_csv("data_raw/names_add_2025-02-04.csv")) |>
  dct_validate()

pteridocat <- pteridocat_new

usethis::use_data(pteridocat, overwrite = TRUE)
