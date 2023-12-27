# Update pteridocat with new names since last release

library(devtools)
library(dwctaxon)

load_all()

names_add <- readr::read_csv("data_raw/names_add_2023-12-27.csv")

pteridocat <-
  dct_add_row(
    pteridocat,
    new_dat = names_add
  ) |>
  dct_validate(
    check_mapping_parent = FALSE,
    check_sci_name = FALSE,
    valid_tax_status = "accepted, ambiguous synonym, provisionally accepted, synonym, variant" # nolint
  )

usethis::use_data(pteridocat, overwrite = TRUE)
