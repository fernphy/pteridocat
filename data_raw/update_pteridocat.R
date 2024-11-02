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
    scientificName = "Pyrrosia polydactylos (Hance) Ching",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Pyrrosia polydactyla (Hance) Ching"
  ) |>
  dct_add_row(
    scientificName = "Thelypteris hattorii (H.Ito) Tagawa",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Thelypteris hattori (H. Itô) Tagawa"
  ) |>
  dct_add_row(
    scientificName = "Thelypteris hattorii var. hattorii (H.Ito) Tagawa",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Thelypteris hattori (H. Itô) Tagawa"
  ) |>
  dct_add_row(
    scientificName = "Thelypteris hattorii var. nemoralis (Ching) Sa.Kurata",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Thelypteris hattori var. nemoralis (Ching) Sa. Kurata"
  ) |>
  dct_add_row(
    scientificName =
      "Asplenium exiguum nothosubsp. glenniei (Baker) Fraser-Jenk.",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Asplenium exiguum subsp. glenniei (Baker) Fraser-Jenk."
  ) |>
  dct_add_row(
    scientificName = "Athyrium fimbriatum T.Moore",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Athyrium fimbriatum Wall. ex T. Moore "
  ) |>
  dct_add_row(
    scientificName = "Dryopteris panda (C.B. Clarke) Christ",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Dryopteris panda (C.B.Clarke) H.Christ"
  ) |>
  dct_add_row(
    scientificName = "Dryopteris spinulosa (O.F. Mull.) Watt",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Dryopteris spinulosa (O. F. Müll.) Kuntze"
  ) |>
  dct_add_row(
    scientificName = "Thelypteris salzmannii (Fee) C.V.Morton",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Thelypteris salzmannii (Fée) Morton"
  ) |>
  dct_add_row(new_dat = readr::read_csv("data_raw/names_add_2024-11-02.csv")) |>
  dct_validate()

pteridocat <- pteridocat_new

usethis::use_data(pteridocat, overwrite = TRUE)
