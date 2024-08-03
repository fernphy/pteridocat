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
  # Add Hemionitis ekmanii Maxon same as Hemionitis ekmanii (Maxon) Christenh.
  # (synonym)
  dct_add_row(
    scientificName = "Hemionitis ekmanii Maxon",
    taxonomicStatus = "synonym",
    acceptedNameUsageID = "47T5J"
  ) |>
  # Add Hymenophyllum okadae Masam as alternate spelling of
  # Hymenophyllum okadai Masam.
  dct_add_row(
    scientificName = "Hymenophyllum okadae Masam.",
    taxonomicStatus = "synonym",
    acceptedNameUsageID = "7WPRQ" # Hymenophyllum okadai Masam.
  ) |>
  # Add Quercifilix zeylanica (Houtt.) Copel. same as
  # Quercifilix zeilanica (Houtt.) Copel. (synonym)
  dct_add_row(
    scientificName = "Quercifilix zeylanica (Houtt.) Copel.",
    taxonomicStatus = "synonym",
    acceptedNameUsageID = "553FK" # Hymenophyllum okadai Masam.
  ) |>
  # Add alternative presentation of author
  dct_add_row(
    scientificName = "Asplenium inaequilaterale Willd.",
    taxonomicStatus = "synonym",
    acceptedNameUsageID = "HKM4" # Asplenium inaequilaterale Bory ex Willd.
  ) |>
  dct_add_row(
    scientificName = "Asplenium paucijugum F.Ballard",
    taxonomicStatus = "synonym",
    acceptedNameUsageID = "7KZ8C" # Asplenium variabile var. paucijugum (Ballard) Alston # nolint
  ) |>
  dct_add_row(
    scientificName = "Asplenium schelpei Braithwaite",
    taxonomicStatus = "synonym",
    acceptedNameUsageID = "686WV" # Asplenium schelpei Braithw.
  ) |>
  dct_add_row(
    scientificName = "Danaea tenera C.V.Morton",
    taxonomicStatus = "synonym",
    acceptedNameUsageID = "343M3" # Danaea tenera Morton
  ) |>
  dct_add_row(
    scientificName = "Hymenophyllum schomburgkii C.Pres",
    taxonomicStatus = "synonym",
    acceptedNameUsageID = "3NKM4" # Hymenophyllum polyanthos (Sw.) Sw.
  ) |>
  # add names from CSV
  dct_add_row(new_dat = readr::read_csv("data_raw/names_add_2024-08-03.csv")) |>
  dct_validate()

usethis::use_data(pteridocat, overwrite = TRUE)
