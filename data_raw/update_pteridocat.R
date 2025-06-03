# Update pteridocat with new names since last release

library(devtools)
library(dwctaxon)

load_all()

dct_options(
  valid_tax_status =
    "accepted, ambiguous synonym, provisionally accepted, synonym",
  check_mapping_parent = FALSE,
  check_sci_name = FALSE,
  remap_parent = FALSE)

pteridocat_new <-
  pteridocat |>
  # Fix spelling
  dct_modify_row(
    taxonID = "37XGF",
    scientificName = "Dryopteris qandoensis Ching" # from qamdoensis
  ) |>
  dct_modify_row(
    taxonID = "37Y37",
    scientificName = "Dryopteris whangshangensis Ching" # from whangshanensis
  ) |>
  # Add names
  dct_add_row(
    scientificName = "Ynesmexia subcordata (D.C.Eaton & Davenp.) Fauskee & Windham", # nolint
    taxonomicStatus = "accepted",
    taxonRank = "species",
    namePublishedIn = "74(x): [15 of 19] (2025). "
  ) |>
  dct_add_row(
    scientificName = "Dryopteris × tiantangzhaiensis H.J.Wei & B.Chen",
    taxonomicStatus = "accepted",
    taxonRank = "species",
    namePublishedIn = "Taxon 73(1): 136 (2024)"
  ) |>
  dct_add_row(
    scientificName = "Diplazium zangnanense R.Wei & M.J.Lian",
    taxonomicStatus = "accepted",
    taxonRank = "species",
    namePublishedIn = "Nordic J. Bot. 2024(12)-e04334: 5 (2024)"
  ) |>
  dct_add_row(
    scientificName = "Ophioglossum hongii Min Y.Li, F.S.Yang & J.P.Shu",
    taxonomicStatus = "accepted",
    taxonRank = "species",
    namePublishedIn = "Phytotaxa 676(2): 171 (2024)3265986599"
  ) |>
  dct_validate()

pteridocat <- pteridocat_new

usethis::use_data(pteridocat, overwrite = TRUE)
