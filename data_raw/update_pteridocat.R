# Update pteridocat with new names since last release

library(devtools)
library(dwctaxon)

load_all()

# Changes 2023-07-29

# pteridocat <-
#   pteridocat |>
#   dct_add_row(
#     scientificName = "Asplenium pseudocapillipes Sang Hee Park, Jung Sung Kim & H.T.Kim",
#     scientificNameAuthorship = "Sang Hee Park, Jung Sung Kim & H.T.Kim",
#     taxonRank = "species",
#     genericName = "Asplenium",
#     specificEpithet = "pseudocapillipes",
#     references = "Plants 2022 Nov; 11(22): 3089",
#     taxonomicStatus = "accepted",
#     stamp_modified = TRUE,
#     strict = FALSE
#   ) |>
#   dct_validate(
#     check_mapping_parent = FALSE,
#     check_sci_name = FALSE,
#     valid_tax_status = "accepted, ambiguous synonym, provisionally accepted, synonym, variant" # nolint
#   )

# Changes 2023-07-30

# pteridocat <-
#   pteridocat |>
#   dct_add_row(
#     scientificName = "Pellaea calidirupium Brownsey & Lovis",
#     acceptedNameUsageID = "76B9V", # Pellaea calidirupia Brownsey & Lovis
#     taxonomicStatus = "synonym"
#   ) |>
#   dct_add_row(
#     scientificName = "Polystichum macleae (Baker) Diels",
#     acceptedNameUsageID = "4LMYZ", # Polystichum macleaii (Baker) Diels
#     taxonomicStatus = "synonym"
#   ) |>
#   dct_add_row(
#     scientificName = "Polystichum whiteleggei Watts",
#     acceptedNameUsageID = "4LND5", # Polystichum whiteleggii Watts
#     taxonomicStatus = "synonym"
#   ) |>
#   dct_add_row(
#     scientificName = "Dicranopteris caudata (Copel.) St.John",
#     acceptedNameUsageID = "6CTMZ", # Dicranopteris caudata (Copel.) H. St. John
#     taxonomicStatus = "synonym"
#   ) |>
#   dct_add_row(
#     scientificName = "Lepisorus yamaokae Seriz.",
#     acceptedNameUsageID = "3TFPY", # Lepisorus yamaokae Seriz. ined.
#     taxonomicStatus = "synonym"
#   ) |>
#   dct_add_row(
#     scientificName = "Polystichum cystostegia (Hook.) J.B.Armstr.",
#     acceptedNameUsageID = "4LMQQ", # Polystichum cystostegia (Hook.) Armstr.
#     taxonomicStatus = "synonym"
#   ) |>
#   dct_add_row(
#     scientificName = "Rouxopteris boryi (Kunze) H.M.Liu",
#     acceptedNameUsageID = "4THFL", # Rouxopteris boryi (Kunze) Hong M. Liu
#     taxonomicStatus = "synonym"
#   ) |>
#   dct_add_row(
#     scientificName = "Abrodictyum inexpectatum Dubuisson & Rouhan",
#     taxonomicStatus = "accepted",
#     namePublishedIn = "https://doi.org/10.11646/phytotaxa.568.1.6"
#   ) |>
#   dct_add_row(
#     scientificName = "Antrophyum crassifolium C.W.Chen",
#     taxonomicStatus = "accepted",
#     namePublishedIn = "https://doi.org/10.1111/cla.12538"
#   ) |>
#   dct_add_row(
#     scientificName = "Antrophyum pseudolatifolium C.W.Chen",
#     taxonomicStatus = "accepted",
#     namePublishedIn = "https://doi.org/10.1111/cla.12538"
#   ) |>
#   dct_add_row(
#     scientificName = "Antrophyum tahitense C.W.Chen & J.H.Nitta",
#     taxonomicStatus = "accepted",
#     namePublishedIn = "https://doi.org/10.1111/cla.12538"
#   ) |>
#   dct_add_row(
#     scientificName = "Asplenium delinghaense S.Q.Liang & X.C.Zhang",
#     taxonomicStatus = "accepted",
#     namePublishedIn = "https://doi.org/10.14258/turczaninowia.25.4.17"
#   ) |>
#   dct_add_row(
#     scientificName = "Hymenasplenium tholiformis Liang Zhang, W.B.Ju & K.W.Xu",
#     taxonomicStatus = "accepted",
#     namePublishedIn = "https://doi.org/10.3897/phytokeys.204.85746"
#   ) |>
#   dct_add_row(
#     scientificName = "Lepisorus rufofuscus T.Fujiwara",
#     taxonomicStatus = "accepted",
#     namePublishedIn = "Acta Phytotax. Geobot. 73 (3): 171-182"
#   ) |>
#   dct_add_row(
#     scientificName = "Pteris pseudoamoena D.M.Yang & R.Guo",
#     taxonomicStatus = "accepted",
#     namePublishedIn = "https://doi.org/10.11646/phytotaxa.550.3.1 "
#   ) |>
#   dct_validate(
#     check_mapping_parent = FALSE,
#     check_sci_name = FALSE,
#     valid_tax_status = "accepted, ambiguous synonym, provisionally accepted, synonym, variant" # nolint
#   )

pteridocat <-
  pteridocat |>
  dct_add_row(
    scientificName = "Tectaria acrophoroides S.Y.Dong & C.W.Chen",
    taxonomicStatus = "accepted",
    namePublishedIn = "https://doi.org/10.1600/036364422X16512572274981"
  ) |>
  dct_add_row(
    scientificName = "Tectaria glenniana S.Y.Dong & C.W.Chen",
    taxonomicStatus = "accepted",
    namePublishedIn = "https://doi.org/10.1600/036364422X16512572274981"
  ) |>
  dct_add_row(
    scientificName = "Tectaria pallescens S.Y.Dong & C.W.Chen",
    taxonomicStatus = "accepted",
    taxonRemarks = "Nom. Inval. due to multiple specimens cited as holotype",
    namePublishedIn = "https://doi.org/10.1600/036364422X16512572274981"
  ) |>
  dct_add_row(
    scientificName = "Tectaria vanikoroensis S.Y.Dong & C.W.Chen",
    taxonomicStatus = "accepted",
    namePublishedIn = "https://doi.org/10.1600/036364422X16512572274981"
  ) |>
  dct_validate(
    check_mapping_parent = FALSE,
    check_sci_name = FALSE,
    valid_tax_status = "accepted, ambiguous synonym, provisionally accepted, synonym, variant" # nolint
  )

usethis::use_data(pteridocat, overwrite = TRUE)
