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
  # Regalado et al 2024 only moved A. aspidioides (type of the genus) to
  # Ctenitis. There are two more Atalopteris species, but whether they are
  # truly distinct from A. aspidioides is not clear. These lack
  # names in Ctenitis. Leave them for now.
  dct_add_row(
    scientificName = "Ctenitis cubensis C. Sánchez",
    taxonomicStatus = "accepted",
    namePublishedIn = "Regalado, L., Lóriga, J., Beck, A. 2024. Plastid data supports the inclusion of the Greater Antillean genus Atalopteris in Ctenitis (Dryopteridaceae: Polypodiales). American Fern Journal 114(4): 289-307."
  ) |>
  dct_modify_row(
    scientificName = "Atalopteris aspidioides (Griseb.) Maxon & C. Chr.",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Ctenitis cubensis C. Sánchez",
    remap_parent = FALSE
  ) |>
  dct_modify_row(
    scientificName = "Thelypteris pachyrhachis (Kunze ex Mett.) Ching",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Amauropelta pachyrhachis (Kunze ex Mett.) Salino & T. E. Almeida"
  ) |>
  dct_modify_row(
    scientificName = "Blechnum fraseri var. philippense (Christ) Copel.",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Diploblechnum fraseri (A. Cunn.) De Vol"
  ) |>
  dct_modify_row(
    scientificName = "Trichomanes singaporianum (Bosch) Alderw.",
    taxonomicStatus = "accepted"
  ) |>
  dct_modify_row(
    scientificName = "Cephalomanes singaporianum Bosch",
    taxonomicStatus = "synonym",
    acceptedNameUsage = "Trichomanes singaporianum (Bosch) Alderw.",
    remap_parent = FALSE
  ) |>
  dct_validate()

pteridocat <- pteridocat_new

usethis::use_data(pteridocat, overwrite = TRUE)
