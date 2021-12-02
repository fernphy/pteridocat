#' Taxonomic database of Pteridophytes
#'
#' Taxonomic database of pteridophytes following
#' [Darwin Core standards for taxonomic data](https://dwc.tdwg.org/terms/#taxon).
#'
#' Description of data below modified from
#' [Biodiversity Information Standards (TDWG)](https://www.tdwg.org/) under
#' [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).
#'
#' @format Tibble, including the following columns:
#' - `taxonID`: Unique identifier for the set of taxon information.
#' - `acceptedNameUsageID `: Identifier for the name usage (documented meaning
#' of the name according to a source) of the currently accepted taxon; maps to
#' `taxonID`.
#' - `taxonomicStatus`: The status of the use of the `scientificName` as a label
#' for a taxon.
#' - `scientificName`: The full scientific name, with authorship information.
#' - `namePublishedIn`: A reference for the publication in which the
#' `scientificName` was originally established under the rules of the
#' International Code of Nomenclature for algae, fungi, and plants
#' - `taxonRemarks`: Notes about the taxon.
#' @source \url{https://dwc.tdwg.org/terms/#taxon}
"pteridocat"
