#' Taxonomic database of Pteridophytes
#'
#' Taxonomic database of pteridophytes following
#' [Darwin Core standards for taxonomic data](https://dwc.tdwg.org/terms/#taxon).
#'
#' Taxonomic data of pteridophytes compiled by Hassler (2021) were downloaded
#' from the [Catalog of Life](https://www.catalogueoflife.org/)
#' and modified under the
#' [Creative Commons Attribution (CC BY) 4.0 license](http://creativecommons.org/licenses/by/4.0/).
#'
#' Description of data format modified from
#' [Biodiversity Information Standards (TDWG)](https://www.tdwg.org/) under the
#' [Creative Commons Attribution (CC BY) 4.0 license](https://creativecommons.org/licenses/by/4.0/).
#'
#' @format Dataframe (tibble), including the following columns:
#' - `taxonID`: Unique identifier for the set of taxon information.
#' - `parentNameUsageID`: Identifier for the name usage (documented meaning of
#' the name according to a source) of the direct, most proximate higher-rank
#' parent taxon (in a classification) of the most specific element of the
#' `scientificName` (i.e., parent taxon); maps to `taxonID`.
#' - `acceptedNameUsageID `: Identifier for the name usage of the currently
#' accepted taxon (i.e., accepted name of a synonym); maps to
#' `taxonID`.
#' - `taxonomicStatus`: The status of the use of the `scientificName` as a label
#' for a taxon.
#' - `taxonRank`: The taxonomic rank of the most specific name in the
#' `scientificName.`
#' - `scientificName`: The full scientific name, with authorship information.
#' - `genericName`: The genus part of the `scientificName` without authorship.
#' - `infragenericEpithet`: The infrageneric part of a binomial name at ranks
#' above species but below genus.
#' - `specificEpithet`: The name of the first or species epithet of the
#' `scientificName.`
#' - `infraspecificEpithet`: The name of the lowest or terminal infraspecific
#' epithet of the scientificName, excluding any rank designation.
#' - `namePublishedIn`: A reference for the publication in which the
#' `scientificName` was originally established under the rules of the
#' `nomenclaturalCode`.
#' - `nomenclaturalCode`: The nomenclatural code under which the
#' `scientificName` is constructed; typically, "ICN", for
#' International Code of Nomenclature for Algae, Fungi, and Plants.
#' - `nomenclaturalStatus`: The status related to the original publication of
#' the name and its conformance to the relevant rules of nomenclature.
#' - `taxonRemarks`: Notes about the taxon.
#' - `references`: 	A related resource that is referenced, cited, or otherwise
#' pointed to by the described resource.
#' - `modified`: The most recent date-time on which the information was changed.
#' (NA for data that were imported from Catalog of Life and not modified).
#' - `nameAccordingTo`: The reference to the source in which the specific taxon
#' concept circumscription is defined or implied.
#' @source Hassler, M. (2021). Checklist of Ferns and Lycophytes of the World.
#'   In O. Bánki, Y. Roskov, L. Vandepitte, R. E. DeWalt, D. Remsen, P. Schalk,
#'   T. Orrell, M. Keping, J. Miller, R. Aalbu, R. Adlard, E. Adriaenssens, C.
#'   Aedo, E. Aescht, N. Akkari, M. A. Alonso-Zarazaga, B. Alvarez, F. Alvarez,
#'   G. Anderson, et al., Catalogue of Life Checklist (Version 2021-08-06).
#'   <https://doi.org/10.48580/d4sd-3dc>
"pteridocat"
