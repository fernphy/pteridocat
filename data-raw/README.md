# data-raw README

This folder includes raw data files used to produce pteridocat.

## 2021-10-18_dwca.zip

[Catalog of Life](https://www.catalogueoflife.org/) taxonomic database in Darwin 
Core format, downloaded from https://download.catalogueoflife.org/col/monthly/2021-10-18_dwca.zip

## matched_fuzzy_inspected.csv

CSV file exported from [Fern Tree of Life (FTOL) project](https://github.com/joelnitta/ftol), 
then annotated manually. Includes fuzzily-matched names (`query`)
from searching fern names from the [NCBI Taxonomy database](https://www.ncbi.nlm.nih.gov/taxonomy)
against the previous version of pteridocat. Fuzzily-matched names need to be
inspected manually because they may include false-positives. Once verified,
these are incorporated into the next version of pteridocat. The first version of
pteridocat is all pteridophytes in the [Catalog of Life](https://www.catalogueoflife.org/),
which corresponds to the [World Ferns database](https://www.worldplants.de/world-ferns/ferns-and-lycophytes-list).

Includes the following columns:

- query: Named used as query input to resolve taxonomic names, from NCBI 
- matched_name: Name in existing pteridocat database matching the query
- matched_status: Taxonomic status of matched name
- query_match_taxon_agree: `TRUE` if canonical form of `query` and `matched_name` agree; `FALSE` otherwise
- dist: String distance between `query` and `matched_name`
- use_query_as_synonym: `1` to use the `query` as a synonym of `matched_name` in the next version of pteridocat; `0` if not
- use_query_as_accepted: `1` to use the `matched_name` as a synonym of `query` in the next version of pteridocat; `0` if not
- use_query_as_new: `1` to use the `query` as a new accepted name in the next version of pteridocat; `0` if not
- taxonomicStatus: Taxonomic status of the `query` if using in the next version of pteridocat
- namePublishedIn: A reference for the publication of the `query` if using in the next version of pteridocat
- nameAccordingTo: The reference to the source in which the specific taxon concept 
  circumscription is defined or implied for `query`, if using in the next version of pteridocat
- taxonRemarks: Additional remarks about `query`, if using in the next version of pteridocat
- notes: Additional notes not added to pteridocat

## new_names.csv

Additional taxonomic names to be added to the next version of pteridocat that were not
included in the fuzzy search results. Changes made to pteridocat based on the
fuzzy search results may impact other names in the database, but that were not
in the search results. For example, a previously accepted name may be changed to
the synonym of a query; if that name had synonyms pointing to it, those also
need to be changed to the query.

Includes the following columns:

- scientificName: Scientific name to use in in the next version of pteridocat
- usage_name: Scientific name to use in in the next version of pteridocat
- taxonomicStatus: Taxonomic status to use in in the next version of pteridocat
- namePublishedIn: A reference for the publication of the `scientificName` to use in in the next version of pteridocat
- nameAccordingTo: The reference to the source in which the specific taxon concept 
  circumscription is defined or implied for `scientificName` to use in in the next version of pteridocat
- taxonRemarks: Additional remarks about `scientificName` to use in the next version of pteridocat
- notes: Additional notes not added to pteridocat
- sci_name_in_fow: `1` if the `scientificName` is in the previous version of 
  pteridocat; `0` otherwise
- usage_name_in_fow: `1` if the `usage_name` is in the previous version of 
  pteridocat; `0` otherwise
 
