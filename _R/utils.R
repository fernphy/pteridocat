#' Extract Ferns of the World taxonomic data from Catalog of Life
#'
#' Also do some quality checks, and filter to only data at species level or below
#'
#' @param col_data Catalog of Life data. Output of of \code{\link{pcg_load_col}()}.
#'
#' @return Dataframe (tibble)
#' @autoglobal
#' @export
pcg_extract_fow <- function(col_data) {
	# Filter Catalog of Life data to only Ferns of the World
	col_data |>
		dplyr::filter(`dwc:datasetID` == "1140") |>
		dplyr::rename_with(~stringr::str_remove_all(., "dwc:|dcterms:")) |>
		# Replace empty strings with NA
		dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::na_if(., ""))) |>
		# Drop empty columns, unused columns
		janitor::remove_empty("cols") |>
		dplyr::select(-datasetID, -scientificNameID) |>
		# Keep only species level and below
		# FIXME: eventually add higher ranks back making sure they conform to PPGI
		dplyr::filter(taxonRank %in% c("form", "infraspecific name", "species", "subform", "subspecies", "subvariety", "variety")) |>
		# Filter some names that were incorrectly labeled species level
		dplyr::filter(stringr::str_detect(scientificName, "Polypodiaceae tribe Thelypterideae|Asplenium grex Triblemma|Pteridaceae tribus Platyzomateae|Filicaceae tribus Taenitideae", negate = TRUE)) |>
		# Note: taxonID is unique, but scientificName may not be (esp in case of ambiguous synonyms)
		assertr::assert(assertr::not_na, taxonID, scientificName) |>
		assertr::assert(assertr::is_uniq, taxonID) |>
		# Fix scientific names that are both synonyms and accepted names
		dplyr::filter(
			!taxonID %in% c(
				"392HN", # Elaphoglossum rigidum (Aubl.) Urb. -> exclude ambiguous synonym
				"392HM", # Elaphoglossum rigidum (Aubl.) Urb. -> exclude ambiguous synonym
				"3926G", # Elaphoglossum killipii Mickel -> exclude ambiguous synonym (per Tropicos)
				"392K8", # Elaphoglossum serpentinum A. Rojas & W. D. Rodr. -> exclude ambiguous synonym
				"HK2Q", # Asplenium centrifugale Baker -> exclude ambiguous synonym
				"36J4J", # Diplazium wangii Ching -> exclude ambiguous synonym
				"36HP3", # Diplazium lonchophyllum Kunze -> exclude ambiguous synonym
				"3GXD3" # Goniopteris hastata Fée -> exclude ambiguous synonym
			)
		) |>
		# Run taxonomic database checks
		dwctaxon::dct_validate()
}

#' Load data from the Catalog of Life
#'
#' Catalog of Life data can be downloaded from here:
#' https://download.catalogueoflife.org/col/monthly/. The "_dwca" format should
#' be selected when downloading (files ending in "_dwca.zip").
#'
#' If `col_data_path` ends in "zip", the function will try to automatically
#' extract the taxonomic names data, which is typically named "Taxon.tsv".
#'
#' @param col_data_path Path to data downloaded from the Catalog of Life; either
#' the original zip file, or the tsv file containing the taxonomic data extracted
#' from the zip file (usually named "Taxon.tsv").
#'
#' @return Dataframe (tibble)
#' @autoglobal
#' @export
pcg_load_col <- function(col_data_path) {
	assertthat::assert_that(fs::file_exists(col_data_path))
	file_ending <- fs::path_ext(col_data_path)

	# Try to unzip Taxon.tsv
	temp_dir <- tempdir()
	temp_tsv <- ""
	if (file_ending == "zip") {
		tryCatch(
			expr = utils::unzip(col_data_path, files = "Taxon.tsv", overwrite = TRUE, exdir = temp_dir),
			finally = "Could not find 'Taxon.tsv' in zip file. Try manually unzipping."
		)
		temp_tsv <- fs::path(temp_dir, "Taxon.tsv")
		col_data_path <- temp_tsv
	}

	# Use data.table as it handles quotation marks in data better than read_*() functions
	res <- data.table::fread(file = col_data_path, sep = "\t", stringsAsFactors = FALSE) |>
		tibble::as_tibble()

	# Clean up
	if (file_ending == "zip" && fs::file_exists(temp_tsv)) fs::file_delete(temp_tsv)

	res

}

#' Search tropicos and return results with query appended
#'
#' Thin wrapper around taxize::tp_search()
#'
#' @param sci String (character vector of length 1);
#' Scientific name for querying tropicos
#'
#' @return Dataframe with original query appended in column "query"
#' @autoglobal
#'
tp_search_add_q <- function(sci) {
	taxize::tp_search(sci = sci) |>
		dplyr::mutate(query = sci)
}

#' Search tropicos and return results with query appended
#'
#' Wrapper around taxize::tp_search(). taxize::tp_search() doesn't allow
#' for character vectors with length > 1; this function does.
#'
#' @param sci Character vector: one or more scientific name(s)
#' for querying tropicos
#'
#' @return Dataframe (tibble) with original query appended in column "query"
#' @autoglobal
#' @export
#'
#' @examples
#' tp_search_sci(c("Crepidomanes minutum", "Crepidomanes bipunctatum"))
tp_search_sci <- function(sci) {
	purrr::map_df(sci, ~tp_search_add_q(sci = .)) |>
		tibble::as_tibble()
}

#' Return summary data by tropicos taxon id
#'
#' Wrapper around taxize::tp_summary(). taxize::tp_summary() doesn't allow
#' for character vectors with length > 1; this function does.
#'
#' @param id Character vector; one or more taxon identifier code(s)
#'
#' @return Dataframe (tibble)
#' @autoglobal
#' @export
#'
#' @examples
#' tp_summary_ids(c("26602584", "26603638"))
tp_summary_ids <- function(id) {
	purrr::map_df(id, ~taxize::tp_summary(id = .)) |>
		tibble::as_tibble()
}
