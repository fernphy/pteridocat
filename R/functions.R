# Data loading ----

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

# tropicos ----

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


#' Fetch data from tropicos for fuzzily matched names
#'
#' Converts to long format. Some taxa names have multiple hits, so will
#' increase number of rows compared to input.
#'
#' @param fuzzy_to_inspect Dataframe; output of fuzzy name matching from FTOL
#' @return Dataframe (tibble)
fetch_trop_fuzzy <- function(fuzzy_to_inspect) {

	# Setup tropicos query in long form
	names_to_lookup <-
		fuzzy_to_inspect |>
		select(query, matched_name, query_match_taxon_agree) %>%
		# add 'set' to group query/match pairs
		mutate(set = 1:nrow(.)) |>
		pivot_longer(names_to = "type", values_to = "sci_name", -c(set, query_match_taxon_agree)) |>
		# parse canonical name from sci name for querying tropicos
		mutate(
			rgnparser::gn_parse_tidy(sci_name) %>%
				select(taxon = canonicalfull)
		)

	# Query tropicos (takes 5 min-ish for 900 names)
	tp_res <- names_to_lookup |>
		pull(taxon) |>
		unique() |>
		tp_search_sci()

	# Query tropicos again for some additional detailed data
	# source = tropicos webpage URL
	# namepublishedcitation = full citation for name
	tp_res_details <-
		tp_res %>%
		filter(!is.na(nameid)) %>%
		pull(nameid) %>%
		unique() %>%
		tp_summary_ids() %>%
		select(nameid, source, namepublishedcitation)

	# Add query results back in to original data
	names_to_lookup |>
		left_join(tp_res, by = c(taxon = "query")) |>
		left_join(tp_res_details, by = "nameid")

}

#' Categorize results of tropicos query on FTOL fuzzy match results
#'
#' Adds `trop_cat` column with three values
#'   - "mult": More than 2 hits per query/match pair
#'   - "both_valid": Each member of query/match pair hits valid name
#'   - "single_valid": One member of query/match pair hits valid name
#'
#' @param fuzzy_tropicos Dataframe; output of fuzzy name matching from FTOL with
#'   tropicos data added
#' @return Dataframe (tibble)
categorize_fuzzy_trop <- function(fuzzy_tropicos) {

	fuzzy_tropicos %>%
		add_count(set) %>%
		verify(n >= 2) %>%
		mutate(name_valid = case_when(
			is.na(nameid) ~ FALSE,
			nomenclaturestatusname == "Illegitimate" ~ FALSE,
			nomenclaturestatusname == "Invalid" ~ FALSE,
			TRUE ~ TRUE
		)) %>%
		group_by(set) %>%
		mutate(
			n_valid_names = sum(name_valid)
		) %>%
		ungroup() %>%
		mutate(
			trop_cat = case_when(
				n > 2 ~ "mult",
				n == 2 & n_valid_names == 1 ~ "single_valid",
				n == 2 & n_valid_names > 1 ~ "both_valid",
				TRUE ~ NA_character_
			)
		) %>%
		select(-n, -n_valid_names, -name_valid)

}

#' Convert results of tropicos query on FTOL fuzzy match results to wide format
#'
#' Only converts results that have been categorized as "single_valid"
#'
#' @param fuzzy_tropicos_cat Dataframe; output of fuzzy name matching from FTOL with
#'   tropicos data added and categorized by result type (in long format)
#' @param fuzzy_to_inspect Dataframe; output of fuzzy name matching from FTOL
#'   (in wide format)
#' @return Dataframe (tibble) in wide format; output of fuzzy name matching from FTOL
#'   (in wide format) with additional data filled in from tropicos
#'
widen_valid_fuzzy_trop <- function(fuzzy_tropicos_cat, fuzzy_to_inspect) {

	single_valid_wide <-
	fuzzy_tropicos_cat %>%
		filter(trop_cat == "single_valid", !is.na(nameid)) %>%
		select(type, sci_name, namePublishedIn = namepublishedcitation, nameAccordingTo = source) %>%
		pivot_wider(names_from = type, values_from = sci_name) %>%
		mutate(
			query_match_taxon_agree = FALSE,
			taxonomicStatus = case_when(
				is.na(query) ~ "synonym",
				is.na(matched_name) ~ "accepted"
			),
			use_query_as_synonym = if_else(is.na(query), 1, 0),
			use_query_as_accepted = if_else(is.na(matched_name), 1, 0),
			use_query_as_new = 0,
			taxonRemarks = "spelling mistake in species name; fix according to Tropicos",
			notes = "automatically determined by pteridocat"
		) %>%
		# Fill in missing query, matched_name
		left_join(
			select(fuzzy_to_inspect, query, matched_name), by = "query", na_matches = "never"
		) %>%
		mutate(
			matched_name = coalesce(matched_name.x, matched_name.y)
		) %>%
		select(-c(matched_name.x, matched_name.y)) %>%
		left_join(
			select(fuzzy_to_inspect, query, matched_name, matched_status), by = "matched_name", na_matches = "never"
		) %>%
		mutate(
			query = coalesce(query.x, query.y)
		) %>%
		select(-c(query.x, query.y)) %>%
		unique() %>%
		assert(not_na, matched_name, query) %>%
		select(
			query, matched_name, matched_status, query_match_taxon_agree,
			use_query_as_synonym, use_query_as_accepted, use_query_as_new,
			taxonomicStatus, namePublishedIn, nameAccordingTo, taxonRemarks, notes)

	fuzzy_to_inspect %>%
		anti_join(single_valid_wide, by = c("query", "matched_name"), na_matches = "never") %>%
		bind_rows(single_valid_wide) %>%
		arrange(query_match_taxon_agree, query, matched_name)

}

# utils ----

# Write CSV and return the output path
write_tar_csv <- function(x, file, ...) {
	readr::write_csv(x = x, file = file, ...)
	file
}

