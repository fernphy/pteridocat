## code to prepare `pteridocat` dataset goes here

library(dwctaxon)
library(tidyverse)
library(assertr)
library(pteridocat)

# Load data ----

# Load Ferns of the World database from Catalog of Life
fow <- pcg_load_col(here::here("data-raw/2021-10-18_dwca.zip")) |>
	pcg_extract_fow()

# Load fuzzily matched names from Fern Tree of Life project,
fuzzy_updates <- readr::read_csv(here::here("data-raw/matched_fuzzy_inspected.csv")) |>
	# filter to only those inspected (notes should be "ok" or something else non-NA)
	filter(!is.na(notes))

# Load other data of new names to add
other_names <- readr::read_csv(here::here("data-raw/new_names.csv")) |>
	# Add new taxonID for new names, lookup for already existing names
	mutate(
		taxonID = case_when(
			!scientificName %in% fow$scientificName ~ purrr::map_chr(scientificName, digest::digest),
			TRUE ~ NA_character_
		)
	) |>
	left_join(
		select(fow, taxonID, scientificName), by = "scientificName"
	) |>
	mutate(taxonID = coalesce(taxonID.x, taxonID.y)) |>
	select(-taxonID.x, -taxonID.y) |>
	assert(not_na, scientificName, taxonID) |>
	assert(is_uniq, scientificName, taxonID)

# Format names ----

# Format names to add as accepted (all new names)
names_add_as_accepted <-
	fuzzy_updates |>
	filter(use_query_as_accepted == 1 | use_query_as_new == 1) |>
	select(scientificName = query, taxonomicStatus, namePublishedIn, nameAccordingTo) |>
	unique() |>
	bind_rows(
		filter(other_names, taxonomicStatus == "accepted") |>
			select(scientificName, taxonomicStatus, namePublishedIn, nameAccordingTo, taxonRemarks)
	) |>
	mutate(
		taxonID = purrr::map_chr(scientificName, digest::digest),
		modified = as.character(Sys.time())
	) |>
	assert(is_uniq, scientificName, taxonID)

# Make tibble of all existing names after addition
fow_plus_new <-
	fow |>
	bind_rows(names_add_as_accepted) |>
	dct_validate()

# Format names to add as synonyms (all new names)
names_add_as_synonym <-
	fuzzy_updates |>
	filter(use_query_as_synonym == 1) |>
	rename(scientificName = query, usage_name = matched_name) |>
	bind_rows(
		filter(other_names, taxonomicStatus == "synonym")
	) |>
	select(scientificName, taxonomicStatus, namePublishedIn, nameAccordingTo, taxonRemarks, usage_name) |>
	# Filter to only new synonyms
	anti_join(fow, by = "scientificName") |>
	# Map acceptedNameUsageID from usage_name
	left_join(
		select(fow, acceptedNameUsageID = taxonID, usage_name = scientificName),
		by = "usage_name"
	) |>
	left_join(
		select(names_add_as_accepted, acceptedNameUsageID = taxonID, usage_name = scientificName),
		by = "usage_name"
	) |>
	mutate(acceptedNameUsageID = coalesce(acceptedNameUsageID.x, acceptedNameUsageID.y)) |>
	select(-acceptedNameUsageID.x, -acceptedNameUsageID.y) |>
	# Create `taxonID` and `modified` columns
	mutate(
		taxonID = purrr::map_chr(scientificName, digest::digest),
		modified = as.character(Sys.time())
	) |>
	select(-usage_name) |>
	verify(taxonomicStatus == "synonym") |>
	# Fix nested synonyms: those where the usage_name maps to a name that's
	# already a synonym. Need to map back to the original acceptedNameUsageID
	left_join(
		select(fow, taxonID, acceptedNameUsageID_2 = acceptedNameUsageID),
		by = c(acceptedNameUsageID = "taxonID")) |>
	mutate(
		acceptedNameUsageID = case_when(
			is.na(acceptedNameUsageID_2) ~ acceptedNameUsageID,
			!is.na(acceptedNameUsageID_2) ~ acceptedNameUsageID_2
		)
	) |>
	select(-acceptedNameUsageID_2) |>
	# Check everything is OK
	assert(not_na, scientificName, taxonID) |>
	assert(is_uniq, scientificName, taxonID)

# Format names already in FOW to change status to synonym
names_change_to_synonym <-
	# Format existing names to change matched names to synonym of query
	fuzzy_updates |>
	filter(use_query_as_accepted == 1) |>
	transmute(usage_name = query, sci_name = matched_name) |>
	mutate(new_status = "synonym") |>
	select(sci_name, usage_name, new_status) |>
	bind_rows(
		filter(other_names, taxonomicStatus == "synonym", scientificName %in% fow$scientificName) |>
			select(sci_name = scientificName, usage_name, new_status = taxonomicStatus)
	) |>
	verify(sci_name %in% fow$scientificName) |>
	assert(is_uniq, sci_name) |>
	assert(not_na, everything())

# Modify Ferns of the World to produce pteridocat database ----
pteridocat <-
	fow |>
	bind_rows(names_add_as_accepted) |>
	bind_rows(names_add_as_synonym) |>
	dct_change_status(args_tbl = names_change_to_synonym) |>
	dct_validate() |>
	# not synonym
	dct_change_status(
		sci_name = "Deparia petersenii var. yakusimensis",
		new_status = "accepted") |>
	# not synonym
	dct_change_status(
		sci_name = "Dryopteris schnellii Tardieu",
		new_status = "accepted") |>
	# orthographic variant:
	# Asplenium richardii Hook.f. -> Asplenium richardii (Hook. fil.) Hook. fil.
	dct_add_row(
		sci_name = "Asplenium richardii Hook.f.",
		tax_status = "synonym", usage_id = "68782") |>
	# orthographic variant:
	# Davallia tasmani Field -> Davallia tasmanii Field
	dct_add_row(
		sci_name = "Davallia tasmani Field",
		tax_status = "synonym", usage_id = "6CG7Q") |>
	# orthographic variant:
	# Dryopteris amurensis H.Christ -> Dryopteris amurensis (Milde) Christ
	dct_add_row(
		sci_name = "Dryopteris amurensis H.Christ",
		tax_status = "synonym", usage_id = "6DMKB") |>
	# not synonym
	dct_change_status(
		sci_name = "Dryopteris spinosa Copel.",
		new_status = "accepted") |>
	# orthographic variant:
	# Asplenium oblongifolium Col. -> Asplenium oblongifolium Colenso
	dct_add_row(
		sci_name = "Asplenium oblongifolium Col.",
		tax_status = "synonym", usage_id = "HL2X") |>
	# not synonym
	dct_change_status(
		sci_name = "Dryopteris triangularis Herter",
		new_status = "accepted") |>
	# not synonym
	dct_change_status(
		sci_name = "Stenochlaena tenuifolia (Desv.) Moore",
		new_status = "accepted") |>
	# orthographic variant:
	# Dryopteris sabae (Franch. & Sav.) C.Chr. -> Dryopteris sabaei (Franch. & Sav.) C. Chr. #nolint
	dct_add_row(
		sci_name = "Dryopteris sabae (Franch. & Sav.) C.Chr.",
		tax_status = "synonym", usage_id = "37XLD") |>
	# not synonym:
	# Davallia polypodioides (Sw.) D. Don, not Microlepia polypodioides (Sw.) C. Presl #nolint
	dct_change_status(
		sci_name = "Davallia polypodioides var. rhomboidea Hook.",
		new_status = "accepted"
	) |>
	dct_change_status(
		sci_name = "Davallia polypodioides var. pilosula C. B. Clarke",
		new_status = "accepted"
	) |>
	dct_change_status(
		sci_name = "Davallia polypodioides var. strigosa (Thunb.) C. B. Clarke",
		new_status = "accepted"
	) |>
	dct_change_status(
		sci_name = "Davallia polypodioides var. hirta (Kaulf.) C. B. Clarke",
		new_status = "accepted"
	) |>
	dct_change_status(
		sci_name = "Davallia polypodioides var. subglabra Hook.",
		new_status = "accepted"
	) |>
	dct_change_status(
		sci_name = "Davallia polypodioides var. pubescens Hook.",
		new_status = "accepted"
	) |>
	dct_change_status(
		sci_name = "Davallia polypodioides D. Don",
		new_status = "accepted"
	) |>
	dct_add_row(
		sci_name = "Davallia polypodioides (Sw.) D.Don",
		tax_status = "synonym", usage_id = "34BS9"
	) |>
	# Run final check
	dct_validate()

# Set encoding of columns with non-ascii characters to UTF-8
Encoding(pteridocat$scientificName) <- "UTF-8"
Encoding(pteridocat$namePublishedIn) <- "UTF-8"

usethis::use_data(pteridocat, overwrite = TRUE)
