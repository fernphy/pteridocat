## code to prepare `pteridocat` dataset goes here

library(dwctaxon)

# Load Ferns of the World database from Catalog of Life
fow <- pcg_load_col(here::here("data-raw/2021-10-18_dwca.zip")) |>
	pcg_extract_fow()

# Modify Ferns of the World to produce pteridocat database
pteridocat <-
	fow |>
	# Add 'modified' column
	dplyr::mutate(modified = NA) |>
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
	dct_validate_tax_dat()

# Set encoding of columns with non-ascii characters to UTF-8
Encoding(pteridocat$scientificName) <- "UTF-8"
Encoding(pteridocat$namePublishedIn) <- "UTF-8"

usethis::use_data(pteridocat, overwrite = TRUE)
