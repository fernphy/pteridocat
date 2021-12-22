source("R/packages.R")
source("R/functions.R")

tar_plan(
	# Load data ----
	# - Ferns of the World database from Catalog of Life
	tar_file(col_dwca_zip, "data-raw/2021-10-18_dwca.zip"),
	fow = pcg_load_col(col_dwca_zip) |> pcg_extract_fow(),
	# - Output of fuzzy name matching from FTOL
	#   Includes one row per query/match pair
	tar_file(matched_fuzzy_from_ftol, "data-raw/matched_fuzzy_to_inspect.csv"),
	fuzzy_to_inspect = readr::read_csv(matched_fuzzy_from_ftol),
	# Add data from tropicos ----
	# Download data
	fuzzy_tropicos = fetch_trop_fuzzy(fuzzy_to_inspect),
	# Categorize results
	# Adds `trop_cat` column with three values:
	# - "mult": More than 2 tropicos hits per query/match pair
	# - "both_valid": Each member of query/match pair hits valid name
	# - "single_valid": One member of query/match pair hits valid name
	fuzzy_tropicos_cat = categorize_fuzzy_trop(fuzzy_tropicos),
	# Convert "single_valid" names to wide format
	fuzzy_tropicos_resolved = widen_valid_fuzzy_trop(
		fuzzy_tropicos_cat, fuzzy_to_inspect),
	# Write out fuzzy name matching from FTOL + data from tropicos
  tar_file(
  	fuzzy_tropicos_resolved_out,
  	write_tar_csv(
  		fuzzy_tropicos_resolved,
  		"data-raw/matched_fuzzy_to_inspect_trop.csv")
  	)
)
