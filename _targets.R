source("R/packages.R")
source("R/functions.R")

# https://download.catalogueoflife.org/col/monthly/2021-12-18_dwca.zip

tar_plan(

	# Load raw data ----
	# Download Catalog of Life (COL)
	tar_url(
		col_dwca_url,
		"https://download.catalogueoflife.org/col/monthly/2021-12-18_dwca.zip"
	),
	# Load Ferns of the World (FOW) database from COL
	tar_file(
		col_dwca_zip,
		download_file(col_dwca_url, "data-raw/2021-12-18_dwca.zip")
	),
	fow = pcg_load_col(col_dwca_zip) %>% pcg_extract_fow(),
	# Write out FOW for FTOL
	tar_file(
		fow_out,
		write_tar_csv(fow, "results/2021-12-18-fow.csv")
	),
	# Load output of initial name matching from FTOL.
	# Includes one row per query/match pair
	tar_file(pterido_names_to_inspect_file, "data-raw/pterido_names_to_inspect.csv"),
	pterido_names_to_inspect = readr::read_csv(pterido_names_to_inspect_file),

	# Fetch taxonomic data (taxize) ----
	# Download data from tropicos for any FTOL fuzzily matched names or "no match" names
	pterido_tropicos = fetch_tropicos(pterido_names_to_inspect),
	# Convert to long format
	pterido_long = tidy_long_tropicos(pterido_names_to_inspect, pterido_tropicos),
	# Download data from IPNI for FTOL "no match" names that lacked any hits
	# in tropicos
	ipni_query = format_ipni_query(pterido_long),
	ipni_res = ipni_seach_names(ipni_query$query),
	# Download data from GBIF for FTOL "no match" names that lacked any hits
	# in tropicos or IPNI
	gbif_query = format_gbif_query(ipni_res),
	gbif_res = gbif_search_names(gbif_query$gbif_query),
	# Download data from POW for FTOL "no match" names that lacked any hits
	# in tropicos, IPNI, or GBIF
	pow_query = format_pow_query(ipni_res),
	pow_res = pow_search_names(pow_query$pow_query),

	# Tidy results ----
	# FTOL Fuzzily matched names
	pterido_fuzzy_tropicos = tidy_fuzzy_tropicos(pterido_names_to_inspect, pterido_tropicos),
	# FTOL no-match names
	pterido_no_match_taxized = tidy_ftol_no_match(
		pterido_long, ipni_res,
		gbif_res, gbif_query,
		pow_res, pow_query),
	# Combine all FTOL tidied and "taxized" names
	pterido_names_taxized = bind_rows(
		pterido_fuzzy_tropicos,
		filter(pterido_long, name_res_status == "mult_match"),
		pterido_no_match_taxized
	),
	# Write out
	tar_file(
		pterido_names_taxized_out,
		write_tar_csv(
			pterido_names_taxized,
			"data-raw/pterido_names_taxized_to_inspect.csv")
	),
	# Read back in data after manual inspection ----
	tar_file(
		pterido_names_taxized_inspected_file,
	  "data-raw/pterido_names_taxized_inspected.csv"
	),
	pterido_names_taxized_inspected = read_csv(pterido_names_taxized_inspected_file) %>%
		filter(done == 1),
	tar_file(
		new_names_file,
		"data-raw/new_names.csv"
	),
	new_names = read_csv(new_names_file) %>% lookup_taxon_id(fow)
)
