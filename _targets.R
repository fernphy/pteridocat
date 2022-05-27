source("_R/packages.R")
source("_R/functions.R")

# Set options:
# - Use targets workspaces for debugging
# - Track dependencies in some packages
tar_option_set(
  workspace_on_error = TRUE,
  imports = c("dwctaxon")
)

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
    download_file(col_dwca_url, "_targets/user/data-raw/2021-12-18_dwca.zip")
  ),
  fow_version = get_fow_version(col_dwca_zip),
  fow = pcg_load_col(col_dwca_zip) %>% pcg_extract_fow(),
  # Write out FOW for FTOL
  tar_file(
    fow_out,
    write_tar_rds(fow, "_targets/user/data-raw/fow.RDS")
  ),
  # Load output of initial name matching from FTOL.
  # (matching against FOW)
  # Includes one row per query/match pair
  tar_file_read(
    pterido_names_to_inspect,
    "_targets/user/data-raw/pterido_names_to_inspect_all.csv",
    readr::read_csv(file = !!.x)
  ),
  # Fetch taxonomic data (taxize) ----
  # Download data from tropicos for any FTOL fuzzily matched names or
  # "no match" names
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
  pterido_fuzzy_tropicos = tidy_fuzzy_tropicos(
    pterido_names_to_inspect, pterido_tropicos),
  # FTOL no-match names
  pterido_no_match_taxized = tidy_ftol_no_match(
    pterido_long, ipni_res,
    gbif_res, gbif_query,
    pow_res, pow_query),
  # Combine all FTOL tidied and "taxized" names
  pterido_names_taxized = combine_taxized_names(
    pterido_fuzzy_tropicos,
    pterido_long,
    pterido_no_match_taxized
  ),
  # Write out
  tar_file(
    pterido_names_taxized_out,
    write_tar_csv(
      pterido_names_taxized,
      "_targets/user/data-raw/pterido_names_taxized_to_inspect.csv")
  ),

  # Read back in data after manual inspection ----
  tar_file_read(
    pterido_names_taxized_inspected,
    "_targets/user/data-raw/pterido_names_taxized_inspected.csv",
    load_pterido_names_taxized_inspected(!!.x)
  ),
  tar_file(
    new_names_file,
    "_targets/user/data-raw/new_names.csv"
  ),
  new_names = read_csv(new_names_file) %>% lookup_taxon_id(fow),

  # Generate new pteridocat database ----
  # Update names after manual inspection
  fow_modified = modify_fow(fow),
  pteridocat = update_fow_names(
    pterido_names_taxized_inspected, new_names, fow_modified),
  # Write out pteridocat database for use in pteridocat package
  tar_file(
    pteridocat_in_pkg,
    use_pteridocat_data(pteridocat)
  )
)
