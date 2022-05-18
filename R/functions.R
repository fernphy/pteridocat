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
  res <- data.table::fread(file = col_data_path, sep = "\t", stringsAsFactors = FALSE) %>%
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
  col_data %>%
    dplyr::filter(`dwc:datasetID` == "1140") %>%
    dplyr::rename_with(~stringr::str_remove_all(., "dwc:|dcterms:")) %>%
    # Replace empty strings with NA
    dplyr::mutate(dplyr::across(dplyr::everything(), ~dplyr::na_if(., ""))) %>%
    # Drop empty columns, unused columns
    janitor::remove_empty("cols") %>%
    dplyr::select(-datasetID, -scientificNameID) %>%
    # Keep only species level and below
    # FIXME: eventually add higher ranks back making sure they conform to PPGI
    dplyr::filter(taxonRank %in% c("form", "infraspecific name", "species", "subform", "subspecies", "subvariety", "variety")) %>%
    # Filter some names that were incorrectly labeled species level
    dplyr::filter(stringr::str_detect(scientificName, "Polypodiaceae tribe Thelypterideae|Asplenium grex Triblemma|Pteridaceae tribus Platyzomateae|Filicaceae tribus Taenitideae", negate = TRUE)) %>%
    # Note: taxonID is unique, but scientificName may not be (esp in case of ambiguous synonyms)
    assertr::assert(assertr::not_na, taxonID, scientificName) %>%
    assertr::assert(assertr::is_uniq, taxonID) %>%
    # Fix scientific names that are both synonyms and accepted names
    dplyr::filter(
      !taxonID %in% c(
        "36HP3", # Diplazium lonchophyllum Kunze -> exclude ambiguous synonym
        "36J4J", # Diplazium wangii Ching -> exclude ambiguous synonym
        "3926G", # Elaphoglossum killipii Mickel -> exclude ambiguous synonym (per Tropicos)
        "392HM", # Elaphoglossum rigidum (Aubl.) Urb. -> exclude ambiguous synonym
        "392HN", # Elaphoglossum rigidum (Aubl.) Urb. -> exclude ambiguous synonym
        "392K8", # Elaphoglossum serpentinum A. Rojas & W. D. Rodr. -> exclude ambiguous synonym
        "8S7SM", # Goniopteris hastata Fée -> exclude ambiguous synonym
        "HK2Q" # Asplenium centrifugale Baker -> exclude ambiguous synonym
      )
    ) %>%
    # Run taxonomic database checks
    dwctaxon::dct_validate()
}

#' Get version for Ferns of the World taxonomic data in the Catalog of Life
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
#' @return String with version of Ferns of the World data in Catalog of Life
#'
get_fow_version <- function(col_data_path) {

  assertthat::assert_that(fs::file_exists(col_data_path))
  file_ending <- fs::path_ext(col_data_path)

  # Try to unzip dataset/1140.xml (XML data matching Ferns of the World)
  temp_dir <- tempdir()
  temp_xml <- ""
  if (file_ending == "zip") {
    tryCatch(
      expr = utils::unzip(
        col_data_path,
        files = "dataset/1140.xml",
        overwrite = TRUE, exdir = temp_dir, junkpaths  = TRUE),
      finally = "Could not find '1140.xml' in zip file. Try manually unzipping."
    )
    temp_xml <- fs::path(temp_dir, "1140.xml")
    col_data_path <- temp_xml
  }

  # Extract FOW title and version
  title <-
    col_data_path %>%
    xml2::read_xml() %>%
    xml2::as_list() %>%
    purrr::pluck("eml", "dataset", "title", 1)

  assertthat::assert_that(
    title == "Checklist of Ferns and Lycophytes of the World",
    msg = "Dataset title is not 'Checklist of Ferns and Lycophytes of the World'"
  )

  version <-
    col_data_path %>%
    xml2::read_xml() %>%
    xml2::as_list() %>%
    purrr::pluck("eml", "additionalMetadata", "metadata", "col", "version", 1)

  # Clean up
  if (file_ending == "zip" && fs::file_exists(temp_xml)) fs::file_delete(temp_xml)

  version

}

#' Load inspected taxonomic names data
#'
#' @param pterido_names_taxized_inspected_file Path to inspected taxonomic
#' names data
#'
#' @return Tibble
#'
load_pterido_names_taxized_inspected <- function(
  pterido_names_taxized_inspected_file) {
  read_csv(pterido_names_taxized_inspected_file) %>%
    # Filter to only names marked as inspected
    assert(within_bounds(0,1), done) %>%
    filter(done == 1) %>%
    # Make sure the "use_query_" columns don't sum to > 1 across each row
    mutate(across(contains("use_query_"), ~replace_na(., 0))) %>%
    assert_rows(rowSums, within_bounds(0,1), contains("use_query_")) %>%
    # Make sure the "use_match_" columns don't sum to > 1 across each row
    mutate(across(contains("use_match_"), ~replace_na(., 0))) %>%
    assert_rows(rowSums, within_bounds(0,1), contains("use_match_"))
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
  taxize::tp_search(sci = sci) %>%
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
  purrr::map_df(sci, ~tp_search_add_q(sci = .)) %>%
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
  purrr::map_df(id, ~taxize::tp_summary(id = .)) %>%
    tibble::as_tibble()
}

#' Fetch data from tropicos for fuzzily matched names
#'
#' Converts to long format. Some taxa names have multiple hits, so will
#' increase number of rows compared to input.
#'
#' @param names_to_inspect Dataframe; output of fuzzy name matching from FTOL
#' @return Dataframe (tibble)
fetch_tropicos <- function(names_to_inspect) {

  # Setup tropicos query in long form
  names_to_lookup <-
    names_to_inspect %>%
    select(query, matched_name, query_match_taxon_agree) %>%
    # add 'set' to group query/match pairs
    mutate(set = 1:nrow(.)) %>%
    pivot_longer(names_to = "type", values_to = "sci_name", -c(set, query_match_taxon_agree)) %>%
    filter(!is.na(sci_name)) %>%
    # parse canonical name from sci name for querying tropicos
    mutate(
      gn_parse_tidy(sci_name) %>%
        select(taxon = canonicalsimple) # drops "var." etc from names
    ) %>%
    # if variety and specific epithet are the same, drop the variety
    separate(taxon, into = c("genus", "specific_epithet", "variety"), sep = " ", fill = "right", remove = FALSE) %>%
    mutate(taxon = case_when(
      is.na(variety) ~ taxon,
      variety == specific_epithet ~ paste(genus, specific_epithet),
      TRUE ~ taxon
    )) %>%
    select(-genus, -specific_epithet, -variety)

  # Query tropicos (takes 5 min-ish for 900 names)
  # tropicos doesn't allow infraspecific rank in name ("var." etc)
  # if variety and specific epithet are same, does better only using
  # genus + specific epipthet
  tp_res <- names_to_lookup %>%
    pull(taxon) %>%
    unique() %>%
    sort() %>%
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
  names_to_lookup %>%
    left_join(tp_res, by = c(taxon = "query")) %>%
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

#' Categorize results of taxastand matching query
#'
#' @param ts_results taxastand matching query, already filtered
#' to either fuzzy matches or queries with no match
#'
#' @return Dataframe (tibble); `ts_results` with `name_res_status` column
#' added, including values
#' - `no_match`: Query name had no matches in reference database
#' - `mult_match`: Query name had multiple matches in reference database
#' - `fuzzy`: Query name had single, fuzzy match to reference database
#'
categorize_fuzzy_taxastand <- function(ts_results) {
  ts_results %>%
    add_count(query) %>%
    mutate(
      name_res_status = case_when(
        is.na(matched_name) ~ "no_match",
        n > 1 ~ "mult_match",
        n == 1 ~ "fuzzy",
        TRUE ~ NA_character_
      )
    ) %>%
    assert(not_na, name_res_status) %>%
    select(-n)
}

# IPNI ----

#' Search IPNI for taxonomic data for a single name
#'
#' @param query String; single scientific name to query
#'
#' @return Tibble with columns `query`, `id` (IPNI id), `version` (IPNI version),
#' `matched_name`, `reference`.
#'
#' @example
#' ipni_search_taxon("Crepidomanes minutum")
#' ipni_search_taxon("Foo bar")
#'
ipni_search_taxon <- function(query) {

  # Early exit for empty query
  if (is.na(query) | query == "") {
    res <- tibble(
      query = query,
      id = NA_character_,
      version = NA_character_,
      matched_name = NA_character_
    )
    return(res)
  }

  # Parse name
  query_parsed <-
    tibble(query = query) %>%
    mutate(gn_parse_tidy(query) %>% select(canonicalsimple)) %>%
    separate(
      canonicalsimple,
      c("genus", "species", "infraspecies"),
      sep = " ",
      fill = "right")

  # Search IPNI
  res <- ipni_search(
    genus = null_if_na(query_parsed$genus),
    species = null_if_na(query_parsed$species),
    infraspecies = null_if_na(query_parsed$infraspecies),
    output = "extended"
  )

  if (nrow(res) > 0) {
    # Format output
    res <- res %>%
      transmute(
        query = query,
        id = as.character(id),
        version = as.character(version),
        matched_name = full_name, reference)
  } else {
    # Return empty tibble if no results
    res <- tibble(
      query = query,
      id = NA_character_,
      version = NA_character_,
      matched_name = NA_character_
    )
  }

  res
}

#' Search IPNI for taxonomic data
#'
#' @param query Character vector; scientific name(s) to query
#'
#' @return Tibble with columns `query`, `id` (IPNI id), `version` (IPNI version),
#' `matched_name`, `reference`.
#'
#' @examples
#' ipni_seach_names(c("Crepidomanes minutum", "Foo bar"))
#'
ipni_seach_names <- function(query) {
  purrr::map_df(query, ipni_search_taxon)
}

# GBIF ----

#' Search GBIF for taxonomic data for a single name
#'
#' @param query String; single scientific name to query
#'
#' @return Tibble with columns `query`, `matched_status`,
#' `matched_name`, `publishedIn`, `nameAccordingTo` (GBIF taxon ID)
#'
#' @example
#' gbif_search_taxon("Crepidomanes minutum")
#' gbif_search_taxon("Foo bar")
#'
gbif_search_taxon <- function(query) {

  gbif_data <- taxize::gbif_name_usage(name = query)

  empty_results <- tibble(
    query = query,
    matched_status = NA,
    matched_name = NA,
    publishedIn = NA,
    nameAccordingTo = NA,
  )

  if (length(gbif_data$results) == 0) return(empty_results)

  # GBIF taxonomic search includes many sources
  # (GBIF backbone, tropicos, POWO, World Flora, etc)
  # Just subset to GBIF backbone data
  gbif_name_data_id <-
    gbif_data$results %>%
    transpose() %>%
    magrittr::extract2("taxonID") %>%
    str_detect("gbif\\:") %>%
    which() %>%
    # If multiple hits, just take first one
    first()

  if (length(gbif_name_data_id) == 0) return(empty_results)

  tibble(
    query = query,
    matched_status = gbif_data$results[[gbif_name_data_id]]$taxonomicStatus,
    matched_name = gbif_data$results[[gbif_name_data_id]]$scientificName,
    namePublishedIn = gbif_data$results[[gbif_name_data_id]]$publishedIn,
    nameAccordingTo = gbif_data$results[[gbif_name_data_id]]$taxonID,
  )

}

#' Search GBIF for taxonomic data
#'
#' @param query Character vector; scientific name(s) to query
#'
#' @return Tibble with columns `query`, `matched_status`,
#' `matched_name`, `publishedIn`, `nameAccordingTo` (GBIF taxon ID)
#'
#' @examples
#' gbif_search_names(c("Crepidomanes minutum", "Foo bar"))
#'
gbif_search_names <- function(query) {
  map_df(query, gbif_search_taxon)
}

# POW ----

# Get a single Plants of the World ID from a query string, as a tibble
get_pow_id <- function(query) {
  pow_id <- get_pow(sci_com = query)
  tibble(
    id = pow_id,
    url = attr(pow_id, "uri")
  )
}

# Search Plants of the World for taxonomic data for a single ID
pow_search_single <- function(id) {
  # Early exit with empty tibble if input is NA
  if (is.na(id)) return(tibble(
    modified = NA,
    matched_name = NA,
    reference = NA,
    taxonomicStatus = NA
  ))
  pow_data <- pow_lookup(id)
  tibble(
    modified = pow_data$meta$modified,
    matched_name = paste(pow_data$meta$name, pow_data$meta$authors),
    reference = pow_data$meta$reference,
    taxonomicStatus = pow_data$meta$taxonomicStatus
  )
}

# Search Plants of the World for taxonomic data from multiple IDs
pow_search_ids <- function(id) {
  purrr::map_df(id, pow_search_single)
}

# Search Plants of the World for taxonomic data from scientific names
#'
#' @param query Character vector; scientific name(s) to query
#'
#' @return Tibble with columns `query`, `id` (POW id), `url`,
#' `modified`, `matched_name`, `reference`, `taxonomicStatus`.
#'
#' @examples
#' pow_search_names(c("Crepidomanes minutum", "Foo bar"))
#'
pow_search_names <- function(query) {
  tibble(query = query) %>%
    mutate(get_pow_id(query)) %>%
    mutate(pow_search_ids(id)) %>%
    # Convert all columns to character
    mutate(across(everything(), as.character))
}

# Wrangling ----

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
      namePublishedIn, nameAccordingTo, taxonRemarks, notes)

  fuzzy_to_inspect %>%
    anti_join(single_valid_wide, by = c("query", "matched_name"), na_matches = "never") %>%
    bind_rows(single_valid_wide) %>%
    arrange(query_match_taxon_agree, query, matched_name)

}

#' Add new taxonID for new names, lookup for already existing names
#'
#' If the name already exists in `fow`, the `taxonID` will be looked up from
#' there. If not, a new `taxonID` will be generated automatically.
#'
#' @param new_names Dataframe; new names to add to pteridocat
#' @param fow Dataframe; existing Ferns of the World data
#'
#' @return Dataframe
#'
lookup_taxon_id <- function(new_names, fow) {
  new_names %>%
    mutate(
      taxonID = case_when(
        !scientificName %in% fow$scientificName ~ purrr::map_chr(
          scientificName, digest::digest),
        TRUE ~ NA_character_
      )
    ) %>%
    left_join(
      select(fow, taxonID, scientificName), by = "scientificName"
    ) %>%
    mutate(taxonID = coalesce(taxonID.x, taxonID.y)) %>%
    select(-taxonID.x, -taxonID.y) %>%
    assert(not_na, scientificName, taxonID) %>%
    assert(is_uniq, scientificName, taxonID)
}

#' Tidy output of searching tropicos for fuzzily matched names
#'
#' Automatically assigns values for how to handle name in next round of
#' pteridocat generation ()
#'
#' @param pterido_names_to_inspect Dataframe; output of name matching from FTOL
#' @param pterido_tropicos Dataframe; output of fuzzy name matching from FTOL with
#'   tropicos data added
#'
#' @return Tibble with values for columns `use_query_as_synonym`,
#' `use_query_as_accepted`, `use_query_as_new` assigned (0 or 1)
#'
tidy_fuzzy_tropicos <- function(pterido_names_to_inspect, pterido_tropicos) {

  # add taxastand categories: 'fuzzy', 'mult_match', 'no_match'
  # in `name_res_status` column
  pterido_names_to_inspect_categorized <-
    categorize_fuzzy_taxastand(pterido_names_to_inspect)

  pterido_names_to_inspect_categorized %>%
    filter(name_res_status == "fuzzy") %>%
    select(sci_name = query) %>%
    # since sci_name is query, first filter by that, then set
    inner_join(pterido_tropicos, by = "sci_name") %>%
    select(set) %>%
    inner_join(pterido_tropicos, by = "set") %>%
    # automatically resolve based on tropicos results
    categorize_fuzzy_trop %>%
    # convert to wide format
    widen_valid_fuzzy_trop(
      filter(pterido_names_to_inspect_categorized, name_res_status == "fuzzy")
    ) %>%
    # fill back in name_res_status, which is NA in some cells after widening
    mutate(name_res_status = "fuzzy") %>%
    arrange(query_match_taxon_agree, query, matched_name)

}

#' Tidy output of searching tropicos for multiple matched or non-matching names
#' from FTOL to long format
#'
#' @param pterido_names_to_inspect Dataframe; output of name matching from FTOL
#' @param pterido_tropicos Dataframe; output of name matching from FTOL with
#'   tropicos data added
#'
#' @return Tibble; names in long format with candidate matches from tropicos added
#'
tidy_long_tropicos <- function(pterido_names_to_inspect, pterido_tropicos) {

  mult_match <-
    pterido_names_to_inspect %>%
    categorize_fuzzy_taxastand() %>%
    filter(name_res_status == "mult_match") %>%
    arrange(query, desc(query_match_taxon_agree), matched_name)

  no_match <-
    pterido_names_to_inspect %>%
    categorize_fuzzy_taxastand() %>%
    filter(name_res_status == "no_match") %>%
    select(sci_name = query, name_res_status) %>%
    inner_join(pterido_tropicos, by = "sci_name") %>%
    select(
      query = sci_name,
      matched_name = scientificnamewithauthors,
      matched_status = nomenclaturestatusname,
      namePublishedIn = namepublishedcitation,
      nameAccordingTo = source,
      name_res_status
    )

  bind_rows(mult_match, no_match) %>%
    arrange(name_res_status, query, desc(query_match_taxon_agree), matched_name)

}

# Format a query for IPNI
format_ipni_query <- function(pterido_long) {
  pterido_long %>%
    filter(name_res_status == "no_match") %>%
    filter(is.na(matched_name)) %>%
    select(query) %>%
    unique()
}

# Format a query for GBIF
format_gbif_query <- function(search_res) {
  search_res %>%
    filter(is.na(matched_name)) %>%
    select(query) %>%
    mutate(gn_parse_tidy(query) %>% select(canonicalsimple)) %>%
    select(query, gbif_query = canonicalsimple)
}

# Format a query for Plants of the World:
# POW queries must be formatted without author.
# For infraspecific taxa, need to include infra rank.
# E.g., "Athyrium yokoscense var. kirisimaense" OK
#       "Athyrium yokoscense kirisimaense" not OK
#       "Athyrium yokoscense var. kirisimaense (Tagawa) Li & J.Z.Wang" not OK
format_pow_query <- function(search_res) {
  search_res %>%
    filter(is.na(matched_name)) %>%
    mutate(gn_parse_tidy(query) %>% select(pow_query = canonicalfull)) %>%
    select(query, pow_query)
}

#' Tidy output of searching various taxonomic databases for non-matching names
#' from FTOL
#'
#' @param pterido_long Dataframe; output of name matching from FTOL
#'   in long format with candidate matches from tropicos added
#' @param ipni_res Dataframe; results of querying IPNI for FTOL "no-match" names
#' @param gbif_res  Dataframe; results of querying GBIF for FTOL "no-match" names
#' @param gbif_query Dataframe; GBIF query terms. `query` column is original FTOL
#' query; `gbif_query` is parsed version used to query GBIF.
#' @param pow_res Dataframe; results of querying POW for FTOL "no-match" names
#' @param pow_query Dataframe; POW query terms. `query` column is original FTOL
#' query; `pow_query` is parsed version used to query POW.
#'
#' @return Dataframe in long format with candidate matching names filled in
#'
tidy_ftol_no_match <- function(
  pterido_long, ipni_res,
  gbif_res, gbif_query,
  pow_res, pow_query) {

  # Format results of querying IPNI, GBIF, and POW for FTOL "no-match" names
  ipni_res_matched <-
    ipni_res %>%
    filter(!is.na(matched_name))
  if (nrow(ipni_res_matched) > 0) {
    ipni_res_matched <-
      ipni_res_matched %>%
      transmute(
        query, matched_name,
        namePublishedIn = reference,
        nameAccordingTo = glue::glue("IPNI|id:{id}|version:{version}"))
  }

  gbif_res_matched <-
    gbif_res %>%
    filter(!is.na(matched_name))
  if (nrow(gbif_res_matched) > 0) {
    gbif_res_matched <-
      gbif_res_matched %>%
      # Recover original FTOL query
      # since a modified query was used to search GBIF
      rename(gbif_query = query) %>%
      left_join(gbif_query, by = "gbif_query") %>%
      select(-gbif_query) %>%
      select(any_of(c(
        "query", "matched_name",
        "matched_status", "namePublishedIn",
        "nameAccordingTo"
      )))
  }

  pow_res_matched <-
    pow_res %>%
    filter(!is.na(matched_name))
  if (nrow(pow_res_matched > 0)) {
    pow_res_matched <-
      pow_res_matched %>%
      # Recover original FTOL query
      # since a modified query was used to search POW
      rename(pow_query = query) %>%
      left_join(pow_query, by = "pow_query") %>%
      transmute(
        query, matched_name,
        matched_status = taxonomicStatus,
        namePublishedIn = reference,
        nameAccordingTo = url
      )
  }

  # Combine results of querying IPNI, GBIF, and POW for FTOL "no-match" names
  ipni_gbif_pow_matched <- bind_rows(ipni_res_matched, gbif_res_matched, pow_res_matched) %>%
    # name_res_status refers to original FTOL search with ts_resolve_names()
    # these were all "no_match"
    mutate(name_res_status = "no_match")

  assertthat::assert_that(
    nrow(ipni_gbif_pow_matched) > 0,
    msg = "No queries matched any names for IPNI, GBIF, or POW searches"
  )

  # Populate "no-match" dataframe in long format
  pterido_long %>%
    select(-query_match_taxon_agree) %>%
    filter(name_res_status == "no_match") %>%
    anti_join(ipni_gbif_pow_matched, by = "query") %>%
    bind_rows(ipni_gbif_pow_matched) %>%
    mutate(
      query_match_taxon_agree = check_taxon_match(query, matched_name),
      use_query_as_new = case_when(
        query_match_taxon_agree == TRUE ~ 1,
        TRUE ~ 0)
    ) %>%
    arrange(name_res_status, query, desc(query_match_taxon_agree), matched_name)

}

#' Modify the Ferns of the World taxonomic database
#'
#' Modifications made directly without input from CSV files
#'
#' @param fow Ferns of the World taxonomic database in Dariwn Core format
#'
#' @return Modified FOW
#'
modify_fow <- function(fow) {
  # Up-front editing directly to FOW before making further edits
  fow %>%
    # remove duplicate Polypodium rhaeticum L.
    filter(taxonID != "4LGR5") %>%
    # Fix author of Asplenium richardii (Hook. fil.) Hook. fil.
    mutate(
      scientificName = case_when(
        scientificName == "Asplenium richardii (Hook. fil.) Hook. fil." ~ "Asplenium richardii Hook. fil.", #nolint
        TRUE ~ scientificName
      )
    ) %>%
    # Add entry for Pleopeltis polypodioides var. polypodioides autonym
    # https://www.ipni.org/n/77067053-1
    dct_add_row(
      sci_name = "Pleopeltis polypodioides var. polypodioides",
      taxonomicStatus = "accepted",
      taxonRank = "variety",
      parentNameUsageID = "4K7WS", # Pleopeltis polypodioides (L.) E. G. Andrews & Windham #nolint
    ) %>%
    # Add entry for Polystichum polyblepharum var. polyblepharum autonym
    # https://www.tropicos.org/name/26611834
    dct_add_row(
      sci_name = "Polystichum polyblepharum var. polyblepharum",
      taxonomicStatus = "accepted",
      taxonRank = "variety",
      parentNameUsageID = "4LN68", # Polystichum polyblepharum (Roem. ex Kunze) C. Presl #nolint
    ) %>%
    # Add entry for Dryopteris simasakii var. simasakii autonym
    dct_add_row(
      sci_name = "Dryopteris simasakii var. simasakii",
      taxonomicStatus = "accepted",
      taxonRank = "variety",
      parentNameUsageID = "37XPH", # Dryopteris simasakii (H. Itô) Kurata
    ) %>%
    # Add entry for Elaphoglossum peltatum f. peltatum
    dct_add_row(
      sci_name = "Elaphoglossum peltatum f. peltatum",
      taxonomicStatus = "accepted",
      taxonRank = "form",
      parentNameUsageID = "6F24P", # Elaphoglossum peltatum (Sw.) Urb.
    ) %>%
    # Add entry for Dryopteris x ebinoensis Sa.Kurata
    dct_add_row(
      sci_name = "Dryopteris x ebinoensis Sa.Kurata",
      taxonomicStatus = "nom. nud.",
      taxonRank = "species",
      namePublishedIn = "J. Nipp. Fern. Cl. 2(29): 7 (1977), n. n.; Sa.Kurata & Nakaike, Ill. Jap. Pterid. 8: 414 (1997).",
      nameAccordingTo = "http://ylist.info/ylist_detail_display.php?pass=32320"
    ) %>%
    # Change synonym status of Diplazium longisorum as indicated by plastome data
    dct_change_status(
      sci_name = "Diplazium longisorum (Baker) C. Chr.",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Desmophlebium longisorum (Baker) Mynssen, A. Vasco, Sylvestre, R. C. Moran & Rouhan",
      new_status = "synonym",
      usage_name = "Diplazium longisorum (Baker) C. Chr."
    ) %>%
    # Change status of Parahemionitis arifolia as indicated by plastome data
    # supposed to be accepted by PPGI
    # not a synonym of Acrostichum aureum
    dct_change_status(
      sci_name = "Parahemionitis arifolia (Burm. fil.) Panigrahi",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Hemionitis arifolia (Burm. fil.) T. Moore",
      new_status = "synonym",
      usage_name = "Parahemionitis arifolia (Burm. fil.) Panigrahi"
    ) %>%
    # Change status of Microsorum insigne as indicated by plastome data
    dct_change_status(
      sci_name = "Microsorum insigne (Blume) Copel.",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Leptochilus insignis (Blume) Fraser-Jenk.",
      new_status = "synonym",
      usage_name = "Microsorum insigne (Blume) Copel."
    ) %>%
    # Change status of Prosaptia khasyana as indicated by plastome data
    dct_change_status(
      sci_name = "Prosaptia khasyana (Hook.) C. Chr. & Tardieu",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Polypodium khasyanum Hook.",
      new_status = "synonym",
      usage_name = "Prosaptia khasyana (Hook.) C. Chr. & Tardieu"
    ) %>%
    # Change status of Alsophila lepifera as indicated by plastome data
    dct_change_status(
      sci_name = "Sphaeropteris lepifera (J. Sm. ex Hook.) R. M. Tryon",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Alsophila lepifera J. Sm. ex Hook.",
      new_status = "synonym",
      usage_name = "Sphaeropteris lepifera (J. Sm. ex Hook.) R. M. Tryon"
    ) %>%
    # Change status of names that should be accepted as indicated by
    # rogue family analysis
    dct_change_status(
      sci_name = "Deparia petersenii var. yakusimensis (H. Itô) M. Kato",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Dryopteris schnellii Tardieu",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Dryopteris spinosa Copel.",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Dryopteris triangularis Herter",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Davallia polypodioides D. Don",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Asplenium scandens Houlston & T. Moore",
      new_status = "accepted"
    ) %>%
    # Change status of Stenochlaena tenuifolia: GenBank data
    # indicate is Stenochlaena, not Lomariopsis
    # https://www.ncbi.nlm.nih.gov/nuccore/?term=Schuettpelz+504+(DUKE)
    dct_change_status(
      sci_name = "Stenochlaena tenuifolia (Desv.) T. Moore",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Lomariopsis tenuifolia (Desv.) Christ",
      new_status = "synonym",
      usage_name = "Stenochlaena tenuifolia (Desv.) T. Moore"
    ) %>%
    # GenBank MF318024 MF318119 MF318366 indicate Campyloneurum
    dct_change_status(
      sci_name = "Campyloneurum fallax Fée",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Polypodium longipetiolatum Brade",
      new_status = "synonym",
      usage_name = "Campyloneurum fallax Fée"
    ) %>%
    # Lonchitis mannii is Lonchitis,
    # not a synonym of Blotiella currorii (Hook.) R. M. Tryon according to
    # genbank data
    # https://www.ncbi.nlm.nih.gov/nuccore/U18641
    dct_change_status(
      sci_name = "Lonchitis mannii (Baker) Alston",
      new_status = "accepted"
    ) %>%
    # Selliguea lanceolata (Sw.) Fée is its own species, separate from
    # Loxogramme lanceolata (Sw.) C. Presl
    # https://www.ncbi.nlm.nih.gov/nuccore/?term=txid449879[Organism:noexp]
    # https://www.ncbi.nlm.nih.gov/nuccore/665985880
    dct_change_status(
      sci_name = "Selliguea lanceolata (Sw.) Fée",
      new_status = "accepted"
    ) %>%
    # Change status of Dryopteris salvinii: GenBank data
    # indicate is Dryopteris, not Ctenitis
    # https://www.ncbi.nlm.nih.gov/nuccore/JN189526
    dct_change_status(
      sci_name = "Dryopteris salvinii (Baker) Kuntze",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Ctenitis salvinii (Baker) Stolze",
      new_status = "synonym",
      usage_name = "Dryopteris salvinii (Baker) Kuntze"
    ) %>%
    # Change status of Lindsaea millefolia K.U.Kramer to
    # accepted, not synonym of Odontosoria decomposita (Baker) C. Chr.
    dct_add_row(
      sci_name = "Lindsaea millefolia K.U.Kramer",
      taxonomicStatus = "accepted",
      namePublishedIn = "Acta Botanica Neerlandica 6(2): 135. 1957. (Acta Bot. Neerl.)",
      nameAccordingTo = "http://www.tropicos.org/Name/26613614"
    ) %>%
    dct_change_status(
      sci_name = "Lindsaea millefolium K. U. Kramer",
      new_status = "synonym",
      usage_name = "Lindsaea millefolia K.U.Kramer"
    ) %>%
    dct_change_status(
      sci_name = "Odontosoria decomposita (Baker) C. Chr.",
      new_status = "synonym",
      usage_name = "Lindsaea millefolia K.U.Kramer"
    ) %>%
    # Change status of Lindsaea goudotiana (Kunze) Mett.: GenBank data indicate
    # is Lindsaea, not Odontosoria
    # GU478818
    dct_change_status(
      sci_name = "Lindsaea goudotiana (Kunze) Mett.",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Odontosoria goudotiana (Kunze) Christenh.",
      new_status = "synonym",
      usage_name = "Lindsaea goudotiana (Kunze) Mett."
    ) %>%
    # Change status of Lindsaea madagascariensis Baker: GenBank data
    # indicate is Lindsaea, not Odontosoria rbcL EF463231
    dct_change_status(
      sci_name = "Lindsaea madagascariensis Baker",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Lindsaea madagascariensis var. davallioides Baker",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Lindsaea madagascariensis var. intermedia Baker",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Odontosoria madagascariensis (Baker) Christenh.",
      new_status = "synonym",
      usage_name = "Lindsaea madagascariensis Baker"
    ) %>%
    # Change status of Lindsaea parasitica to accepted: GenBank data
    # indicate is Lindsaea, not Odontosoria HQ157277
    # leave Nesolindsaea caudata (Hook.) Lehtonen & Christenh.
    # as accepted for now
    dct_change_status(
      sci_name = "Lindsaea parasitica (Roxb. ex Griff.) Hieron.",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Lindsaea parasitica Wall.",
      new_status = "synonym",
      usage_name = "Lindsaea parasitica (Roxb. ex Griff.) Hieron."
    ) %>%
    # Change status of Lindsaea viridis Colenso to accepted: GenBank data
    # indicate is Lindsaea, not Odontosoria GU478769
    dct_change_status(
      sci_name = "Lindsaea viridis Colenso",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Odontosoria viridis Kuhn",
      new_status = "synonym",
      usage_name = "Lindsaea viridis Colenso"
    ) %>%
    # Change status of Cyathea senilis to accepted. GenBank indicate
    # Cyathea, not Sphaeropteris AM410203
    dct_change_status(
      sci_name = "Cyathea senilis (Klotzsch) Domin",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Sphaeropteris senilis (Klotzsch) R. M. Tryon",
      new_status = "synonym",
      usage_name = "Cyathea senilis (Klotzsch) Domin"
    ) %>%
    # Change status of Cyathea myosuroides to accepted. GenBank indicate
    # Cyathea, not Sphaeropteris KY684763
    dct_change_status(
      sci_name = "Cyathea myosuroides (Liebm.) Domin",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Sphaeropteris myosuroides (Liebm.) R. M. Tryon",
      new_status = "synonym",
      usage_name = "Cyathea myosuroides (Liebm.) Domin"
    ) %>%
    # Change status of Alsophila paucifolia Baker to accepted. GenBank indicate
    # Alsophila, not Cyathea MN155436
    dct_change_status(
      sci_name = "Alsophila paucifolia Baker",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Cyathea paucifolia (Baker) Domin",
      new_status = "synonym",
      usage_name = "Alsophila paucifolia Baker"
    ) %>%
    # Change status of Alsophila costularis Baker to accepted. GenBank indicate
    # Alsophila, not Cyathea KY858715
    dct_change_status(
      sci_name = "Alsophila costularis Baker",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Sphaeropteris chinensis (Copel.) comb. ined.",
      new_status = "synonym",
      usage_name = "Alsophila costularis Baker"
    ) %>%
    # Change status of Sphaeropteris brunoniana (Wall. ex Hook.) R. M. Tryon
    # to accepted. GenBank indicate Sphaeropteris, not Alsophila
    dct_change_status(
      sci_name = "Alsophila brunoniana Wall.",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Sphaeropteris brunoniana (Wall. ex Hook.) R. M. Tryon",
      new_status = "synonym",
      usage_name = "Alsophila brunoniana Wall."
    ) %>%
    # Change status of Sphaeropteris australis (C. Presl) R. M. Tryon
    # to accepted. GenBank indicate Sphaeropteris, not Alsophila leichhardtiana
    # AM410215
    dct_change_status(
      sci_name = "Sphaeropteris australis (C. Presl) R. M. Tryon",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Alsophila leichhardtiana F. Muell.",
      new_status = "synonym",
      usage_name = "Sphaeropteris australis (C. Presl) R. M. Tryon"
    ) %>%
    # Change status of Trichomanes guineense Afzel. ex Sw.
    # to accepted. GenBank indicate Trichomanes, not Abrodictyum
    # KP153243
    dct_change_status(
      sci_name = "Trichomanes guineense Afzel. ex Sw.",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Abrodictyum guineense (Afzel. ex Sw.) J. P. Roux",
      new_status = "synonym",
      usage_name = "Trichomanes guineense Afzel. ex Sw."
    ) %>%
    # Change Polyphlebium pyxidiferum to Crepidomanes
    dct_change_status(
      sci_name = "Crepidomanes pyxidiferum (L.) Dubuisson & Ebihara",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Polyphlebium pyxidiferum (L.) Ebihara & Dubuisson",
      new_status = "synonym",
      usage_name = "Crepidomanes pyxidiferum (L.) Dubuisson & Ebihara"
    ) %>%
    # Change Crepidomanes liukiuense to Vandenboschia
    dct_change_status(
      sci_name = "Vandenboschia liukiuensis (Y. Yabe) Tagawa",
      new_status = "accepted"
    ) %>%
    dct_change_status(
      sci_name = "Crepidomanes liukiuense (Y. Yabe) K. Iwats.",
      new_status = "synonym",
      usage_name = "Vandenboschia liukiuensis (Y. Yabe) Tagawa"
    ) %>%
    dct_validate(check_taxonomic_status = FALSE)
}

#' Update taxonomic names in Ferns of the World to generate Pteridocat
#'
#' @param pterido_names_taxized_inspected Dataframe (tibble); Inspected taxonomic names of
#' pteridophytes output from FTOL workflow after automatically adding
#' taxonomic data with taxize
#' @param new_names Dataframe (tibble); Manually curated new names to add to taxonomic database
#' @param fow Dataframe (tibble); Ferns of the World taxonomic database
#'
#' @return Dataframe (tibble); updated taxonomic database ("pteridocat")
#'
update_fow_names <- function(pterido_names_taxized_inspected, new_names, fow) {

  # Format names to add as accepted
  names_add_as_accepted <-
    pterido_names_taxized_inspected %>%
    mutate(
      add_as_accepted = case_when(
        use_query_as_accepted == 1 & matched_status == "accepted" ~ TRUE,
        use_query_as_accepted == 1 & name_res_status == "no_match" ~ TRUE, # matched_status for "no_match" names varies #nolint
        use_query_as_new == 1 ~ TRUE,
        use_match_as_accepted == 1 ~ TRUE,
        use_match_as_new == 1 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(add_as_accepted) %>%
    select(-add_as_accepted) %>%
    mutate(taxonomicStatus = "accepted") %>%
    select(scientificName = query, namePublishedIn,
           nameAccordingTo, taxonomicStatus) %>%
    bind_rows(
      filter(new_names, taxonomicStatus == "accepted") %>%
        select(scientificName, taxonomicStatus, namePublishedIn,
               nameAccordingTo, taxonRemarks)
    ) %>%
    mutate(
      taxonID = purrr::map_chr(scientificName, digest::digest),
      modified = as.character(Sys.time())
    ) %>%
    assert(is_uniq, scientificName, taxonID)

  # Make tibble of all existing names after addition
  fow_plus_new <-
    fow %>%
    bind_rows(names_add_as_accepted) %>%
    dct_validate(check_taxonomic_status = FALSE)

  # Format names to add as synonyms
  names_add_as_synonym <-
    pterido_names_taxized_inspected %>%
    mutate(
      add_as_synonym = case_when(
        use_query_as_accepted == 1 & matched_status != "accepted" ~ TRUE,
        use_query_as_synonym == 1 ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(add_as_synonym) %>%
    select(-add_as_synonym) %>%
    rename(scientificName = query, usage_name = matched_name) %>%
    mutate(taxonomicStatus = "synonym") %>%
    bind_rows(
      filter(new_names, taxonomicStatus == "synonym")
    ) %>%
    select(scientificName, taxonomicStatus, namePublishedIn,
           nameAccordingTo, taxonRemarks, usage_name) %>%
    # Filter to only new synonyms
    anti_join(fow_plus_new, by = "scientificName") %>%
    # Map acceptedNameUsageID from usage_name
    left_join(
      select(
        fow_plus_new, acceptedNameUsageID = taxonID,
        usage_name = scientificName),
      by = "usage_name"
    ) %>%
    left_join(
      select(names_add_as_accepted, acceptedNameUsageID = taxonID,
             usage_name = scientificName),
      by = "usage_name"
    ) %>%
    mutate(acceptedNameUsageID = coalesce(
      acceptedNameUsageID.x, acceptedNameUsageID.y)) %>%
    select(-acceptedNameUsageID.x, -acceptedNameUsageID.y) %>%
    select(-usage_name) %>%
    # Create `taxonID` and `modified` columns
    mutate(
      taxonID = purrr::map_chr(scientificName, digest::digest),
      modified = as.character(Sys.time())
    ) %>%
    verify(taxonomicStatus == "synonym") %>%
    # Fix nested synonyms: those where the usage_name maps to a name that's
    # already a synonym. Need to map back to the original acceptedNameUsageID
    left_join(
      select(
        fow_plus_new, taxonID,
        acceptedNameUsageID_2 = acceptedNameUsageID),
      by = c(acceptedNameUsageID = "taxonID")) %>%
    mutate(
      acceptedNameUsageID = case_when(
        is.na(acceptedNameUsageID_2) ~ acceptedNameUsageID,
        !is.na(acceptedNameUsageID_2) ~ acceptedNameUsageID_2
      )
    ) %>%
    select(-acceptedNameUsageID_2) %>%
    # Check everything is OK
    assert(not_na, scientificName, taxonID, acceptedNameUsageID) %>%
    assert(is_uniq, scientificName, taxonID)

  # Format names already in FOW to change status to synonym.
  # Three categories:
  # - FOW match is a synonym
  # - FOW match is accepted with no synonyms
  # - FOW match is accepted with mult synonyms
  names_to_change_status_raw <-
    # Format existing names to change matched names to synonym of query
    pterido_names_taxized_inspected %>%
    filter(use_query_as_accepted == 1) %>%
    select(query, matched_name) %>%
    # Add FOW taxon ID
    left_join(
      select(fow, matched_name = scientificName, fow_taxonID = taxonID),
      by = "matched_name"
    ) %>%
    assert(not_na, fow_taxonID) %>%
    left_join(
      select(fow, fow_acceptedNameUsageID = acceptedNameUsageID,
             fow_taxonID = taxonID),
      by = "fow_taxonID"
    ) %>%
    left_join(
      select(fow, fow_taxonID = acceptedNameUsageID,
             sci_name_synonym = scientificName),
      by = "fow_taxonID"
    )

  # - FOW match is a synonym
  names_to_change_status_fow_is_syn <-
    names_to_change_status_raw %>%
    filter(!is.na(fow_acceptedNameUsageID)) %>%
    left_join(
      select(fow, usage_name = scientificName,
             fow_acceptedNameUsageID = taxonID),
      by = "fow_acceptedNameUsageID"
    ) %>%
    transmute(sci_name = query, usage_name, new_status = "synonym")

  # - FOW match is accepted with no synonyms
  names_to_change_status_fow_with_no_syn <-
    names_to_change_status_raw %>%
    filter(is.na(fow_acceptedNameUsageID), is.na(sci_name_synonym)) %>%
    transmute(sci_name = matched_name,
              usage_name = query, new_status = "synonym")

  # - FOW match is accepted with multiple synonyms
  names_to_change_status_fow_with_mult_syn <-
    bind_rows(
      names_to_change_status_raw %>%
        filter(is.na(fow_acceptedNameUsageID), !is.na(sci_name_synonym)) %>%
        transmute(sci_name = matched_name,
                  usage_name = query, new_status = "synonym"),
      names_to_change_status_raw %>%
        filter(is.na(fow_acceptedNameUsageID), !is.na(sci_name_synonym)) %>%
        transmute(sci_name = sci_name_synonym,
                  usage_name = query, new_status = "synonym")
    )

  names_to_change_status <-
    bind_rows(
      names_to_change_status_fow_is_syn,
      names_to_change_status_fow_with_no_syn,
      names_to_change_status_fow_with_mult_syn,
      filter(new_names, taxonomicStatus == "synonym",
             scientificName %in% fow$scientificName) %>%
        select(sci_name = scientificName,
               usage_name, new_status = taxonomicStatus)
    ) %>%
    anti_join(names_add_as_synonym, by = c(sci_name = "scientificName")) %>%
    unique()

  res <-
    fow %>%
    bind_rows(names_add_as_accepted) %>%
    bind_rows(names_add_as_synonym) %>%
    dct_change_status(
      args_tbl = names_to_change_status, strict = FALSE, quiet = TRUE)

  res %>%
    dct_validate(check_taxonomic_status = FALSE)
}

#' Combine taxized data for writing out for inspection
#'
#' @param pterido_fuzzy_tropicos Tibble; output of tidy_fuzzy_tropicos()
#' @param pterido_long Tibble; output of format_ipni_query()
#' @param pterido_no_match_taxized Tibble; output of tidy_ftol_no_match()
#'
#' @return Tibble
#'
combine_taxized_names <- function(
  pterido_fuzzy_tropicos,
  pterido_long, pterido_no_match_taxized) {

  bind_rows(
    pterido_fuzzy_tropicos,
    filter(pterido_long, name_res_status == "mult_match"),
    pterido_no_match_taxized
  ) %>%
    # Select final columns
    transmute(
      query,
      matched_name,
      matched_status,
      query_match_taxon_agree,
      use_query_as_synonym,
      use_query_as_accepted,
      use_query_as_new,
      use_match_as_accepted = NA_character_,
      use_match_as_new = NA_character_,
      namePublishedIn,
      nameAccordingTo,
      taxonRemarks,
      notes,
      name_res_status,
      done = 0
    )
}

# utils ----

# Write CSV and return the output path
write_tar_csv <- function(x, file, ...) {
  readr::write_csv(x = x, file = file, ...)
  file
}

# Write RDS and return the output path
write_tar_rds <- function(object, file, ...) {
  saveRDS(object = object, file = file, ...)
  file
}

# Download a file and return the output path
download_file <- function(url, destfile, ...) {

  download.file(url = url, destfile = destfile, ...)

  destfile

}

# Silence chatty readr function in gn_parse_tidy()
gn_parse_tidy <- function(...) {
  suppressMessages(rgnparser::gn_parse_tidy(...))
}

# Helper function: convert NA to NULL
null_if_na <- function(x) {ifelse(is.na(x), return(NULL), return(x))}

#' Check that the canonical form of two scientific names is the same
#'
#' @param name1 Character vector; first scientific name
#' @param name2 Character vector; second scientific name
#'
#' @examples
#' check_taxon_match(
#'   c("Crep minut J. Nitta", "Hym poly J. Nitta", NA, NA),
#'   c("Crep minut J.H. Nitta", "Hym polyanthos J. Nitta", NA, "a")
#' )
check_taxon_match <- function(name1, name2) {
  # Check input: no missing values, name1 and name2 are same length
  assertthat::assert_that(
    !all(is.na(name1)),
    msg = "name1 cannot be all missing data")
  assertthat::assert_that(
    !all(is.na(name2)),
    msg = "name2 cannot be all missing data")
  assertthat::assert_that(
    length(name1) == length(name2),
    msg = "name1 and name2 must be same length")
  magrittr::equals(
    gn_parse_tidy(name1) %>% pull(canonicalsimple),
    gn_parse_tidy(name2) %>% pull(canonicalsimple)
  )
}
