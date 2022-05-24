# Read in pteridocat from targets workflow
pteridocat <- readRDS("_targets/objects/pteridocat")

# Save as data file for pteridocat package
usethis::use_data(pteridocat, overwrite = TRUE)
