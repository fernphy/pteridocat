# pteridocat.RDS should be copied here from the pteridocat_maker project

pteridocat <- readRDS("data/pteridocat.RDS")

usethis::use_data(pteridocat, overwrite = TRUE)
