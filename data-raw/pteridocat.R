# pteridocat.RDS should be copied here from the pteridocat_maker project
# https://github.com/fernphy/pteridocat_maker

fs::file_copy(
	"../pteridocat_maker/results/pteridocat.RDS", # path to pteridocat_maker
	"data-raw/pteridocat.RDS")

pteridocat <- readRDS("data-raw/pteridocat.RDS")

usethis::use_data(pteridocat, overwrite = TRUE)
