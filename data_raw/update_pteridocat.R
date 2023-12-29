# Update pteridocat with new names since last release

library(devtools)
library(dwctaxon)

load_all()

pteridocat <-
  pteridocat |>
  # Change D. giganteum (Baker) Ching to accepted from synonym of D. maximum
  dct_modify_row(
    scientificName = "Diplazium giganteum (Baker) Ching",
    taxonomicStatus = "Accepted",
    nameAccordingTo = "Wei R, Schneider H, Zhang XC (2013) Taxon 62:441-457"
  )

usethis::use_data(pteridocat, overwrite = TRUE)
