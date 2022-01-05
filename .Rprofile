source("renv/activate.R")
# Resolve conflicts
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("gn_parse", "rgnparser")

# Increase time-limit for downloads
# (default 60s not long enough for Catalog of Life data, ca 400mb)
options(timeout = max(600, getOption("timeout")))
