# R CMD BUILD and R CMD CHECK copy all files before excluding anything
# This causes folders with large number of files to take way too long
# https://github.com/r-lib/pkgbuild/issues/59
# This is a work-around so that devtools::check() runs in a reasonable time
library(devtools)
library(fs)

# Move large directories to external folder
temp_dir <- tempdir()

dir_copy("_targets", temp_dir)
dir_copy("renv", temp_dir)
dir_delete("_targets")
dir_delete("renv")

# Run check
res <- check()

# Put large directories back
dir_copy(path(temp_dir, "_targets"), ".")
dir_copy(path(temp_dir, "renv"), ".")
dir_delete(path(temp_dir, "_targets"))
dir_delete(path(temp_dir, "renv"))
