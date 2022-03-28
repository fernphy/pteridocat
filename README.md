
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pteridocat

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/474876749.svg)](https://zenodo.org/badge/latestdoi/474876749)
<!-- badges: end -->

The goal of pteridocat is to provide a taxonomic database of
pteridophytes that adheres to [Darwin Core
standards](https://dwc.tdwg.org/).

## Installation

You can install pteridocat from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("joelnitta/pteridocat")
```

## Usage

This package consists of a single data object, a taxonomic database of
pteridophytes called `pteridocat`.

The `tibble` package is recommended for pretty printing.

``` r
library(pteridocat)
library(tibble)

pteridocat
#> # A tibble: 64,602 × 17
#>    taxonID parentNameUsageID acceptedNameUsageID taxonomicStatus taxonRank
#>    <chr>   <chr>             <chr>               <chr>           <chr>    
#>  1 392BH   632BH             <NA>                accepted        species  
#>  2 6F23D   632BH             <NA>                accepted        species  
#>  3 6F226   632BH             <NA>                accepted        species  
#>  4 392L6   632BH             <NA>                accepted        species  
#>  5 392KN   632BH             <NA>                accepted        species  
#>  6 392KT   632BH             <NA>                accepted        species  
#>  7 392KF   632BH             <NA>                accepted        species  
#>  8 6F25L   632BH             <NA>                accepted        species  
#>  9 392KM   632BH             <NA>                accepted        species  
#> 10 392KK   632BH             <NA>                accepted        species  
#> # … with 64,592 more rows, and 12 more variables: scientificName <chr>,
#> #   genericName <chr>, infragenericEpithet <chr>, specificEpithet <chr>,
#> #   infraspecificEpithet <chr>, namePublishedIn <chr>, nomenclaturalCode <chr>,
#> #   nomenclaturalStatus <chr>, taxonRemarks <chr>, references <chr>,
#> #   modified <chr>, nameAccordingTo <chr>
```

Run `help(pteridocat)` to see more details about the data.

## Where do these data come from?

The data were downloaded from the [Catalog of
Life](https://www.catalogueoflife.org/) and modified under the [Creative
Commons Attribution (CC BY)
4.0](https://creativecommons.org/licenses/by/4.0/) license. They were
originally compiled by Michael Hassler, who maintains the [World Ferns
database](https://www.worldplants.de/world-ferns/ferns-and-lycophytes-list),
and generously contributed them to [Catalog of
Life](https://www.catalogueoflife.org/).

## Citing the data

If you use this package, please cite it! Here is an example:

    Nitta, JH (2022) pteridocat: A taxonomic database of pteridophytes. https://doi.org/10.5281/zenodo.6388786

The example DOI above is for the overall package.

Here is the latest DOI, which you should use if you are using the latest
version of the package:

[![DOI](https://zenodo.org/badge/474876749.svg)](https://zenodo.org/badge/latestdoi/474876749)

You can find DOIs for older versions by viewing the “Releases” menu on
the right.

You should also cite the [World Ferns
database](https://www.worldplants.de/world-ferns/ferns-and-lycophytes-list)
(contained in [Catalog of Life](https://www.catalogueoflife.org/)),
which forms the basis for `pteridocat`:

    Hassler, M. (2021). Checklist of Ferns and Lycophytes of the World. In O. Bánki, Y. Roskov, M. Döring, G. Ower, L. Vandepitte, D. Hobern, D. Remsen, P. Schalk, R. E. DeWalt, M. Keping, J. Miller, T. Orrell, R. Aalbu, R. Adlard, E. Adriaenssens, C. Aedo, E. Aescht, N. Akkari, M. A. Alonso-Zarazaga, et al., Catalogue of Life Checklist (12.8, Dec 2021). https://doi.org/10.48580/d4tm-3dc

## License

-   [GPL v3.0](LICENSE.md)

[CC BY-SA 4.0 is one-way compatible with GPL
v3.0](https://creativecommons.org/share-your-work/licensing-considerations/compatible-licenses/).
