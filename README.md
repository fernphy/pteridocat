
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pteridocat

<!-- badges: start -->
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

The taxonomic database is provided as a tibble:

``` r
library(pteridocat)
library(tibble)

pteridocat
#> # A tibble: 63,743 × 16
#>    taxonID parentNameUsageID acceptedNameUsageID taxonomicStatus taxonRank
#>    <chr>   <chr>             <chr>               <chr>           <chr>    
#>  1 6VC3M   6NPC              <NA>                accepted        species  
#>  2 4GFPS   6NPC              <NA>                accepted        species  
#>  3 4GFR7   6NPC              <NA>                accepted        species  
#>  4 6VC3J   6NPC              <NA>                accepted        species  
#>  5 4GFQC   6NPC              <NA>                accepted        species  
#>  6 4GFPQ   6NPC              <NA>                accepted        species  
#>  7 6VC3K   6NPC              <NA>                accepted        species  
#>  8 4GFPG   6NPC              <NA>                accepted        species  
#>  9 4GFPH   6NPC              <NA>                accepted        species  
#> 10 4GFMR   6NPC              <NA>                accepted        species  
#> # … with 63,733 more rows, and 11 more variables: scientificName <chr>,
#> #   genericName <chr>, infragenericEpithet <chr>, specificEpithet <chr>,
#> #   infraspecificEpithet <chr>, namePublishedIn <chr>, nomenclaturalCode <chr>,
#> #   nomenclaturalStatus <chr>, taxonRemarks <chr>, references <chr>,
#> #   modified <dttm>
```

Run `help(pteridocat)` to see more details about the data.

## Where do these data come from?

The data were downloaded from the [Catalog of
Life](https://www.catalogueoflife.org/) and modified according to code
in \[data-raw/pteridocat.R\] under the [Creative Commons Attribution (CC
BY) 4.0](https://creativecommons.org/licenses/by/4.0/) license. They
were originally compiled by Michael Hassler, who maintains the [World
Ferns
database](https://www.worldplants.de/world-ferns/ferns-and-lycophytes-list),
and generously contributed them to Catalog of Life.

## Licenses

-   Code: [MIT](LICENSE.md)
-   `pteridocat` dataset: [Creative Commons Attribution (CC BY)
    4.0](https://creativecommons.org/licenses/by/4.0/)
