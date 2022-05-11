
<!-- README.md is generated from README.Rmd. Please edit that file -->

# P3STAT294

<!-- badges: start -->
<!-- badges: end -->

`P3STAT294` extracts the latest version of vcf_GRCh37 published by  
ClinVar <https://ftp.ncbi.nlm.nih.gov/pub/clinvar/> to create a database
that the user can manipulate.

## Installation

You can install the development version of `P3STAT294` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("anagonzalezalvarez/P3STAT294", build_vignettes = TRUE)
```

## Example

This is a basic example which shows you how to download the ClinVar
database:

``` r
library(P3STAT294)

mutations_summary <- download_clinvar()
## Will obtain barplot with count of mutation in each chromosome obtained from the latest version of Clinvar's vcf_GRCh37
```

<img src="./plotchrom.png" width="700" />

The VCF file will be located in the working directory
