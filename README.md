
<!-- README.md is generated from README.Rmd. Please edit README.Rmd only. -->

# stringSAS

<img src="man/figures/logo.png" align="right" height="180" alt="stringSAS logo"/>

**SAS-Inspired Character Functions for R**

`stringSAS` provides familiar SAS character manipulation functions for R
users, particularly SAS programmers transitioning to R and tidyverse
workflows.

The package focuses on SAS-inspired syntax, vectorized operations, and
straightforward integration with `dplyr`.

## Motivation

In pharmaceutical statistical programming, SAS and R are frequently used
side by side. Switching between these languages can be inconvenient
because similar character manipulation tasks require different function
names and argument conventions.

`stringSAS` was developed to reduce this cognitive overhead by providing
familiar SAS-inspired character functions in R.

The goal is not to replace existing R string manipulation packages, but
to provide a convenient interface for programmers who regularly work
with both languages.

## Installation

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("YOUR_USERNAME/stringSAS")
```

After the package is published on CRAN:

``` r
install.packages("stringSAS")
```

## Available Functions

| Function         | Description                               |
|------------------|-------------------------------------------|
| `sas_compress()` | Remove or retain selected characters      |
| `sas_scan()`     | Extract words by position                 |
| `sas_substr()`   | Extract substrings by position and length |
| `sas_catx()`     | Concatenate strings with a delimiter      |
| `sas_find()`     | Find a substring’s position               |
| `sas_tranwrd()`  | Replace substrings                        |

## Quick Examples

``` r
library(stringSAS)

sas_compress("AB-123", "-")
```

    ## [1] "AB123"

``` r
sas_scan("STUDY-001-002", 2, "-")
```

    ## [1] "001"

``` r
sas_substr("STUDY001", 1, 5)
```

    ## [1] "STUDY"

``` r
sas_catx("-", "STUDY", "001", "002")
```

    ## [1] "STUDY-001-002"

``` r
sas_find("Hello World", "World")
```

    ## [1] 7

``` r
sas_tranwrd("A-B-C", "-", "_")
```

    ## [1] "A_B_C"

## Using stringSAS with dplyr

`stringSAS` functions are vectorized and can be used directly inside
`mutate()`.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
dm <- tibble(
  usubjid = c(
    "ABC-001-0001",
    "ABC-002-0002",
    "XYZ-003-0003"
  )
)

dm %>%
  mutate(
    study = sas_scan(usubjid, 1, "-"),
    site = sas_scan(usubjid, 2, "-"),
    subject = sas_scan(usubjid, 3, "-"),
    clean_id = sas_compress(usubjid, "-")
  )
```

    ## # A tibble: 3 × 5
    ##   usubjid      study site  subject clean_id  
    ##   <chr>        <chr> <chr> <chr>   <chr>     
    ## 1 ABC-001-0001 ABC   001   0001    ABC0010001
    ## 2 ABC-002-0002 ABC   002   0002    ABC0020002
    ## 3 XYZ-003-0003 XYZ   003   0003    XYZ0030003

## Design Philosophy

`stringSAS` is designed to make SAS character manipulation familiar in
R.

The package:

- Uses recognizable SAS-style function names.
- Supports vectorized character inputs.
- Works naturally with tidyverse pipelines.
- Preserves R missing values where appropriate.
- Documents intentional differences from SAS behavior.

Unlike SAS program translation tools, `stringSAS` focuses specifically
on character manipulation.

## Contributing

Contributions, bug reports, and feature requests are welcome through
GitHub Issues.

## License

MIT License

## Author

Geonhyeok Jeong
