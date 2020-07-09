
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lychee

The package [`lychee`](https://sumtxt.github.io/lychee/) helps to link
and join data frames with key variables that are are similiar but not
identical (e.g., a variable with geographic names spelled slightly
different or nearby geographic coordinates). Different from the
`fuzzyjoin` package, the package
[`lychee`](https://sumtxt.github.io/lychee/) does not output all matches
given some definition of sufficient similarity, but constructs optimal
one-to-one matches minimizing the total difference across all matches.

The function `linkr()` stacks two data frames and finds an optimal
one-to-one pairing of rows in one data frame with rows in the other data
frame. The output is a data frame with as many rows as there are in the
two datasets and a common identifier for the pairs. The complementary
function is `joinr()` which, instead of stacking and assigning a common
identifier, joins two data frames similar to the `base::merge()` or
`dplyr::full_join()` function.

### Installation

``` r
# Install development version from GitHub
remotes::install_github("sumtxt/lychee")
```

For more details and to learn how to use this package: [Getting Started
with lychee](https://sumtxt.github.io/lychee/articles/lychee.html).

### Usage

The example belows shows how `joinr` finds optimal matches in two data
frames (`elec94` and `elec09`) within two groups (strata). The first
data frame (`elec94`) lists the strongholds of Germany’s green party in
the 1994 Federal election (`election=BTW`) and the 1994 European
election (`election=EP`). The second data frame lists such strongholds
for the 2009 elections. While the names of the cities are spelled
slightly different in the two data frames, `joinr` merges the two tables
correctly.

``` r
elec94
#> # A tibble: 6 x 3
#>   city                        election greens
#>   <chr>                       <chr>     <dbl>
#> 1 Tübingen                    BTW        15.1
#> 2 Heidelberg, Stadt           BTW        18.4
#> 3 Freiburg im Breisgau, Stadt BTW        21.9
#> 4 Münster, Stadt              EP         20.7
#> 5 Heidelberg, Stadt           EP         21.9
#> 6 Freiburg im Breisgau, Stadt EP         29

elec09
#> # A tibble: 6 x 3
#>   city                          election greens
#>   <chr>                         <chr>     <dbl>
#> 1 Darmstadt, Wissenschaftsstadt BTW        20.9
#> 2 Heidelberg                    BTW        22.4
#> 3 Freiburg (Breisgau)           BTW        25.4
#> 4 Heidelberg                    EP         28.6
#> 5 Lüchow-Dannenberg             EP         29.9
#> 6 Freiburg (Breisgau)           EP         32.5

joinr(elec94,elec09,
    strata="election",
  by="city", 
  suffix=c("94","09"),
  add_distance=TRUE, 
  caliper=12,
  method='lcs',
  full=TRUE)
#> # A tibble: 8 x 6
#>   election city94                  greens94 match_dist city09                   greens09
#>   <chr>    <chr>                      <dbl>      <dbl> <chr>                       <dbl>
#> 1 BTW      Tübingen                    15.1         NA <NA>                         NA  
#> 2 BTW      Heidelberg, Stadt           18.4          7 Heidelberg                   22.4
#> 3 BTW      Freiburg im Breisgau, …     21.9         12 Freiburg (Breisgau)          25.4
#> 4 BTW      <NA>                        NA           NA Darmstadt, Wissenschaft…     20.9
#> 5 EP       Münster, Stadt              20.7         NA <NA>                         NA  
#> 6 EP       Heidelberg, Stadt           21.9          7 Heidelberg                   28.6
#> 7 EP       Freiburg im Breisgau, …     29           12 Freiburg (Breisgau)          32.5
#> 8 EP       <NA>                        NA           NA Lüchow-Dannenberg            29.9
```
