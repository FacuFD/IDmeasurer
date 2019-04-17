
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IDmeasurer

The goal of `IDmeasurer` package is to provide tools for assessment and
quantification of individual identity information in animal signals.
This package accompanies a research article by Linhart et al.:
[‘Measuring individual identity information in animal signals:
Overview and performance of available identity
metrics’](https://www.biorxiv.org/content/10.1101/546143v1), which can
currently be accessed at BioRxive.

## Installation

The package is currently available at
GitHub:

``` r
devtools::install_github('pygmy83/IDmeasurer', build = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```

The package has been also submitted to CRAN and it should be soon
possible to install the released version of `IDmeasurer` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("IDmeasurer")
```

## Example

This is a basic example which shows how to calculate individual identity
information in territorial calls of little owls (`ANspec` example data):

``` r
library(IDmeasurer)
```

Input data for the calculation of identity metrics in this package, in
general, is a data frame with the first column containing individual
identity codes (factor) and the other columns containing individuality
traits (numeric).

``` r
summary(ANspec)   
#>        id           dur               df              minf       
#>  007a   : 10   Min.   :0.3680   Min.   : 547.2   Min.   : 476.6  
#>  042a   : 10   1st Qu.:0.5040   1st Qu.: 955.7   1st Qu.: 742.2  
#>  045a   : 10   Median :0.5680   Median :1014.0   Median : 820.3  
#>  055a   : 10   Mean   :0.5733   Mean   :1033.0   Mean   : 798.7  
#>  062a   : 10   3rd Qu.:0.6320   3rd Qu.:1073.6   3rd Qu.: 890.6  
#>  070p   : 10   Max.   :0.9760   Max.   :1781.4   Max.   :1101.6  
#>  (Other):270                                                     
#>       maxf             q25              q50              q75        
#>  Min.   : 929.7   Min.   : 570.3   Min.   : 875.0   Min.   : 898.4  
#>  1st Qu.:1234.4   1st Qu.: 906.3   1st Qu.: 992.2   1st Qu.:1109.4  
#>  Median :1839.8   Median : 953.1   Median :1039.1   Median :1203.1  
#>  Mean   :1609.0   Mean   : 959.0   Mean   :1049.6   Mean   :1291.4  
#>  3rd Qu.:1882.8   3rd Qu.:1007.8   3rd Qu.:1084.0   3rd Qu.:1523.4  
#>  Max.   :1937.5   Max.   :1203.1   Max.   :1398.4   Max.   :1750.0  
#> 
```

This calculates HS metric for every single trait variable in the
dataset.

``` r
calcHS(ANspec, sumHS=F)
#>   vars Pr   HS
#> 2  dur  0 1.13
#> 3   df  0 0.58
#> 4 minf  0 0.80
#> 5 maxf  0 1.06
#> 6  q25  0 1.04
#> 7  q50  0 1.48
#> 8  q75  0 0.93
```

To calculate the HS for an entire signal, it is neccessary to have
uncorrelated variables in dataset. Raw (correlated) trait variables need
to be transformed into principal components by the Principal component
analysis.

``` r
temp <- calcPCA(ANspec) 
```

Calculate HS for an entire signal.

``` r
calcHS(temp) 
#> HS for significant vars         HS for all vars 
#>                    4.68                    4.68
```

To see description of the example dataset, use:

``` r
?ANspec
```

More examples can be found in IDmeasurer vignette:

``` r
vignette('idmeasurer-workflow-examples')
```
