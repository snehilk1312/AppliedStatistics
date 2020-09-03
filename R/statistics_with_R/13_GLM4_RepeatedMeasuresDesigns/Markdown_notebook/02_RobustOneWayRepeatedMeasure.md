Robust RM Anova
================

``` r
library(ez)
```

    ## Registered S3 methods overwritten by 'lme4':
    ##   method                          from
    ##   cooks.distance.influence.merMod car 
    ##   influence.merMod                car 
    ##   dfbeta.influence.merMod         car 
    ##   dfbetas.influence.merMod        car

``` r
library(nlme)
library(ggplot2)
library(multcomp)
```

    ## Loading required package: mvtnorm

    ## Loading required package: survival

    ## Loading required package: TH.data

    ## Loading required package: MASS

    ## 
    ## Attaching package: 'TH.data'

    ## The following object is masked from 'package:MASS':
    ## 
    ##     geyser

``` r
library(pastecs)
library(WRS)
```

    ## Loading required package: akima

    ## Loading required package: robustbase

    ## 
    ## Attaching package: 'robustbase'

    ## The following object is masked from 'package:survival':
    ## 
    ##     heart

    ## 
    ## Attaching package: 'WRS'

    ## The following object is masked from 'package:robustbase':
    ## 
    ##     hard.rejection

    ## The following object is masked from 'package:MASS':
    ## 
    ##     ltsreg

    ## The following object is masked from 'package:stats':
    ## 
    ##     ecdf

    ## The following object is masked from 'package:grDevices':
    ## 
    ##     bmp

``` r
library(reshape)
```

``` r
df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/13_GLM4_RepeatedMeasuresDesigns/Data_Files/Bushtucker.dat', header = TRUE)
```

## we will use the package WRS, which needs the data in wide format

## we will use functions as rmanova(), rmmcp() , rmanovab(), pairdepb()

``` r
df<- df[, -c(1)]
```

### one way repeated measure anova based on trimmed means, rmanova(data, tr = .2)

``` r
rmanova(df)
```

    ## [1] "The number of groups to be compared is"
    ## [1] 4

    ## $test
    ## [1] 2.752794
    ## 
    ## $df
    ## [1]  2.309193 11.545964
    ## 
    ## $siglevel
    ## [1] 0.1002
    ## 
    ## $tmeans
    ## [1] 8.000000 4.166667 4.000000 6.000000
    ## 
    ## $ehat
    ## [1] 0.5873188
    ## 
    ## $etil
    ## [1] 0.7697309

### performs one-way repeated-measures ANOVA using a bootstrap procedure, rmanovab(data, tr = .2, alpha = .05, nboot = 599)

``` r
rmanovab(df, nboot=2000)
```

    ## [1] "Taking bootstrap samples. Please wait."

    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero

    ## [1] "The number of groups to be compared is"
    ## [1] 4

    ## $teststat
    ## [1] 2.752794
    ## 
    ## $crit
    ## [1] 5.623696

### performs post hoc tests for one-way repeated-measures design based on trimmed means.

``` r
rmmcp(df)
```

    ## $n
    ## [1] 8
    ## 
    ## $test
    ##      Group Group       test    p.value  p.crit        se
    ## [1,]     1     2  3.7282016 0.01359625 0.01020 0.9834947
    ## [2,]     1     3  3.8733436 0.01172054 0.00851 1.0326995
    ## [3,]     1     4  0.8355727 0.44148206 0.01690 2.3935678
    ## [4,]     2     3  0.0000000 1.00000000 0.05000 1.2769904
    ## [5,]     2     4 -1.0454201 0.34371248 0.01270 1.7536809
    ## [6,]     3     4 -0.8000000 0.46001407 0.02500 2.5000000
    ## 
    ## $psihat
    ##      Group Group    psihat    ci.lower  ci.upper
    ## [1,]     1     2  3.666667  -0.4830017  7.816335
    ## [2,]     1     3  4.000000  -0.3572784  8.357278
    ## [3,]     1     4  2.000000  -8.0992023 12.099202
    ## [4,]     2     3  0.000000  -5.3880170  5.388017
    ## [5,]     2     4 -1.833333  -9.2326553  5.565989
    ## [6,]     3     4 -2.000000 -12.5482728  8.548273
    ## 
    ## $con
    ##      [,1]
    ## [1,]    0
    ## 
    ## $num.sig
    ## [1] 0

### performs post hoc test according to rmanovab()

``` r
pairdepb(df, nboot = 2000)
```

    ## [1] "Taking bootstrap samples. Please wait."

    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero
    
    ## Warning in cor(xvec, yvec): the standard deviation is zero

    ## $test
    ##      Group Group       test        se
    ## [1,]     1     2  4.2097438 0.9105859
    ## [2,]     1     3  3.8729833 1.0327956
    ## [3,]     1     4  1.0192944 1.9621417
    ## [4,]     2     3  0.1116291 1.4930394
    ## [5,]     2     4 -1.1527967 1.5903354
    ## [6,]     3     4 -0.9144599 2.1870833
    ## 
    ## $psihat
    ##      Group Group     psihat   ci.lower  ci.upper
    ## [1,]     1     2  3.8333333  -1.023125  8.689791
    ## [2,]     1     3  4.0000000  -1.508243  9.508243
    ## [3,]     1     4  2.0000000  -8.464756 12.464756
    ## [4,]     2     3  0.1666667  -7.796210  8.129543
    ## [5,]     2     4 -1.8333333 -10.315122  6.648455
    ## [6,]     3     4 -2.0000000 -13.664444  9.664444
    ## 
    ## $crit
    ## [1] 5.333333

# Conclusion: According to all robust anova and post-hoc tests , none of groups differ significantly
