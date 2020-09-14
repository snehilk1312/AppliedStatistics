Correlations
================

``` r
library(boot)
library(ggm)
library(Hmisc)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'lattice'

    ## The following object is masked from 'package:boot':
    ## 
    ##     melanoma

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survival'

    ## The following object is masked from 'package:boot':
    ## 
    ##     aml

    ## Loading required package: Formula

    ## Loading required package: ggplot2

    ## 
    ## Attaching package: 'Hmisc'

    ## The following object is masked from 'package:ggm':
    ## 
    ##     rcorr

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
library(ggplot2)
library(polycor)
```

## we have cor() , cor.test() which are part of base R system, while rcorr() which is a part of Hmisc package.

``` r
data<-read.delim('/home/atrides/Desktop/R/statistics_with_Python/06_Correlation/Data_Files/Exam Anxiety.dat',header=TRUE)

head(data)
```

    ##   Code Revise Exam Anxiety Gender
    ## 1    1      4   40  86.298   Male
    ## 2    2     11   65  88.716 Female
    ## 3    3     27   80  70.178   Male
    ## 4    4     53   80  61.312   Male
    ## 5    5      4   40  89.522   Male
    ## 6    6     22   70  60.506 Female

``` r
is.factor(data$Gender)
```

    ## [1] FALSE

``` r
data$Gender<-factor(data$Gender)
data$GenderX<-as.numeric(data$Gender)

is.factor(data$Gender)
```

    ## [1] TRUE

## Using cor()

``` r
# pearson with complete.obs
cor(data[,cbind('Code','Revise', 'Exam','Anxiety','GenderX')], use='complete.obs', method='pearson')
```

    ##                Code      Revise         Exam     Anxiety      GenderX
    ## Code     1.00000000 -0.22182864 -0.097793758  0.11356524 -0.033961781
    ## Revise  -0.22182864  1.00000000  0.396720697 -0.70924926 -0.085350584
    ## Exam    -0.09779376  0.39672070  1.000000000 -0.44099341  0.004674066
    ## Anxiety  0.11356524 -0.70924926 -0.440993412  1.00000000  0.002365840
    ## GenderX -0.03396178 -0.08535058  0.004674066  0.00236584  1.000000000

``` r
# kendall with complete.obs
cor(data[,cbind('Code','Revise', 'Exam','Anxiety','GenderX')], use='complete.obs', method='kendall')
```

    ##                Code      Revise         Exam     Anxiety      GenderX
    ## Code     1.00000000 -0.17498684 -0.051688850  0.10706392 -0.027863963
    ## Revise  -0.17498684  1.00000000  0.263325853 -0.48856004 -0.099160691
    ## Exam    -0.05168885  0.26332585  1.000000000 -0.28479188  0.007164456
    ## Anxiety  0.10706392 -0.48856004 -0.284791876  1.00000000 -0.018699690
    ## GenderX -0.02786396 -0.09916069  0.007164456 -0.01869969  1.000000000

``` r
# kendall with pairwise.complete.obs
cor(data[,cbind('Code','Revise', 'Exam','Anxiety','GenderX')], use='pairwise.complete.obs', method='kendall')
```

    ##                Code      Revise         Exam     Anxiety      GenderX
    ## Code     1.00000000 -0.17498684 -0.051688850  0.10706392 -0.027863963
    ## Revise  -0.17498684  1.00000000  0.263325853 -0.48856004 -0.099160691
    ## Exam    -0.05168885  0.26332585  1.000000000 -0.28479188  0.007164456
    ## Anxiety  0.10706392 -0.48856004 -0.284791876  1.00000000 -0.018699690
    ## GenderX -0.02786396 -0.09916069  0.007164456 -0.01869969  1.000000000

## using rcorr(), from Hmisc . This function does not work on dataframes, convert it into a matrix

## Note: rcorr() works only on matrix and gives, pearson and spearman only and use pairwise exclusion of missing values

``` r
dm <- data.matrix(data[,cbind('Code','Revise', 'Exam','Anxiety', 'Gender')])

rcorr(dm, type=c('pearson'))
```

    ##          Code Revise  Exam Anxiety Gender
    ## Code     1.00  -0.22 -0.10    0.11  -0.03
    ## Revise  -0.22   1.00  0.40   -0.71  -0.09
    ## Exam    -0.10   0.40  1.00   -0.44   0.00
    ## Anxiety  0.11  -0.71 -0.44    1.00   0.00
    ## Gender  -0.03  -0.09  0.00    0.00   1.00
    ## 
    ## n= 103 
    ## 
    ## 
    ## P
    ##         Code   Revise Exam   Anxiety Gender
    ## Code           0.0243 0.3257 0.2534  0.7334
    ## Revise  0.0243        0.0000 0.0000  0.3913
    ## Exam    0.3257 0.0000        0.0000  0.9626
    ## Anxiety 0.2534 0.0000 0.0000         0.9811
    ## Gender  0.7334 0.3913 0.9626 0.9811

## using cor.test(), only does one pair of variables at a time

``` r
cor.test(data$Exam, data$Anxiety, alternative = 'two.sided', method='pearson', conf.level = 0.95)
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  data$Exam and data$Anxiety
    ## t = -4.938, df = 101, p-value = 3.128e-06
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.5846244 -0.2705591
    ## sample estimates:
    ##        cor 
    ## -0.4409934

# Note:

# use pearson r for parametric

# use spearman rho for non-parametric

# use tendall tau for non-parametric and small datasets

## Now , if data is non-normal , we would use bootstrapping correaltion

``` r
library(boot)
data<-read.delim('/home/atrides/Desktop/R/statistics_with_Python/06_Correlation/Data_Files/The Biggest Liar.dat',header=TRUE)
```

``` r
bootTau<-function(data, i)cor(data$Position[i], data$Creativity[i], use='complete.obs', method='kendall')
boot_kendall<-boot(data, bootTau, 2000)
boot_kendall
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = data, statistic = bootTau, R = 2000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##       original       bias    std. error
    ## t1* -0.3002413 -0.002899185  0.09797569

``` r
# 95% confidence interval
boot.ci(boot_kendall)
```

    ## Warning in boot.ci(boot_kendall): bootstrap variances needed for studentized
    ## intervals

    ## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
    ## Based on 2000 bootstrap replicates
    ## 
    ## CALL : 
    ## boot.ci(boot.out = boot_kendall)
    ## 
    ## Intervals : 
    ## Level      Normal              Basic         
    ## 95%   (-0.4894, -0.1053 )   (-0.4974, -0.1160 )  
    ## 
    ## Level     Percentile            BCa          
    ## 95%   (-0.4845, -0.1031 )   (-0.4663, -0.0653 )  
    ## Calculations and Intervals on Original Scale
