
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pretrends

<!-- badges: start -->

<!-- badges: end -->

The pretrends package provides tools for power calculations for
pre-trends tests, and visualization of possible violations of parallel
trends. Calculations are based on [Roth (2022,
AER:Insights)](https://jonathandroth.github.io/assets/files/roth_pretrends_testing.pdf).
(Please cite the paper if you enjoy the package!)

If you’re not an R user, you may also be interested in the associated
[Stata package](https://github.com/mcaceresb/stata-pretrends#pretrends)
or [Shiny app](https://github.com/jonathandroth/PretrendsPower).

## Installation

You can install the released version of pretrends from Github using the
devtools package:

``` r
# install.packages("devtools") #install devtools if not installed
devtools::install_github("jonathandroth/pretrends")
```

## Application to He and Wang (2017)

We illustrate how to use the package with an application to [He and Wang
(2017)](https://www.aeaweb.org/articles?id=10.1257/app.20160079). The
analysis will be based on the event-study in Figure 2C, which looks like
this:

![He and Wang Plot.](man/figures/HeAndWang.png)

### Load the package and data

We first load the pretrends package.

``` r
library(pretrends)
```

Next we load the estimation results used for the event-plot, namely the
coefficients (*beta*), the variance-covariance matrix (*sigma*), and the
corresponding event-times (*t*). In this case, these coefficients come
from a two-way fixed effects regression, but the pretrends package can
accommodate an event-study from any asymptotically normal estimator,
including [Callaway and Sant’Anna
(2020)](https://www.sciencedirect.com/science/article/pii/S0304407620303948?dgcid=author)
and [Sun and Abraham
(2020)](https://www.sciencedirect.com/science/article/abs/pii/S030440762030378X).

``` r
#Load the coefficients, covariance matrix, and time periods
beta <- pretrends::HeAndWangResults$beta
sigma <- pretrends::HeAndWangResults$sigma
tVec <- pretrends::HeAndWangResults$tVec
referencePeriod <- -1 #This is the omitted period in the regression
data.frame(t = tVec, beta = beta)
#>    t         beta
#> 1 -4  0.066703148
#> 2 -3 -0.007701792
#> 3 -2 -0.030769054
#> 4  0  0.084030658
#> 5  1  0.242441818
#> 6  2  0.219878986
#> 7  3  0.191092536
```

### Using the package

The pretrends package has two main functions. The first is
*slope_for_power()*, which calculates the slope of a linear violation of
parallel trends that a pre-trends test would detect a specified fraction
of the time. (By detect, we mean that there is any significant
pre-treatment coefficient.)

``` r
#Compute slope that gives us 50% power
slope50 <-
slope_for_power(sigma = sigma,
                targetPower = 0.5,
                tVec = tVec,
                referencePeriod = referencePeriod)
slope50
#> [1] 0.0520609
```

The package’s second (and main) function is *pretrends()*, which enables
power analyses and visualization given the results of an event-study and
a user-hypothesized difference in trends. We illustrate this using the
linear trend against which pre-tests have 50 percent power, computed
above.

``` r
pretrendsResults <- 
  pretrends(betahat = beta, 
            sigma = sigma, 
            tVec = tVec, 
            referencePeriod = referencePeriod,
            deltatrue = slope50 * (tVec - referencePeriod))
```

The pretrends function returns a list of objects, which we examine in
turn. First, we can visualize the event-plot and the hypothesized trend.

``` r
pretrendsResults$event_plot
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

Next, *df_power* displays several useful statistics about the power of
the pre-test against the hypothesized trend:

-   **Power** The probability that we would find a significant pre-trend
    under the hypothesized pre-trend. (This is 0.50, up to numerical
    precision error, by construction in our example).

-   **Bayes Factor** The ratio of the probability of ‘’passing’’ the
    pre-test under the hypothesized trend relative to under parallel
    trends.

-   **Likelihood Ratio** The ratio of the likelihood of the observed
    coefficients under the hypothesized trend relative to under parallel
    trends.

``` r
pretrendsResults$df_power
#>       Power Bayes.Factor Likelihood.Ratio
#> 1 0.4996962    0.5693746         0.105746
```

Next, *df_eventplot* contains the data used to make the event-plot. It
also includes a column *meanAfterPretesting*, which shows the expected
value of the coefficients conditional on passing the pre-test under the
hypothesized trend.

``` r
pretrendsResults$df_eventplot
#>    t      betahat  deltatrue         se meanAfterPretesting
#> 1 -4  0.066703148 -0.1561827 0.09437463         -0.09231506
#> 2 -3 -0.007701792 -0.1041218 0.07705139         -0.05555288
#> 3 -2 -0.030769054 -0.0520609 0.05512372         -0.02790767
#> 4 -1  0.000000000  0.0000000 0.00000000          0.00000000
#> 5  0  0.084030658  0.0520609 0.06264775          0.06490538
#> 6  1  0.242441818  0.1041218 0.08981034          0.12085292
#> 7  2  0.219878986  0.1561827 0.08877826          0.16947289
#> 8  3  0.191092536  0.2082436 0.09893648          0.22454859
```

Finally, the plot event_plot_pretest adds the *meanAfterPretesting* to
the original event-plot (see description above).

``` r
pretrendsResults$event_plot_pretest
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

Although our example has focused on a linear violation of parallel
trends, the package allows the user to input an arbitrary non-linear
hypothesized trend. For instance, here is the event-plot and power
analysis from a quadratic trend.

``` r
  quadraticPretrend <-
  pretrends(betahat = beta, 
            sigma = sigma, 
            tVec = tVec, 
            referencePeriod = referencePeriod,
            deltatrue = 0.024 * (tVec - referencePeriod)^2)

quadraticPretrend$event_plot_pretest
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

``` r
quadraticPretrend$df_power
#>       Power Bayes.Factor Likelihood.Ratio
#> 1 0.6624588    0.3842651        0.4332635
```
