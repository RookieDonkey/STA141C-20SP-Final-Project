# blblm

<!-- badges: start -->
<!-- badges: end -->

## Examples

``` r
library(blblm)
fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
coef(fit)
#> (Intercept)          wt          hp       wt:hp 
#> 48.88428523 -7.88702986 -0.11576659  0.02600976
confint(fit, c("wt", "hp"))
#>           2.5%       97.5%
#> wt -10.7902240 -5.61586271
#> hp  -0.1960903 -0.07049867
sigma(fit)
#> [1] 1.838911
sigma(fit, confidence = TRUE)
#>    sigma      lwr      upr 
#> 1.838911 1.350269 2.276347
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
#>        1        2 
#> 21.55538 18.80785
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
#>        fit      lwr      upr
#> 1 21.55538 20.02457 22.48764
#> 2 18.80785 17.50654 19.71772


fitglm <- blbglm(Species ~ Sepal.Length * Sepal.Width, data = iris[1:100,], m = 3, B = 100, family = binomial)
coef(fitglm)
#> (Intercept)   Sepal.Length   Sepal.Width  Sepal.Length:Sepal.Width 
#>  556.52302     -61.47023     -285.97767            39.74875 
confint(fitglm, c("Sepal.Length", "Sepal.Width"))
#>                 2.5%     97.5%
#> Sepal.Length -282.8369 204.1730
#> Sepal.Width  -677.7462 186.2225
sigma(fitglm)
#> 5.763032e-06
sigma(fitglm, confidence = TRUE)
#>       sigma          lwr          upr 
#> 5.763032e-06 4.651379e-06 7.097591e-06 
predict(fitglm, data.frame(Sepal.Length = c(5.5, 5.1), Sepal.Width = c(2.3, 3.5)))
#>        1        2 
#> 48.53386 37.68657 
predict(fitglm, data.frame(Sepal.Length = c(5.5, 5.1), Sepal.Width = c(2.3, 3.5)), confidence = TRUE)      #>  fit       lwr      upr
#> 1 48.53386  18.33746 71.41485
#> 2 37.68657 -20.14303 89.66908
```

