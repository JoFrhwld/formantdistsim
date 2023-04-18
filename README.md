
<!-- README.md is generated from README.Rmd. Please edit that file -->

# formantdistsim

<!-- badges: start -->
<!-- badges: end -->

``` r
library(formantdistsim)
library(tidyverse)
```

This package is supposed to simulate an underlying binomial, or logistic
change, where the data is generated from a different gaussian, depending
on which category was chosen.

- `phi`: the weight on the beta distribution. Higher = more closely
  clustered together probabilities.

- `n`:The number of samples from each “speaker”

- `pop_size`: the number of “speakers”

- `mu1`/`mu2`: the $\mu$ of each distribution

- `sigma1`/`sigma2`: the $\sigma$ of each distribution

- `logit_low`/`logit_high` the start and end point, in logit space, of
  the probabilistic change.

``` r
set.seed(200)

sim_prob_cont(
  max_year = 100,
  phi = 10, 
  n =  50, 
  pop_size = 100, 
)->
  simulated
```

``` r
simulated |>
  group_by(id) |> 
  summarise(
    value = mean(value),
    dob = mean(dob)
  ) |> 
  ggplot(aes(dob, value))+
    geom_point()
```

<img src="man/figures/README-simulated data-1.png" width="100%" />
