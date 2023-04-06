#' Sim probabilistic change
#'
#' Simulate a probabilistic change progressing linearly, in logit space.
#'
#' @param max_year The total number of "years" the change is spread across
#' @param pop_size The total number of simulated data points
#' @param phi The "precision" of the beta distribution
#' @param logit_low The low point in the logit space
#' @param logut_high The high point in the logit space
#'
#' @example examples/sim_prob_change_ex.R
#'
#' @importFrom purrr map_vec
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number
#' @importFrom stats rbeta
#' @export

sim_prob_change <-
  function(
    max_year = 100,
    pop_size = 300,
    phi = 10,
    logit_low = -4,
    logit_high = 4
  ){
    dobs_pop = 1:max_year
    logit_pop <- seq(logit_low, logit_high, length = 100)
    prob_pop <- plogis(logit_pop)
    dobs_obs <- sample(dobs_pop, size = pop_size, replace = T)
    probs_center <- prob_pop[dobs_obs]
    probs_obs <- purrr::map_vec(probs_center, .f = \(p) stats::rbeta(1, shape1 = p*phi, shape2 = (1-p)*phi))
    out <-
      tibble::tibble(
        dob = dobs_obs,
        prob_ml = probs_center,
        prob_obs = probs_obs,
        max_year = max_year,
        pop_size = pop_size,
        phi = phi
      ) |>
      dplyr::mutate(id = dplyr::row_number())
    return(out)
  }


#' Simulate a probablistic/continuous change
#'
#' @param n Number of simulated values
#' @param prob Probability of distribution 1
#' @param mu1 Mean of distrubtion 1
#' @param sigma1 sd of distribution 1
#' @param mu2 mean of distribution2
#' @param sigma2 sd of distribution 2
#'
#' @importFrom purrr map_vec
#' @importFrom tibble tibble
#' @export
sim_mixture <- function(
    n = 50,
    prob = 0.2,
    mu1=1,
    sigma1=0.25,
    mu2=0,
    sigma2=0.25
  ){

  # step 1: sample distribution indices
  dist_choice <- sample(
    c(1,2),
    size = n,
    prob = c(prob, 1-prob),
    replace = T
  )

  # step 2: random samole from distributuins
  dists <- list(
    rnorm(n, mean = mu1, sd = sigma1),
    rnorm(n, mean = mu2, sd = sigma2)
  )

  # step 3: get the sample from each distribution
  sim_values <- map_vec(
    1:n,
    \(idx){
      dists[[
        dist_choice[idx]
      ]][idx]
    }
  )

  out <- tibble(
    class = dist_choice,
    value = sim_values
  )

  return(out)
}

#' simulate prob/continuous change
#'
#' @param max_year The total number of "years" the change is spread across
#' @param pop_size The total number of simulated data points
#' @param phi The "precision" of the beta distribution
#' @param n Number of simulated values
#' @param prob Probability of distribution 1
#' @param mu1 Mean of distrubtion 1
#' @param sigma1 sd of distribution 1
#' @param mu2 mean of distribution2
#' @param sigma2 sd of distribution 2
#'
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @export
sim_prob_cont <- function(
    max_year = 100,
    pop_size = 300,
    phi = 10,
    n = 50,
    prob = 0.2,
    mu1=1,
    sigma1=0.25,
    mu2=0,
    sigma2=0.25
    logit_low = -4,
    logit_high = 4
){
  prob_df <- sim_prob_change(
    max_year = max_year,
    pop_size = pop_size,
    phi = phi,
    logit_low = logit_low,
    logit_high = logit_high
  )
  prob_df |>
    rowwise() |>
    mutate(
      continuous = list(sim_mixture(prob = prob_obs))
    ) |>
    unnest(continuous)->
    full_sim
  return(full_sim)
}


