#' Sim probabilistic change
#'
#' @importFrom purrr map_vec
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number
#' @export

sim_prob_change <-
  function(
    max_year = 100,
    pop_size = 300,
    kappa = 10
  ){
    dobs_pop = 1:max_year
    logit_pop <- seq(-4, 4, length = 100)
    prob_pop <- inv_logit(logit_pop)
    dobs_obs <- sample(dobs_pop, size = pop_size, replace = T)
    probs_center <- prob_pop[dobs_obs]
    probs_obs <- purrr::map_vec(probs_center, .f = \(p) rbeta(1, shape1 = p*kappa, shape2 = (1-p)*kappa))
    out <-
      tibble::tibble(
        dob = dobs_obs,
        prob_ml = probs_center,
        prob_obs = probs_obs,
        max_year = max_year,
        pop_size = pop_size,
        kappa = kappa
      ) |>
      dplyr::mutate(id = dplyr::row_number())
    return(out)
  }

#' inverse logit
inv_logit <- \(x) exp(x)/(1+exp(x))
