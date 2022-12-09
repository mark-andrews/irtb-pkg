#' Bayesian Item Response Theory Model
#'
#' Implements a Bayesian multilevel 2PL IRT model
#'
#' @param formula A formula like score ~ person + item. The lhs is the binary
#'   score or accuracy variable. The rhs gives the person
#'   (student/respondent/subject/participant) and item (question) variables, in
#'   that order.
#' @param chi Guessing parameter: 0 < c < 1.
#' @param data The data frame with the required variables
#' @param returns Which object or objects is returned. Options are 'samples',
#'   'model', or 'both'. The 'samples' is a list of 'person' and 'item' related
#'   parameter samples. The 'model' is the rstan model. The 'both' options
#'   return a list with both 'samples' and 'model.
#'
#' @return An rstan model
#' @export
#' @examples
#' \dontrun{
#' M <- irt(score ~ nid + item, data = exam_df)
#' }
irt <- function(formula, chi = 0, data, returns = 'samples') {

  stopifnot(formula.tools::is.two.sided(formula))
  stopifnot(returns %in% c('samples', 'model', 'both'))

  lhs_vars <- formula.tools::lhs.vars(formula)
  rhs_vars <- formula.tools::rhs.vars(formula)

  stopifnot(length(rhs_vars) == 2)
  stopifnot(length(lhs_vars) == 1)

  person_var <- rlang::sym(rhs_vars[1])
  item_var <- rlang::sym(rhs_vars[2])

  data <- dplyr::mutate(data,
                        person_uid = as.numeric(factor( {{ person_var }} )),
                        item_uid = as.numeric(factor( {{ item_var }})))

  model_data <- within(list(), {

    person <- dplyr::pull(data, person_uid)

    item <- dplyr::pull(data, item_uid)

    y <- data[[lhs_vars[1]]]

    stopifnot(all(sort(unique(y)) == c(0, 1)))

    stopifnot(length(person) == length(item))
    stopifnot(length(person) == length(y))

    N <- length(person)

    J <- length(unique(person))

    K <- length(unique(item))

    chi = chi

  })

  M <- rstan::sampling(object = stanmodels$irtv2,
                       control = list(adapt_delta = 0.99),
                       data = model_data,
                       iter = 10000,
                       warmup = 2500,
                       cores = 4)

  samples <- tidybayes::gather_draws(M, alpha[i], beta[i], gamma[i]) %>%
    dplyr::ungroup()

  results <- list()

  # return all alpha values along with the person_var
  results[['person']] <- dplyr::left_join(
    dplyr::select(data, {{ person_var }}, person_uid) %>%
      dplyr::distinct(),
     samples %>%
      dplyr::filter(.variable == 'alpha') %>%
      dplyr::select(person_uid = i, alpha = .value),
    by = 'person_uid'
  ) %>% dplyr::select(-person_uid)

  # return all gamma and beta values along with the item_var
  results[['item']] <- dplyr::left_join(
    dplyr::select(data, {{ item_var }}, item_uid) %>%
      dplyr::distinct(),
    samples %>%
      dplyr::filter(.variable %in% c('beta','gamma')) %>%
      dplyr::select(item_uid = i, .draw, .variable, .value),
    by = 'item_uid'
  ) %>% dplyr::select(-item_uid) %>%
    pivot_wider(names_from = .variable, values_from = .value) %>%
    select(-.draw)

  if (returns == 'samples'){
    return(results)
  } else if (returns == 'model') {
    return(M)
  } else if (returns == 'both') {
    return(list(samples = results,
                model = M))
  } else {
    return(NULL)
  }


}
