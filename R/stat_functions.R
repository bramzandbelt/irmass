#' Show trial numbers for al trial types and all individuals
#'
#' @param df a tidied data frame containing the performance data
show_trial_numbers <- function(df) {
  df %>%
    dplyr::group_by(subjectIx,trial,t_d) %>%
    dplyr::summarise(count = n()) %>%
    tidyr::spread(key = subjectIx, value = count)
}

#' Test of inhibition function for individual-level data
#'
#' Tests the independent race model's qualitative prediction that the probability of responding given a stop-signal increases as a function of stop-signal delay, in individual-level data.
#'
#' @param data a data frame containing the data
test_if_idv <- function(df) {

}

#' Test of inhibition function for group-level data
#'
#' Tests the independent race model's qualitative prediction that the probability of responding given a stop-signal increases as a function of stop-signal delay, in group-level data.
#'
#' @inheritParams test_if_idv
test_if_grp <- function() {

}

#' Test of stop-respond RT vs. no-signal RT for individual-level data
#'
#' Tests the independent race model's qualitative prediction that stop-respond RT should be shorter than no-signal, in individual-level data.
#'
#' @param ssrt a vector of stop-respond response times
#' @param nsrt a vector of no-signal response times
test_srrt_vs_nsrt_idv <- function(data, srrt, nsrt) {





  # Bayes Factor packages cannot handle tibbles, so we convert to data frame and refactorize the trial_alt variable
  df <-
    tibb %>%
    as.data.frame(.)
  df$trial_alt <- factor(df$trial_alt, levels = levels)

  # Get Bayes Factors and their inverse
  B10_data <-
    df %>%
    split(.$subjectIx) %>%
    map(~ttestBF(formula = RT_trial_inv ~ trial_alt,
                 data = .,
                 rscale = rscale
    )
    )

  B01_data <- map(B10_data, ~1/.)

  # Note that Bayes Factors are stored as log BF, so we need to take exponential
  B01 <- B01_data %>% map_dbl(~.@bayesFactor$bf) %>% exp()

  # Put the relevant stats in a tibble
  tibble(
    subjectIx = names(B10_data),
    model = "B01",
    B = B01,
    log10B = log10(B01),
    B_rank = min_rank(B01),
    error = B01_data %>% map_dbl(~.@bayesFactor$error),
    label = cut(B01,
                breaks = bf_breaks,
                labels = bf_labels)
  )



}

#' Test of stop-respond RT vs. no-signal RT for group-level data
#'
#' Paired t-test
#'
test_srrt_vs_nsrt_grp <- function(data, ns, sr) {

}

#' Test of stop-respond RT vs. stop-signal delay for individual-level data
#'
#' Bayesian ANOVA
test_srrt_vs_ssd_idv <- function() {

}

#' Test of stop-respond RT vs. stop-signal delay for group-level data
#'
test_srrt_vs_ssd_grp <- function() {

}
