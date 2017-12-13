#' Derive Bayesian logistic regression predictionsgiven best-fitting parameters
#'
#' @param tibb Tibble containing columns subject identifier (subjectIx), best-fitting parameters (mean_beta0, mean_beta), and Bayes factor (B)
#' @export
derive_bayesian_logistic_regression_preds <- function(tibb) {

  prd_data <-
    tibble::tibble(subjectIx = tibb$subjectIx,
                   # subjectIx = as.character(tibb$subjectIx),
                   beta0 = tibb$mean_beta0,
                   beta = tibb$mean_beta,
                   B = tibb$B
    ) %>%
    # dplyr::left_join(expand.grid(subjectIx = as.character(tibb$subjectIx),
    dplyr::left_join(expand.grid(subjectIx = tibb$subjectIx,
                                 # From 0 to 750 ms seems a good range for stop-signal delays of 66, 166, 266, 366, and 466 ms
                                 x = seq(from = 0,
                                         to = 0.75,
                                         by = 0.01)
    ),
    by = 'subjectIx') %>%
    dplyr::mutate(y = logistic(beta0 + beta * x))

  prd_data

}

#' Get parameters for Bayesian hypothesis testing
#'
#' These include the width of the priod (rscale), Bayes factor category labels (labels) and cutoffs (breaks).
#'
#' @param param parameter to get settings of
#' @export
get_bf_settings <- function(param) {

  switch(param,
         rscale = 0.5,
         breaks = c(1/Inf, 1/100, 1/30, 1/10, 1/3, 1, 3, 10, 30, 100, Inf),
         labels = c('extreme evidence for H1',
                    'very strong evidence for H1',
                    'strong evidence for H1',
                    'moderate evidence for H1',
                    'anecdotal evidence for H1',
                    'anecdotal evidence for H0',
                    'moderate evidence for H0',
                    'strong evidence for H0',
                    'very strong evidence for H0',
                    'extreme evidence for H0'),
         n_iter = 100000 # Number of iterations for sampling from posterior distribution
         )


}

#' Join tibbles containing descriptive and inferential statistics data
#'
#' Joining occurs via a left-join operation by the subjectIx variable
#' @param descriptives tibble containing descriptives statistics
#' @param inferentials tibble containing inferential statistics
#' @export
join_stats_tibbles <- function(descriptives, inferentials) {

  # Assertions
  # assertthat::assert_that(assertthat::descriptives(descriptives, 'subjectIx'))
  # assertthat::assert_that(assertthat::descriptives(inferentials, 'subjectIx'))

  descriptives %>%
    dplyr::left_join(inferentials, by = "subjectIx")

}

#' Logistic function
#'
#' @param x data
#' @export
logistic <- function(x) {
  1 / (1 + exp(-x))
  }

#' Savage-Dicky density ratio test
#'
#' @param samples MCMC samples
#' @param prior prior

savage_dickey <- function(samples, prior = dnorm( 0 , 1/2^2 )) {
  fit.posterior <- polspline::logspline(samples)
  posterior <- polspline::dlogspline(0, fit.posterior)
  BF01 <- posterior / prior
  BF01
}


#' Show trial numbers for al trial types and all individuals
#'
#' @param df a tidied data frame containing the performance data
#' @export
show_trial_numbers <- function(df) {
  df %>%
    dplyr::group_by(subjectIx,trial,t_d) %>%
    dplyr::summarise(count = n()) %>%
    tidyr::spread(., key = subjectIx, value = count)
}

#' Test of inhibition function for individual-level data
#'
#' Tests the independent race model's qualitative prediction that the probability of responding given a stop-signal increases as a function of stop-signal delay, in individual-level data.
#'
#' @param data a data frame containing the data; y variable should be coded as integers
#' @param y_name e.g. 'r_bi'
#' @param x_name e.g. 't_d'
#' @param num_saved_steps number of saved steps
#' @param thin_steps how many steps for thinning
#' @param file_name_root base file name for output
#' @param graph_file_type graphic output format (defaults to 'eps')
#' @export
test_if_idv <- function(tibb) {

  # Specify some functions -----------------------------------------------------
  bayesian_logistic_regression <-
    function(df, y_name, x_name, num_saved_steps = 15000, thin_steps = 2,
             file_name_root, graph_file_type = 'eps') {

      # Markov-chain Monte Carlo
      mcmcCoda = genMCMC(data = df,
                         xName = x_name,
                         yName = y_name,
                         numSavedSteps = num_saved_steps,
                         thinSteps = thin_steps,
                         saveName = file_name_root
                         )

      # Display diagnostics of chain, for specified parameters (and write to disk)
      parameterNames = varnames(mcmcCoda) # get all parameter names
    for (parName in parameterNames) {
      diagMCMC(codaObject = mcmcCoda,
               parName = parName,
               saveName = file_name_root,
               saveType  = graph_file_type
      )
    }

    # Get summary statistics of chain
    summaryInfo = smryMCMC(mcmcCoda,
                           saveName = file_name_root
    )
    show(summaryInfo)

    # Display posterior information
    plotMCMC(mcmcCoda,
             data = df,
             xName = x_name,
             yName = y_name,
             pairsPlot = TRUE,
             showCurve = FALSE,
             saveName = file_name_root,
             saveType = graph_file_type)

    # Compute Bayes Factor using Savage-Dickey density ratio test
    BF01_savage <- savage_dickey(mcmcCoda[,'beta'])
    BF10_savage <- 1 / BF01_savage

    # Step 7 - # Put the relevant stats in a tibble
    df_bf_output <- tibble::tibble(model = "B01",
                                   B = BF01_savage,
                                   log10B = log10(BF01_savage),
                                   label = cut(BF01_savage,
                                               breaks = get_bf_settings("breaks"),
                                               labels = get_bf_settings("labels"))
    )

    # Close XQuartz windows
    for (idev in seq(from = 2, to = max(dev.list()))) {
      dev.off(which = idev)
    }

    list(params = summaryInfo,
         bf = tibble::tibble(model = "B01",
                             B = BF01_savage,
                             log10B = log10(BF01_savage),
                             label = cut(BF01_savage,
                                         breaks = get_bf_settings("breaks"),
                                         labels = get_bf_settings("labels")
                             )
         )
    )
  }

  # Make a tibble for storing descriptive and inferential statistics -----------
  bf_and_params <-
    tibble::tibble(subjectIx = integer(),
                   model = character(),
                   B = double(),
                   log10B = double(),
                   label = character(),
                   mean_beta0 = double(),
                   mean_beta = double()
    )

  # Loop over participants
  for (i_subject in unique(tibb$subjectIx)) {

    # Select data
    df_sub <-
      tibb %>%
      dplyr::filter(subjectIx == i_subject) %>%
      dplyr::select(t_d,r_bi) %>%
      dplyr::mutate(r_bi = as.integer(r_bi),
                    t_d = as.numeric(levels(t_d)[t_d])) %>%
      as.data.frame(.)


    # Perform Bayesian logistic regression and compute BF with Savage-Dickey density method
    # N.B. Graphics showing diagnostics and best-fits are written to disk
    bf_sub <-
      bayesian_logistic_regression(df = df_sub,
                                   y_name = 'r_bi',
                                   x_name = 't_d',
                                   file_name_root = file.path(figures_dir,
                                                              notebook_name,
                                                              sprintf('sub-%.02d',as.integer(i_subject))),
                                   graph_file_type = 'eps')

    # Store descriptive and inferential statistics in data frame
    bf_and_params <-
      tibble::add_row(bf_and_params,
                      subjectIx = i_subject,
                      model = bf_sub$bf$model,
                      B = bf_sub$bf$B,
                      log10B = bf_sub$bf$log10B,
                      label = bf_sub$bf$label,
                      mean_beta0 = bf_sub$params['beta0','Mean'],
                      mean_beta = bf_sub$params['beta','Mean']

      )

  }

  # Output
  bf_and_params

}

#' Test of inhibition function for group-level data
#'
#' Tests the independent race model's qualitative prediction that the probability of responding given a stop-signal increases as a function of stop-signal delay, in group-level data.
#' @param tibb tibble containing subject-level data, including Bayesian logistic regression parameter estimates
#' @param par_name name of the parameter containing the estimate of the logistic regression slope
test_if_grp <- function(tibb, par_name) {

  df <-
    tibb %>%
    as.data.frame()

  B_data <-
    BayesFactor::ttestBF(x = df[[par_name]],
                         # The independent race model predicts a positive slope
                         nullInterval = c(0, Inf),
                         rscale = irmass::get_bf_settings('rscale')
                         )
  B10 <- exp(B_data@bayesFactor$bf[1])
  B01 <- 1 / B10

  tibble::tibble(
    model = "B01",
    B = B01,
    log10B = log10(B01),
    error = B_data@bayesFactor$error[1],
    label = cut(B01,
                breaks = get_bf_settings('breaks'),
                labels = get_bf_settings('labels')
                )
    )
}

#' Test of stop-respond RT vs. no-signal RT for individual-level data
#'
#' Tests the independent race model's qualitative prediction that stop-respond RT should be shorter than no-signal, in individual-level data.
#'
#' @param tibb tibble with trial-level data
#' @param trial_alt_levels 2-element character vector containing the levels to contrast
#' @export
test_srrt_vs_nsrt_idv <- function(tibb, trial_alt_levels) {

  # Assertions
  assertthat::assert_that(assertthat::has_name(tibb, 'trial_alt'))
  assertthat::assert_that(assertthat::has_name(tibb, 'RT_trial'))
  assertthat::assert_that(assertthat::has_name(tibb, 'RT_trial_inv'))
  assertthat::assert_that(trial_alt_levels[2] == 'NS')

  # Bayes Factor packages cannot handle tibbles, so:
  # - convert to data frame
  # - refactorize independent variables
  df <-
    tibb %>%
    dplyr::filter(trial_alt %in% trial_alt_levels) %>%
    as.data.frame(.)
  df$trial_alt <- factor(df$trial_alt, levels = trial_alt_levels)

  # For each subject, get Bayes Factors and their inverse
  B10_data <-
    df %>%
    split(.$subjectIx) %>%
    # Perform an independent Bayesian t-test (trials assumed independent)

    # Note that we're comparings inverse response times; the race model predicts that stop-respond response times are shorter than no-signal response times (i.e. mean stop-respond RT - mean no-signal RT is negative). Consequently, the race model predicts that inverse stop-respond response times are longer than no-signal response times (i.e. mean inverse stop-respond RT - mean inverse no-signal RT is positive). So, we perform a one-sided Bayesian t-test, with the null interval in the positive range.


    # Null, mu1 - mu2 = d = 0, where
    # mu1 = RT_trial_inv_SAS
    # mu1 = RT_trial_inv_NS

    # Alt. 1: r=0.5 0<d<Inf
    # Alt. 2: r=0.5 !(0<d<Inf)

    purrr::map(~BayesFactor::ttestBF(formula = RT_trial_inv ~ trial_alt,
                                     data = .,
                                     nullInterval = c(0, Inf),
                                     rscale = get_bf_settings('rscale')
                                     )
               )

    # Take the inverse (i.e. Evidence for Null vs. Evidence for Alt. 1)
    B01_data <- purrr::map(B10_data, ~1/.[1])

    # Note that Bayes Factors are stored as log BF, so we need to take exponential
    B01 <- B01_data %>% purrr::map_dbl(~.@bayesFactor$bf) %>% exp()

    # Put the relevant stats in a tibble
    tibble::tibble(
      subjectIx = factor(as.integer(names(B10_data))),
      model = "B01",
      B = B01,
      log10B = log10(B01),
      # B_rank = dplyr::min_rank(B01),
      error = B01_data %>% purrr::map_dbl(~.@bayesFactor$error),
      label = cut(B01,
                  breaks = get_bf_settings('breaks'),
                  labels = get_bf_settings('labels'))
    ) %>%
      dplyr::mutate(subjectIx = factor(subjectIx))
}

#' Test of stop-respond RT vs. no-signal RT for group-level data
#'
#' Paired t-test
#' @param data a tibble
#' @param sr column name of stop-respond trial response time data
#' @param ns column name of no-stop trial response time data
#' @export
test_srrt_vs_nsrt_grp <- function(data, sr, ns) {

  # Convert to data frame, because Bayes Factor package cannot handle tibbles
  df <-
    data %>%
    as.data.frame(.)

  # Bayesian paired t-test
  B10_data <-
    BayesFactor::ttestBF(x = df[[sr]],
                         y = df[[ns]],
                         paired = TRUE,
                         nullInterval = c(-Inf,0), # race model predicts shorter RTs on sr than ns trials
                         rscale = get_bf_settings('rscale')
                         )

  # B10 data returns Bayes factor for two models:
  # - selected interval, -Inf < delta < 0,
  # - the complement interval, not -Inf < delta < 0
  # We're interested in the first model

  B01_data <- 1 / B10_data[1]

  # Bayes Factors are stored as log BF, so we need to take exponential
  B01 <- exp(B01_data@bayesFactor$bf)

  # Tibble for BayesFactor output
  tibble::tibble(
    model = "B01",
    B = B01,
    log10B = log10(B01),
    error = B01_data@bayesFactor$error,
    label = cut(B01,
                breaks = get_bf_settings('breaks'),
                labels = get_bf_settings('labels'))
  )

}

#' Test of stop-respond RT vs. stop-signal delay for individual-level data
#'
#' Bayesian ANOVA
#' @param tibb a tibble containing the data for the analysis
#' @export
test_effect_ssd_on_srrt_idv <- function(tibb) {

  # Assertions
  assertthat::assert_that(assertthat::has_name(tibb, 't_d_alt'))
  assertthat::assert_that(assertthat::has_name(tibb, 'RT_trial'))
  assertthat::assert_that(assertthat::has_name(tibb, 'RT_trial_inv'))
  assertthat::assert_that(is.ordered(tibb$t_d_alt))

  get_full_anova_model <- function(tibb) {
    tibb %>%
      as.data.frame() %>%
      BayesFactor::anovaBF(formula = RT_trial_inv ~ t_d_alt,
                           data = .,
                           rscaleFixed = get_bf_settings('rscale')
      )
  }

  get_bf_null_vs_full <- function(model) {
    # Bayes Factors are stored as log BF, so we need to take exponential
    1 / exp(model@bayesFactor$bf)
  }

  get_bf_null_vs_order_restricted <- function(tibb, model) {

    t_d_alt_levels <- c('short','intermediate','long')

    # Assertions
    assertthat::assert_that(all(levels(tibb$t_d_alt) %in% t_d_alt_levels))
    assertthat::assert_that(length(levels(tibb$t_d_alt)) == 3)

    df <-
      tibb %>%
      as.data.frame()

    # Number of ways in which t_d_alt levels could be ordered
    n_orderings <- length(combinat::permn(t_d_alt_levels))

    # Number of iterations to sample
    n_iter <- get_bf_settings('n_iter')

    # Sample from posterior distribution
    samples <- BayesFactor::posterior(model = model,
                                      iterations = n_iter)

    # We expect RT on stop-respond trials to increase with t_d.
    # Therefore, the inverse of RT should decrease with t_d.
    consistent <-
      (samples[, "t_d_alt-short"] > samples[, "t_d_alt-intermediate"]) &
      (samples[, "t_d_alt-intermediate"] > samples[, "t_d_alt-long"])

    # How many of the samples from posterior are consistent with this pattern?
    # n_consistent <- sum(consistent)
    # To prevent division by zero
    n_consistent = max(c(1,sum(consistent)))

    # Compute the BayesFactor of order-restricted model against the full model
    model_order_restricted_vs_full <- (n_consistent / n_iter) / (1 / n_orderings)

    # Compute the BayesFactor of order-restricted model against the null model
    model_order_restricted_vs_null <-
      model_order_restricted_vs_full *
      as.vector(model)


    # Express as null against order-restricted
    model_null_vs_order_restricted <- 1 / model_order_restricted_vs_null

    model_null_vs_order_restricted
  }

  # Nested tibble
  output <-
    tibb %>%
    dplyr::group_by(subjectIx) %>%
    tidyr::nest()

  # Run Bayesian ANOVA on RT_trial_inv with t_d_alt as factor
  output$model <-
    purrr::map(output$data, get_full_anova_model)

  # Compute the Bayes factor for the null model (intercept only) vs full model (containing t_d_alt as factor)
  output$B_null_vs_full <-
    purrr::map(output$model, get_bf_null_vs_full)

  # Compute the Bayes factor for the null model (intercept only) vs order-restricted model (RT_trial_inv decreases with t_d_alt)
  output$B_null_vs_order_restricted <-
    purrr::map2(output$data, output$model, get_bf_null_vs_order_restricted)

  output
}

#' Test of stop-respond RT vs. stop-signal delay for group-level data
#'
#' @param df data frame
#' @export
test_effect_ssd_on_srrt_grp <- function(df) {

  # Tibble for output
  bf_output <- tibble::tibble(mdl_class = character(),
                              mdl = character(),
                              B = numeric(),
                              log10_B = numeric(),
                              error = numeric(),
                              label = character()
                              )

  # BayesFactor packages cannot handle tibbles, so convert to data frame
  df <-
    df %>%
    as.data.frame(.)

  # Compute Bayes factor for full model
  if ('trial_alt' %in% colnames(df)) {
    B_full_vs_null <-
      BayesFactor::anovaBF(mean_RT ~ t_d_alt*trial_alt + subjectIx,
                           data = df,
                           whichRandom = "subjectIx",
                           rscaleFixed = get_bf_settings('rscale')
      )
  } else {
    B_full_vs_null <-
      BayesFactor::anovaBF(mean_RT ~ t_d_alt + subjectIx,
                           data = df,
                           whichRandom = "subjectIx",
                           rscaleFixed = get_bf_settings('rscale')
      )
  }

  # Fill in tibble
  for (mdl in rownames(B_full_vs_null@bayesFactor)) {
    bf_output <- tibble::add_row(bf_output,
                                 mdl_class = "null_vs_full",
                                 mdl = mdl,
                                 B = 1 / exp(B_full_vs_null[mdl]@bayesFactor$bf),
                                 log10_B = log10(1 / exp(B_full_vs_null[mdl]@bayesFactor$bf)),
                                 error = B_full_vs_null[mdl]@bayesFactor$error,
                                 label = cut(1 / exp(B_full_vs_null[mdl]@bayesFactor$bf),
                                             breaks = get_bf_settings('breaks'),
                                             labels = get_bf_settings('labels')
                                             )
                                 )
  }

  # Compute Bayes factor for order restricted model
  #
  # Test order-restricted model: RT_short < RT_intermediate < RT_long
  #
  # The independent race model predicts that stop-respond response time increases with signal delay.
  #
  # We do this using a 3-step procedure
  # 1. Sample from the posterior distribution and estimate the frequency of correct orderings
  # 2. Compute the BayesFactor of order-restricted model against the full model
  # 3. Compute the BayesFactor of order-restricted model against the null model (based on transitivity of Bayes Factors)
  #
  # For background on implementation, see the following paper and blog posts by Richard Morey:
  # - Morey, R. D., & Wagenmakers, E.-J. (2014). Simple relation between Bayesian order-restricted and point-null hypothesis tests. Statistics & Probability Letters, 92, 121–124. http://doi.org/10.1016/j.spl.2014.05.010
  # - http://bayesfactor.blogspot.nl/2015/01/multiple-comparisons-with-bayesfactor-2.html
  # - http://bayesfactor.blogspot.nl/2015/01/multiple-comparisons-with-bayesfactor-1.html

  n_iter = get_bf_settings('n_iter')

  for (mdl in rownames(B_full_vs_null@bayesFactor)) {

    # Only test order restricted model if 't_d_alt' is a factor
    if (grepl('t_d_alt',mdl)) {

      # Number of possible orderings of t_d
      n_orderings = length(combinat::permn(unique(df$t_d_alt)))

      # Sample from the posterior distribution and estimate the frequency of correct orderings
      samples = BayesFactor::posterior(B_full_vs_null[mdl], iterations = n_iter)

      # We expect mean RT on stop-respond trials to increase with t_d.
      consistent =
        (samples[, "t_d_alt-short"] < samples[, "t_d_alt-intermediate"]) &
        (samples[, "t_d_alt-intermediate"] < samples[, "t_d_alt-long"])
      # n_consistent = sum(consistent)

      # To prevent division by 0
      n_consistent = max(c(1,sum(consistent)))

      # Compute the BayesFactor of order-restricted model against the full model
      B_order_restricted_vs_full = (n_consistent / n_iter) / (1 / n_orderings)

      # Compute the BayesFactor of order-restricted model against the null model
      B_order_restricted_vs_null = B_order_restricted_vs_full * as.vector(B_full_vs_null[mdl])

      # Express as null against order-restricted
      B_null_vs_order_restricted = 1 / B_order_restricted_vs_null

      # Fill in tibble
      for (mdl in rownames(B_full_vs_null@bayesFactor)) {
        bf_output <- tibble::add_row(bf_output,
                                     mdl_class = "null_vs_order_restricted",
                                     mdl = mdl,
                                     B = B_null_vs_order_restricted,
                                     log10_B = log10(B_null_vs_order_restricted),
                                     error = NA,
                                     label = cut(B_null_vs_order_restricted,
                                                 breaks = get_bf_settings('breaks'),
                                                 labels = get_bf_settings('labels')
                                                 )
        )
      }

      # Remove variables
      rm(list=c('n_orderings',
                'samples',
                'consistent',
                'n_consistent',
                'B_order_restricted_vs_full',
                'B_order_restricted_vs_null',
                'B_null_vs_order_restricted'
                )
         )
    }

  }

  # # Add Bayes Factor labels
  # bf_output$label <- cut(log10(bf_output$B),
  #                        breaks = get_bf_settings('breaks'),
  #                        labels = get_bf_settings('labels')
  #                        )

  bf_output
}

# ==============================================================================

#' Assesses task performance criteria of the experimental session
#'
#' Returns a tibble with columns:
#' subjectIx
#' criterion
#' mean_perf_overall
#' failed_overall_crit
#' max_n_consec_fails
#' failed_blockwise_crit
#' @param data A tibble containing block data from experiment session
#' @export
assess_expt_performance_crit <- function(data) {

  cumsum_reset <- function(x) {x[x == 1] = sequence(with(rle(x), lengths[values == 1]));x}

  # Assess overall task performance
  overall_performance <-
    data %>%
    dplyr::group_by(subjectIx, criterion) %>%
    dplyr::summarize(mean_perf_overall = mean(performance)) %>%
    dplyr::mutate(failed_overall_crit = dplyr::case_when(
      criterion == 'NS_accuracy' & mean_perf_overall < 85 ~ TRUE,
      criterion == 'NS_accuracy' & mean_perf_overall >= 85 ~ FALSE,
      criterion == 'NS_mean_RT' & mean_perf_overall > 650 ~ TRUE,
      criterion == 'NS_mean_RT' & mean_perf_overall <= 650 ~ FALSE,
      criterion == 'NS_mean_RTdiff' ~ FALSE,
      criterion == 'SL_accuracy' & mean_perf_overall < 20 ~ TRUE,
      criterion == 'SL_accuracy' & mean_perf_overall >= 20 ~ FALSE,
      criterion == 'SR_accuracy' & mean_perf_overall < 20 ~ TRUE,
      criterion == 'SR_accuracy' & mean_perf_overall >= 20 ~ FALSE,
      criterion == 'SB_accuracy' & mean_perf_overall < 20 ~ TRUE,
      criterion == 'SB_accuracy' & mean_perf_overall >= 20 ~ FALSE,
      criterion == 'IG_accuracy' & mean_perf_overall < 80 ~ TRUE,
      criterion == 'IG_accuracy' & mean_perf_overall >= 80 ~ FALSE),
      mean_perf_overall = round(mean_perf_overall, digits = 0) # For plotting purposes
      )

  # Assess blockwise task performance - maximum number of consecutive failures
  blockwise_performance <-
    data %>%
    dplyr::group_by(subjectIx, criterion) %>%
    dplyr::mutate(n_consec_failures = cumsum_reset(as.integer(failed))) %>%
    dplyr::summarize(max_n_consec_failures = max(n_consec_failures),
                     failed_blockwise_crit = max_n_consec_failures >=5
    )

  out <- dplyr::left_join(overall_performance,
                          blockwise_performance,
                          by = c("subjectIx", "criterion")
                          )

  out$subjectIx <- factor(out$subjectIx)
  out


}
