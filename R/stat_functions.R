#' Compute stop-signal reactione time
#'
#' Stop-signal reaction time (SSRT) can be computed according to one of the
#' following methods:
#' - integration method of the independent race model (Logan & Cowan, Psych Rev,
#' 1984)
#' - mean method (Logan & Cowan, Psych Rev, 1984)
#' - modified integration method ()
#'
#' Integration method ----------------------------------------------------------
#' - Compute probability of responding given a stop-signal across delays (staircase) or for each delay separately
#' (no staircase)
#' - Estimate finishing time: integrate the no-signal response time distribution up to the point corresponding to the probability of responding given a stop-signal
#' - Subtract mean delay (staircase) or delay (no staircase)
#'
#' Mean method -----------------------------------------------------------------
#' - Compute probability of responding given a stop-signal across delays (staircase) or for each delay separately
#' (no staircase)
#' - Estimate finishing time: mean no-signal response time
#'
#' Modified integration method -------------------------------------------------
#' - Compute probability of responding
#'
#' @export
compute_ssrt <- function(tibb, for_each_ssd = FALSE) {

  #Include input args: method, rt_col, ssd_col, accuracy_col, staircase

  ssds <- unique(tibb$t_d[!is.na(tibb$t_d)])

  tibb <-
    tibb %>%
    dplyr::mutate(trial_alt = factor(trial_alt,
                                     levels = c("NS", "SAS", "SSS")),
                  trial = factor(trial,
                                 levels = c("NS", "SL", "SR", "SB", "IG")),
                  t_d = factor(t_d,
                               levels = ssds)
                  )

  # Compute probability of responding ------------------------------------------
  p_respond <-
    tibb %>%
    # dplyr::filter(trial %in% c("SB", "IG"),
    dplyr::filter(trial %in% c("SL", "SR", "SB"),
                  trial_performance %in% c("SB_stop-inhibit",
                                           "SB_stop-respond-bi",
                                           "SL_stop-inhibit",
                                           "SL_stop-respond-bi",
                                           "SR_stop-inhibit",
                                           "SR_stop-respond-bi"
                                           )
                  )
  if (for_each_ssd) {
    p_respond <-
      p_respond %>%
      # No staircase: we want p_respond for each trial type and delay
      dplyr::group_by(trial_alt, t_d)
  } else {
    p_respond <-
      p_respond %>%
      # Staircase: we want p_respond for each trial type
      dplyr::group_by(trial_alt)
  }
  p_respond <-
    p_respond %>%
    tidyr::nest() %>%
    # Compute probability of responding given a signal for each group
    dplyr::mutate(p_respond = purrr::pmap_dbl(.l = list(data = .$data),
                                          .f = function(data) {
                                            1 - mean(data$trialCorrect)
                                            }),
                  true_ssrt = purrr::pmap_dbl(.l = list(data = .$data),
                                              .f = function(data) {
                                                median(data$SSRT_true, na.rm = TRUE)
                                              })
                  )
  if (for_each_ssd) {
    p_respond <-
      p_respond %>%
      dplyr::mutate(t_d = as.numeric(levels(t_d))[t_d])

  } else {
    p_respond <-
      p_respond %>%
      # Compute mean t_d
      dplyr::mutate(t_d = purrr::pmap_dbl(.l = list(data = .$data),
                                          .f = function(data) {
                                            mean(as.numeric(levels(data$t_d))[data$t_d])
                                          }))
    }

  p_respond <-
    p_respond %>%
    # Keep relevant columns only
    dplyr::select(-data)

  # Estimate finishing time ----------------------------------------------------

  no_signal_rt <-
    tibb %>%
    dplyr::filter(trial_alt == 'NS',
                  r_bi == TRUE) %>%
    dplyr::select(RT_trial) %>%
    dplyr::pull()

  # ignore_rt <-
  #   tibb %>%
  #   dplyr::filter(trial == 'IG') %>%
  #   # dplyr::filter(trial_alt == 'NS',
  #   #               r_bi == TRUE) %>%
  #   dplyr::select(RT_trial) %>%
  #   dplyr::pull()

  p_respond <-
    p_respond %>%
    dplyr::mutate(ft = quantile(no_signal_rt, probs = p_respond, na.rm = TRUE) * 1000,
                  ssrt = ft - t_d)


  p_respond

  # Compare with true SSRT
  # tibb %>%
  #   dplyr::filter(trial %in% c("SL", "SR", "SB"),
  #                 trial_performance %in% c("SL_stop-inhibit",
  #                                          "SR_stop-inhibit")
  #   ) %>%
  #   dplyr::group_by(trial_alt) %>%
  #   dplyr::summarize(count = n(),
  #                    median_SSRT_true = median(SSRT_true))

  # tibb %>%
  #   dplyr::filter(trial %in% c("SL", "SR", "SB"),
  #                 trial_performance %in% c("SB_stop-inhibit",
  #                                          "SB_stop-respond-bi",
  #                                          "SL_stop-inhibit",
  #                                          "SL_stop-respond-bi",
  #                                          "SR_stop-inhibit",
  #                                          "SR_stop-respond-bi"
  #                                          )
  #                 ) %>%
  #   dplyr::group_by(trial_alt) %>%
  #   dplyr::summarize(count = n(),
  #                    median_SSRT_true = median(SSRT_true, na.rm = TRUE))
}

#' Derive Bayesian logistic regression predictionsgiven best-fitting parameters
#'
#' @param tibb Tibble containing columns subject identifier (subjectIx), best-fitting parameters (mean_beta0, mean_beta), and Bayes factor (B)
#' @export
derive_bayesian_logistic_regression_preds <- function(tibb, scaled_t_ds) {

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
                                 x = seq(from = min(scaled_t_ds),
                                         to = max(scaled_t_ds),
                                         length.out = 100)
    ),
    by = 'subjectIx') %>%
    dplyr::mutate(y = logistic(beta0 + beta * x),
                  subjectIx = factor(subjectIx))

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
#' @param tibb, tibble containing data on subjectIx trial_alt t_d and r_bi
#' @param figures_dir, path to the directory for saving figures
#' @param notebook_name, name of the notebook
#' @param modeling_approach, whether using kruscke methods or brms to fit models
#' @param rscale, width of the prior (in standardized effect size units)
#' @export
test_if_idv <- function(tibb, stopping_type, derivatives_dir, figures_dir, notebook_name, modeling_approach = 'brms', rscale = 0.5) {

  brms_bayesian_logistic_regression <-
    function(df, derivatives_dir, notebook_name, tag) {

      require(Rcpp)
      require(rstan)

      # Priors =====================================================================

      # We use a Cauchy prior, with scaling parameter of 0.5 (see preregistration)
      h1_prior <-
        c(brms::set_prior(sprintf("cauchy(0, %f)", rscale), class = 'Intercept'),
          brms::set_prior(sprintf("cauchy(0, %f)", rscale), class = 'b'))

      # N.B. Intercept free to vary and, implicitly, b = 0
      h0_prior <-
        c(brms::set_prior(sprintf("cauchy(0, %f)", rscale), class = 'Intercept'))

      # H0 model ===================================================================

      # Fit model
      m_0 <-
        brms::brm(
          r_bi ~ 1,
          data = df,
          prior = h0_prior,
          family = "bernoulli",
          seed = 19821101,
          sample_prior = TRUE,
          save_all_pars = TRUE
          # save_model = file.path(derivatives_dir, notebook_name, tag,
          #                        "stan_model_code_p_r_bi_m_0.txt"),
          # file = file.path(derivatives_dir, notebook_name, tag,
          #                  "fitted_model_object_p_r_bi_m_0")
        )

      # H1 model ===================================================================
      m_td <-
        brms::brm(
          r_bi ~ 1 + t_d,
          data = df,
          prior = h1_prior,
          family = "bernoulli",
          seed = 19821101,
          sample_prior = TRUE,
          save_all_pars = TRUE
          # save_model = file.path(derivatives_dir, notebook_name, tag,
          #                        "stan_model_code_p_r_bi_m_td.txt"),
          # file = file.path(derivatives_dir, notebook_name, tag,
          #                  "fitted_model_object_p_r_bi_m_td")
          )

      # Model comparison  ===================================================
      # Compute the Bayes factor

      BF01brms <- brms::bayes_factor(m_0, m_td, log = TRUE)$bf

      # Put the relevant stats in a tibble
      bf_sub <-
        tibble::tibble(model = "B01",
                       B = exp(BF01brms),
                       logB = BF01brms,
                       label = cut(exp(BF01brms),
                                   breaks = get_bf_settings("breaks"),
                                   labels = get_bf_settings("labels")),
                       mean_beta0 = brms::fixef(m_td)["Intercept","Estimate"],
                       mean_beta = brms::fixef(m_td)["t_d","Estimate"]
                       )
    }


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
                                   logB = log(BF01_savage),
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
                             logB = log(BF01_savage),
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
                   logB = double(),
                   label = character(),
                   mean_beta0 = double(),
                   mean_beta = double()
    )

  # Loop over participants
  for (i_subject in unique(tibb$subjectIx)) {

    tag <- sprintf('%s_sub-%.02d', stopping_type, as.integer(i_subject))

    # Print which subject is being processed, so that notebook output can be parsed more easily
    print(tag)

    # Select data
    df_sub <-
      tibb %>%
      dplyr::filter(subjectIx == i_subject) %>%
      dplyr::select(t_d,r_bi) %>%
      as.data.frame(.)

    if (modeling_approach == "kruschke") {

      # Perform Bayesian logistic regression and compute BF with Savage-Dickey density method
      # N.B. Graphics showing diagnostics and best-fits are written to disk
      bf_sub <-
        bayesian_logistic_regression(df = df_sub,
                                     y_name = 'r_bi',
                                     x_name = 't_d',
                                     file_name_root = file.path(figures_dir,
                                                                notebook_name,
                                                                tag),
                                     graph_file_type = 'eps')

      # Store descriptive and inferential statistics in data frame
      bf_and_params <-
        tibble::add_row(bf_and_params,
                        subjectIx = as.integer(i_subject),
                        model = bf_sub$bf$model,
                        B = bf_sub$bf$B,
                        logB = bf_sub$bf$logB,
                        label = bf_sub$bf$label,
                        mean_beta0 = bf_sub$params['beta0','Mean'],
                        mean_beta = bf_sub$params['beta','Mean']
                        )

    } else if (modeling_approach == "brms") {

      bf_sub <-
        brms_bayesian_logistic_regression(df = df_sub,
                                          derivatives_dir = derivatives_dir,
                                          notebook_name = notebook_name,
                                          tag = tag)

      bf_and_params <-
        tibble::add_row(bf_and_params,
                        subjectIx = as.integer(i_subject),
                        model = bf_sub$model,
                        B = bf_sub$B,
                        logB = bf_sub$logB,
                        label = bf_sub$label,
                        mean_beta0 = bf_sub$mean_beta0,
                        mean_beta = bf_sub$mean_beta
                        )


    }

  }

  # Output
  bf_and_params %>%
    dplyr::mutate(subjectIx = factor(subjectIx))

}

#' Test of inhibition function for group-level data
#'
#' Tests the independent race model's qualitative prediction that the probability of responding given a stop-signal increases as a function of stop-signal delay, in group-level data.
#' @param tibb tibble containing subject-level data, including Bayesian logistic regression parameter estimates
#' @param par_name name of the parameter containing the estimate of the logistic regression slope
#' @export
test_if_grp <- function(tibb, stopping_type, derivatives_dir, figures_dir, notebook_name) {

  df <-
    tibb %>%
    as.data.frame()

  save_diagnostic_plots <- function(mdl, mdl_name) {

    mcmc_diagnostics_m0 <- irmass::plot_mcmc_analysis(mdl)
    posterior_m0 <- irmass::plot_posterior(mdl)
    ggplot2::ggsave(filename = sprintf("traceplot_%s_%s.pdf", stopping_type, mdl_name),
                    path = file.path(derivatives_dir, notebook_name),
                    plot = mcmc_diagnostics_m0[[1]])

    ggplot2::ggsave(filename = sprintf("Rhat_%s_%s.pdf", stopping_type, mdl_name),
                    path = file.path(derivatives_dir, notebook_name),
                    plot = mcmc_diagnostics_m0[[2]])

    ggplot2::ggsave(filename = sprintf("Rhat_%s_%s.pdf", stopping_type, mdl_name),
                    path = file.path(derivatives_dir, notebook_name),
                    plot = posterior_m0)
  }


  # Specify priors -------------------------------------------------------------
  # We use a Cauchy prior, with scaling parameter of 0.5 (see preregistration)
  model_priors <-
    c(brms::set_prior("cauchy(0, 0.5)", class = 'Intercept'),
      brms::set_prior("cauchy(0, 0.5)", class = 'b'))


  # Fit null model -------------------------------------------------------------
  m_0 <-
    brms::brm(
      resp ~ (1 + subjectIx),
      data = df,
      prior = model_priors,
      family = "bernoulli",
      seed = 19821101,
      sample_prior = TRUE,
      save_all_pars = TRUE
    )

  # Save plots: traces, Rhat, and posterior
  save_diagnostic_plots(mdl = m_0, mdl_name = "m_0")

  # Fit t_d model --------------------------------------------------------------
  m_td <-
    brms::brm(
      resp ~ t_d + (1 + subjectIx),
      data = df,
      prior = model_priors,
      family = "bernoulli",
      seed = 19821101,
      sample_prior = TRUE,
      save_all_pars = TRUE
    )

  # Save plots: traces, Rhat, and posterior
  save_diagnostic_plots(mdl = m_td, mdl_name = "m_td")

  # Model comparison  ----------------------------------------------------------
  BF01 <- brms::bayes_factor(m_0, m_td, log = TRUE)$bf

  # Make a tibble for storing descriptive and inferential statistics -----------
  bf_and_params <-
    tibble::tibble(model = character(),
                   B = double(),
                   logB = double(),
                   label = character(),
                   mean_beta0 = double(),
                   mean_beta = double()
    )

  bf_and_params <-
    tibble::add_row(bf_and_params,
                    model = "B01",
                    B = exp(BF01),
                    logB = BF01,
                    label = cut(exp(BF01),
                                breaks = get_bf_settings("breaks"),
                                labels = get_bf_settings("labels")),
                    mean_beta0 = brms::fixef(m_td)["Intercept","Estimate"],
                    mean_beta = brms::fixef(m_td)["t_d","Estimate"]
                    )

}

#' Test of stop-respond RT vs. no-signal RT for individual-level data
#'
#' Tests the independent race model's qualitative prediction that stop-respond RT should be shorter than no-signal, in individual-level data.
#'
#' @param tibb tibble with trial-level data
#' @param trial_alt_levels 2-element character vector containing the levels to contrast
#' @export
test_srrt_vs_nsrt_idv <- function(tibb, trial_alt_levels, rscale = get_bf_settings('rscale')) {

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
                                     rscale = rscale
                                     )
               )

    # Take the inverse (i.e. Evidence for Null vs. Evidence for Alt. 1)
    B01_data <- purrr::map(B10_data, ~1/.[1])

    # Note that Bayes Factors are stored as log BF, so we need to take exponential
    B01 <- B01_data %>% purrr::map_dbl(~.@bayesFactor$bf) %>% base::exp(.)

    # Put the relevant stats in a tibble
    tibble::tibble(
      subjectIx = factor(as.integer(names(B10_data))),
      model = "B01",
      B = B01,
      logB = log(B01),
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
    logB = log(B01),
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
test_effect_ssd_on_srrt_idv <- function(tibb, rscale = get_bf_settings('rscale')) {

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
                           rscaleFixed = rscale
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
                              logB = numeric(),
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
                                 logB = log(1 / exp(B_full_vs_null[mdl]@bayesFactor$bf)),
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
                                     logB = log(B_null_vs_order_restricted),
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
