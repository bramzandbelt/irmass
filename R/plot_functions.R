#' Get colorbar variables for visualizaing Bayes factor data
#'
#' @export
get_bf_colorbar_vars <- function() {

  # Bayes factor category boundaries
  bf_breaks <- get_bf_settings('breaks')

  breaks <- bf_breaks[2:(length(bf_breaks) - 1)]
  labels <- fancy_scientific(breaks)
  limits <- range(breaks)

  list(breaks = breaks,
       labels = labels,
       limits = limits
       )
}

#' Convert double to scientific notation-formatted string for annotation purposes
#'
#' @param dbl double
#' @export
fancy_scientific <- function(dbl) {

  # Assertions
  assertthat::assert_that(is.double(dbl))

  chr <- vector(,length = length(dbl))

  for (i in 1:length(dbl)) {
    chr[i] <- ifelse(dbl[i] >= 1000,
                     format(dbl[i], scientific = TRUE, digits = 2, nsmall = 2),
                     ifelse(dbl[i] >= 100,
                            format(dbl[i], scientific = FALSE, digits = 1, nsmall = 0),
                            ifelse(dbl[i] >= 10,
                                   format(dbl[i], scientific = FALSE, digits = 1, nsmall = 1),
                                   ifelse(dbl[i] >= 1,
                                          format(dbl[i], scientific = FALSE, digits = 1 , nsmall = 2),
                                          ifelse(dbl[i] >= 0.01,
                                                 format(dbl[i], scientific = FALSE, digits = 1, nsmall = 3),
                                                 format(dbl[i], scientific = TRUE, digits = 2, nsmall = 2)
                                          )
                                   )
                            )
                     )
    )

  }

  # turn in to character string in scientific notation
  # chr <- format(dbl, scientific = TRUE, digits = 2)

  # quote the part before the exponent to keep all the digits
  chr <- gsub("^(.*)e", "'\\1'e", chr)

  # turn the 'e+' into plotmath format & return
  chr <- gsub("e", "%*%10^", chr)

  for (i in 1:length(chr)) {

    if (grepl('\\%10\\^',chr[i])) {
      # chr[i] = parse(text = chr[i])
    } else {
      chr[i] <- paste0("'",chr[i],"'")
    }
  }

  chr
}

#' Rank subjects based on the Bayes Factor
#'
#' @param tibb Tibble containing columns named subjectIx and B (Bayes Factors)
rank_by_bayes_factor <- function(tibb, bvar = "B") {

  # Determine ranking ----------------------------------------------------------
  ranking <-
    tibb %>%
    dplyr::select_("subjectIx",
                   bvar) %>%
    dplyr::rename_("B" = bvar) %>%
    dplyr::arrange(., desc(B))

  # Based on ranking, determine subject levels and labels ----------------------
  subject_levels <- as.vector(ranking$subjectIx)
  subject_labels <- paste0(sprintf("s%02d",as.double(subject_levels)),
                           " \n ",
                           sprintf("B = %.02f", as.vector(ranking$B))
  )

  # Reorder subjects based on Bayes factor -------------------------------------
  tibb$subjectIx <- factor(tibb$subjectIx,
                           levels = rev(unique(subject_levels)))

  # Output ---------------------------------------------------------------------
  tibb
}

#' Get default ggplot theme settings to be used in this project
#' @export
theme_irmass <- function() {

  title_font_size <- 16
  subtitle_font_size <- 12
  label_font_size <- 10

  ggthemes::theme_few() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold",
                                         size = title_font_size,
                                         hjust = 0.5),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = label_font_size),
      axis.text.x = ggplot2::element_text(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = subtitle_font_size),

      legend.background = ggplot2::element_rect(),
      legend.text = ggplot2::element_text(size = label_font_size),
      legend.title = ggplot2::element_text(size = subtitle_font_size),

      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.01,"in"),

      strip.background = ggplot2::element_rect(),
      strip.text = ggplot2::element_text(size = label_font_size),
      # strip.background = ggplot2::element_blank(),
      # strip.text = ggplot2::element_blank()

    )
}

#' Plots task performance overview, for assessment of performance criteria
#'
#' Plotting can be done for practice and experimental stage.
#'
#' @param data tibble containing data
#' @param xvar x axis variable
#' @param xvardesc x axis variable
#' @param yorder criterion level order
#' @param yticklabels criterion level description
#' @param fillvar fill variable
#' @param fillvardesc fill variable description
#' @param labelvar label variable
#' @param facetwrapvar facet_wrap variable(s), if any
#' @param facetgridvar facet_grid variable(s), if any
#'  @param plottitle plot title, if any
#' @export
plot_overview <- function(data, xvar, xvardesc, yorder, yticklabels, fillvar, fillvardesc, labelvar, facetwrapvar, facetgridvar, plottitle) {

  # Main variables (x, y, fill, label) -----------------------------------------
  plt <- ggplot2::ggplot(data,
                         ggplot2::aes_string(x = xvar,
                                             y = 'criterion',
                                             fill = fillvar
                                             )
                         )
  # Label variable, if any -----------------------------------------------------
  if (!missing(labelvar)) {
    plt <- plt + ggplot2::aes_string(label = labelvar)
    # plt <- plt + ggplot2::aes(label = sprintf('%0.0f', paste0('data$',labelvar)))

  }

  # Facets ---------------------------------------------------------------------
  if (!missing(facetwrapvar)) {
    plt <- plt + ggplot2::facet_wrap(facetwrapvar)
  } else if (!missing(facetgridvar)) {
    plt <- plt + ggplot2::facet_grid(reformulate(facetgridvar[2],facetgridvar[1]))
  }

  # Geoms  ---------------------------------------------------------------------
  plt <- plt + ggplot2::geom_tile(na.rm = TRUE, size = 2)

  if (!missing(labelvar)) {
    plt <- plt + ggplot2::geom_text(size = 2, color = "white", check_overlap = TRUE)
  }

  # Scales  --------------------------------------------------------------------
  plt <- plt + ggplot2::scale_fill_manual(values = c("#008000","#FF0000"),
                                          guide = ggplot2::guide_legend(
                                            title.position = "top",
                                            title.hjust = 0.5),
                                          name = fillvardesc
                                          )

  # coord_equal
  plt <- plt + ggplot2::coord_equal()

  # x and y
  plt <- plt + ggplot2::scale_x_discrete()
  plt <- plt + ggplot2::scale_y_discrete(drop = TRUE,
                                         limits = rev(levels(data$criterion)),
                                         labels = yticklabels)


  # Plot title  ----------------------------------------------------------------
  if (!missing(plottitle)) {
    plt <- plt + ggplot2::ggtitle(plottitle)
  }

  # Axes labels  ---------------------------------------------------------------
  plt <- plt + ggplot2::xlab(xvardesc) + ggplot2::ylab('Criterion')

  # Theme  ---------------------------------------------------------------------

  plt <- plt + irmass::theme_irmass()
  plt <- plt + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                                  hjust = 0.5,
                                                                  size = 3)
                              )
  plt
}

#' Function for plotting response time and Bayes factor data from individual-level analyses
#'
#' This function is used to compare response times between signal-respond and no-signal trials and to compare response times on signal-respond trials across different stop-signal delay categories.
#'
#' @param trial_data tibble with trial data
#' @param summary_data tibble with trial-averaged summary data
#' @param colorbar Color bar variables
#' @param xvar x axis variable
#' @param bvar Bayes factor variable
#' @param plot_orientation horizontal or vertical
#' @param bf_as_background whether or not plot background should encodes Bayes Factor
plot_idv_rt_data_bf <- function(trial_data, bf_data, summary_data, colorbar, xvar, bvar, plot_orientation, bf_as_background) {

  # Main variables (x, y, fill, label) -----------------------------------------
  plt <- ggplot2::ggplot(trial_data,
                         ggplot2::aes_string(x = xvar,
                                             y = 'RT_trial')
  )

  # Facets ---------------------------------------------------------------------
  if (plot_orientation == 'vertical') {

    plt <- plt + ggplot2::facet_wrap("subjectIx",
                                     nrow = 2
    )

  } else if (plot_orientation == 'horizontal') {
    plt <- plt + ggplot2::facet_wrap("subjectIx",
                                     ncol = 4
    )
  }



  # Geoms  ---------------------------------------------------------------------
  # Bayes factor

  if (xvar == 'trial_alt') {
    x = 1.5
    if (plot_orientation == 'vertical') {
      labels = c('S-R','N-S')
    } else if (plot_orientation == 'horizontal') {
      labels = c('stop-respond','no-signal')
    }
    x_axis_name = 'Trial type'
  } else if (xvar == 't_d_alt') {
    x = 2

    if (plot_orientation == 'vertical') {
      labels = c('S', 'I', 'L')
    } else if (plot_orientation == 'horizontal') {
      labels = c('short', 'intermediate', 'long')
    }

    x_axis_name = 'Stop-signal delay category'
  }


  # Shape depends on plot orientation
  if (plot_orientation == 'vertical') {
    shape_id = 21
  } else if (plot_orientation == 'horizontal') {
    shape_id = 21
  }

  if (bf_as_background) {
    plt <-
      plt +

      # Background
      ggplot2::geom_rect(ggplot2::aes_string(fill = bvar),
                         xmin = -Inf,
                         xmax = Inf,
                         ymin = -Inf,
                         ymax = Inf,
                         alpha = 0.3
      )

  }

  plt <-
    plt +

    # Individual trial RT data
    ggbeeswarm::geom_quasirandom(fill = NA,
                                 shape = 42, # asterisk
                                 size = 3,
                                 stroke = 0.25,
                                 alpha = 0.50 #0.25
    ) +

    # Trial mean RT data
    ggplot2::geom_point(data = summary_data,
                        ggplot2::aes_string(x = xvar,
                                            y = 'mean_RT_trial'),
                        shape = shape_id,
                        size = 1.5,
                        stroke = 1.5,
                        color = "white",
                        fill = "black"
    )

    if (plot_orientation == 'vertical') {
      plt <-
        plt +
        switch(bvar,
               'B' = ggplot2::geom_text(data = bf_data,
                                        ggplot2::aes(label = fancy_scientific(B)),
                                        x = x,
                                        y = 0.2,
                                        size = 2.5,
                                        parse = TRUE
               ),
               'B_null_vs_order_restricted' = ggplot2::geom_text(data = bf_data,
                                                                 ggplot2::aes(label = fancy_scientific(B_null_vs_order_restricted)),
                                                                 x = x,
                                                                 y = 0.2,
                                                                 size = 2.5,
                                                                 parse = TRUE
               )
        )

    } else if (plot_orientation == 'horizontal') {
      plt <-
        plt +
        ggplot2::geom_text(mapping = ggplot2::aes(label = subjectIx),
                           x = 1,
                           y = Inf,
                           hjust = 1,
                           vjust = 1,
                           size = 2.5,
                           fontface = "plain"
                           )

    }

  # Scales  --------------------------------------------------------------------

  if (bf_as_background) {

    plt <-
      plt +

      switch(bvar,
             'B' = ggplot2::scale_fill_gradient2(midpoint = 0,
                                                 # Colorblind-friendly colors
                                                 low = "#91bfdb",
                                                 mid = "#ffffbf",
                                                 high = "#fc8d59",
                                                 # name = expression(log['10'](B['01'])),
                                                 name = expression(B['01']),
                                                 breaks = colorbar$breaks,
                                                 labels = parse(text = colorbar$labels),
                                                 # labels = colorbar$labels,
                                                 limits = colorbar$limits,
                                                 trans = 'log10',
                                                 oob = scales::squish
             ),
             'B_null_vs_order_restricted' = ggplot2::scale_fill_gradient2(midpoint = 0,
                                                                          # Colorblind-friendly colors
                                                                          low = "#91bfdb",
                                                                          mid = "#ffffbf",
                                                                          high = "#fc8d59",
                                                                          # name = expression(log['10'](B['01'])),
                                                                          name = expression(B['0 vs. order-restricted']),
                                                                          breaks = colorbar$breaks,
                                                                          labels = parse(text = colorbar$labels),
                                                                          # labels = colorbar$labels,
                                                                          limits = colorbar$limits,
                                                                          trans = 'log10',
                                                                          oob = scales::squish
             ))

  }


  if (xvar == 't_d_alt' & plot_orientation == 'horizontal') {
    plt <-
      plt +
      ggplot2::scale_x_discrete(name = x_axis_name,
                                labels = labels,
                                limits = rev(levels(xvar)))
  } else {
    plt <-
      plt +
      ggplot2::scale_x_discrete(name = x_axis_name,
                                labels = labels)
  }

  if (plot_orientation == 'horizontal') {

    plt <-
      plt +
      ggplot2::scale_y_continuous(name = "Response time (s)",
                                  breaks = seq(from = 0.4,to = 1, by = 0.2),
                                  limits = c(0.2,1.2))

  } else if (plot_orientation == 'vertical') {

    plt <-
      plt +
      ggplot2::scale_y_continuous(name = "Response time (s)",
                                  breaks = seq(from = 0.4,to = 1, by = 0.2),
                                  limits = c(0.2,1.2))
    }
    # Theme  ---------------------------------------------------------------------

  plt <-
    plt +
    irmass::theme_irmass()

  if (xvar == 'trial_alt') {

    if (plot_orientation == 'horizontal') {
      plt <-
        plt +
        ggplot2::theme(strip.background = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank(),
                       panel.spacing.x = ggplot2::unit(0.04,"in"),
                       panel.spacing.y = ggplot2::unit(0.04,"in"),
                       legend.key.width = ggplot2::unit(6,"line"),
                       legend.position = "bottom",
                       legend.text.align = 0.5
                       ) +
        ggplot2::coord_flip()
    } else if (plot_orientation == 'vertical') {
      plt <-
        plt + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                                 hjust = 1),
                             panel.spacing.x = ggplot2::unit(0.04,"in"),
                             panel.spacing.y = ggplot2::unit(0.04,"in")
                             )
    }

  } else if (xvar == 't_d_alt') {

    if (plot_orientation == 'horizontal') {
      plt <-
        plt +
        ggplot2::theme(strip.background = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank(),
                       panel.spacing.x = ggplot2::unit(0.04,"in"),
                       panel.spacing.y = ggplot2::unit(0.04,"in"),
                       legend.key.width = ggplot2::unit(6,"line"),
                       legend.position = "bottom",
                       legend.text.align = 0.5
        ) +
        ggplot2::coord_flip()
    } else if (plot_orientation == 'vertical') {
      plt <-
        plt + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,
                                                                 hjust = 0.5),
                             panel.spacing.x = ggplot2::unit(0.04,"in"),
                             panel.spacing.y = ggplot2::unit(0.04,"in")
        )
    }

  }

}


#' Plot inhibition function for individual-level data
#'
#' Plots the probability of responding given a stop-signal against stop-signal delay, a histogram of the number stop-inhibit and stop-respond trials for each stop-signal delay, and a logistic regression fit to the data.
#' @param obs_data tibble with observed response probabilities
#' @param prd_data tibble with predicted response probabilities
#' @param bf_data Bayes factor data
#' @param colorbar Color bar variables
#' @param yvar y axis variable
#' @param bvar Bayes factor variable
#' @export
plot_if_idv <- function(obs_data, prd_data, bf_data, colorbar, yvar, bvar) {

  # Add Bayes Factor data to tibble with observed response probabilities
  obs_data <-
    obs_data %>%
    dplyr::left_join(bf_data,
                     by = 'subjectIx')

  bf_data <- rank_by_bayes_factor(bf_data)
  obs_data <- rank_by_bayes_factor(obs_data)
  prd_data <- rank_by_bayes_factor(prd_data)

  # Main variables (x, y, fill, label) -----------------------------------------
  plt <- ggplot2::ggplot(obs_data,
                         ggplot2::aes_string(x = 't_d',
                                             y = yvar)
  )

  # Facets ---------------------------------------------------------------------
  plt <- plt + ggplot2::facet_wrap("subjectIx",
                                   nrow = 8
  )

  # Geoms  ---------------------------------------------------------------------

  plt <-
    plt +

    # Background
    ggplot2::geom_rect(ggplot2::aes_string(fill = bvar),
                       xmin = -Inf,
                       xmax = Inf,
                       ymin = -Inf,
                       ymax = Inf,
                       alpha = 0.3
    ) +

    # Observed probability of responding given a stop-signal (open circles)
    ggplot2::geom_point(shape = 1,
                        size = 2) +

    # Predicted probability of responding given a stop-signal (line)
    ggplot2::geom_line(data = prd_data,
                       ggplot2::aes(x = x,
                                    y = y)
                       ) +
    ggplot2::geom_text(mapping = ggplot2::aes(label = subjectIx),
                       x = Inf,
                       y = 0,
                       hjust = 1,
                       vjust = 0,
                       size = 2.5,
                       fontface = "plain"
                       )

    # ggplot2::geom_text(data = bf_data,
    #                    ggplot2::aes(label = fancy_scientific(B)),
    #                    x = 0.75,
    #                    y = 0,
    #                    hjust = 1,
    #                    vjust = 0,
    #                    size = 2,
    #                    parse = TRUE
    #                    )


  # Scales  --------------------------------------------------------------------

  plt <-
    plt +

    ggplot2::scale_fill_gradient2(midpoint = 0,
                                  # Colorblind-friendly colors
                                  low = "#91bfdb",
                                  mid = "#ffffbf",
                                  high = "#fc8d59",
                                  # name = expression(log['10'](B['01'])),
                                  name = expression(B['01']),
                                  breaks = colorbar$breaks,
                                  labels = parse(text = colorbar$labels),
                                  # labels = colorbar$labels,
                                  limits = colorbar$limits,
                                  trans = 'log10',
                                  oob = scales::squish
    ) +

    ggplot2::scale_x_continuous(name = 'Stop-signal delay (s)',
                                breaks = sort(unique(obs_data$t_d))[c(2,4)],
                                labels = c("0.166", "0.366"),
                                limits = c(0,0.5)
                                ) +

    ggplot2::scale_y_continuous(name = 'P(bimanual response | stop-signal)',
                                breaks = seq(from = 0.25,to = .75, by = 0.5),
                                limits = c(0,1)
    )

  # Theme  ---------------------------------------------------------------------

  plt <-
    plt +

    irmass::theme_irmass() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.04,"in"),
                   panel.spacing.y = ggplot2::unit(0.04,"in"),
                   legend.key.width = ggplot2::unit(6,"line"),
                   legend.position = "bottom",
                   legend.text.align = 0.5,
                   axis.text.x = ggplot2::element_text(angle = 0,
                                                       hjust = 0.5
                                                       )
                   )



}

#' Plot inhibition function for group-level data
#' @param obs tibble containing observations
#' @param prd tibble containing predicted parameter estimates from Bayesian logistic regression model
#' @export
plot_if_grp <- function(obs, prd) {

  idv_alpha = 0.2

  ggplot2::ggplot(data = obs,
                  ggplot2::aes(x = t_d,
                               y = p_respond)
  ) +

    ggbeeswarm::geom_quasirandom(shape = 1,
                                 size = 2,
                                 alpha = idv_alpha,
                                 width = .05) +

    # Predicted probability of responding given a stop-signal (line)
    ggplot2::geom_line(data = prd,
                       ggplot2::aes(x = t_d,
                                    y = p_respond,
                                    group = subjectIx),
                       alpha = idv_alpha
    ) +

    ggplot2::stat_summary(data = prd,
                          fun.y = mean,
                          geom = "line",
                          size = 2,
                          linetype = 'solid',
                          na.rm = TRUE) +

    ggplot2::scale_x_continuous(name = 'Stop-signal delay (s)',
                                breaks = sort(unique(obs$t_d)),
                                labels = c("0.066", "0.166", "0.266", "0.366", "0.466")
    ) +

    ggplot2::scale_y_continuous(name = "P(respond | stop-signal)") +

    irmass::theme_irmass()

}

#' Plot stop-respond RT vs. no-signal RT for individual-level data
#'
#' @param trial_data tibble containing trial-level data
#' @param bf_data tibble containing subject-level Bayes factor data
#' @export
plot_srrt_vs_nsrt_idv <- function(trial_data, bf_data, plot_orientation = 'vertical', bf_as_background = TRUE) {

  # Get colorbar variables for visualizaing Bayes factor data
  colorbar <- get_bf_colorbar_vars()

  # Add Bayes factor data to trial_data tibble
  trial_data <-
    dplyr::left_join(trial_data,
                     bf_data %>% dplyr::select(subjectIx, B),
                     by = 'subjectIx') %>%
    dplyr::arrange(., desc(B))

  # Compute subject-level data
  summary_data <-
    trial_data %>%
    dplyr::group_by(subjectIx,trial_alt) %>%
    dplyr::summarize(mean_RT_trial = mean(RT_trial),
                     mean_RT_trial_inv = mean(RT_trial_inv)
    ) %>%
    dplyr::left_join(., bf_data, by = 'subjectIx') %>%
    dplyr::arrange(., desc(B))

  # Reorder subjects based on their Bayes factors
  bf_data <- rank_by_bayes_factor(bf_data)
  trial_data <- rank_by_bayes_factor(trial_data)
  summary_data <- rank_by_bayes_factor(summary_data)

  # ranking <-
  #   bf_data %>%
  #   dplyr::select(subjectIx,
  #                 B) %>%
  #   dplyr::arrange(., desc(B))
  #
  # subject_levels <- as.vector(ranking$subjectIx)
  # subject_labels <- paste0(sprintf("s%02d",as.double(subject_levels)),
  #                          " \n ",
  #                          sprintf("B = %.02f", as.vector(ranking$B))
  #                          )

  # # Reorder subjects based on Bayes factor
  # trial_data$subjectIx <- factor(trial_data$subjectIx,
  #                                levels = subject_levels)
  # bf_data$subjectIx <- factor(bf_data$subjectIx,
  #                             levels = subject_levels)
  # summary_data$subjectIx <- factor(summary_data$subjectIx,
  #                                  levels = subject_levels)

  plt <- plot_idv_rt_data_bf(trial_data = trial_data,
                             summary_data = summary_data,
                             bf_data = bf_data,
                             colorbar = colorbar,
                             xvar = 'trial_alt',
                             bvar = 'B',
                             plot_orientation = plot_orientation,
                             bf_as_background = bf_as_background)

}

#' Plot stop-respond RT vs. no-signal RT for group-level data
#'
#' @param data tibble in wide-format (each row is a subject) containing RT data
#' @param sr column name containing stop-respond response times
#' @param ns column name containing no-stop response times
#' @export
plot_srrt_vs_nsrt_grp <- function(data, sr, ns) {
  ggplot2::ggplot(data = data,
                  ggplot2::aes(x = data[[ns]],
                               y = data[[sr]]
                               # color = is_outlier
                               )
                  ) +

    ggplot2::geom_point(shape = 21) +
    ggplot2::geom_abline(intercept = 0,
                         slope = 1,
                         linetype = "dashed"
                         ) +
    ggplot2::geom_vline(xintercept = 0.65,
                        linetype = "dotted"
    ) +
    ggplot2::scale_x_continuous(name = "Mean N-S RT (s)",
                                limits = c(0.45, 0.75)
    ) +
    ggplot2::scale_y_continuous(name = "Mean S-R RT (s)",
                                limits = c(0.45, 0.75)
    ) +
    irmass::theme_irmass() +
    ggplot2::theme(aspect.ratio = 1)
}

#' Plot stop-respond RT vs. stop-signal delay for individual-level data
#'
#' @export
plot_srrt_vs_ssd_idv <- function(trial_data, bf_data, plot_orientation = 'vertical', bf_as_background = TRUE) {

  # Preliminaries  -------------------------------------------------------------

  # Get colorbar variables for visualizaing Bayes factor data
  colorbar <- get_bf_colorbar_vars()

  # Determine Bayes factor-based ranking (descending order) of subjects
  # ranking <-
  #   bf_data %>%
  #   dplyr::select(subjectIx,
  #                 B_null_vs_order_restricted) %>%
  #   dplyr::arrange(., desc(B_null_vs_order_restricted))
  #
  # subject_levels <- as.vector(ranking$subjectIx)
  # subject_labels <- paste0(sprintf("s%02d",as.double(subject_levels)),
  #                          " \n ",
  #                          sprintf("B = %.02f", as.vector(ranking$B_null_vs_order_restricted))
  # )

  # Add Bayes factor data to trial_data tibble
  trial_data <-
    dplyr::left_join(trial_data,
                     bf_data %>% dplyr::select(subjectIx, B_null_vs_order_restricted),
                     by = 'subjectIx') %>%
    dplyr::arrange(., desc(B_null_vs_order_restricted))

  # Compute subject-level data
  summary_data <-
    trial_data %>%
    dplyr::group_by(subjectIx,t_d_alt) %>%
    dplyr::summarize(mean_RT_trial = mean(RT_trial),
                     mean_RT_trial_inv = mean(RT_trial_inv)
    ) %>%
    dplyr::left_join(., bf_data, by = 'subjectIx') %>%
    dplyr::arrange(., desc(B_null_vs_order_restricted))

  # Reorder subjects based on their Bayes factors
  bf_data <- rank_by_bayes_factor(bf_data, bvar = 'B_null_vs_order_restricted')
  trial_data <- rank_by_bayes_factor(trial_data, bvar = 'B_null_vs_order_restricted')
  summary_data <- rank_by_bayes_factor(summary_data, bvar = 'B_null_vs_order_restricted')

  # # Reorder subjects based on Bayes factor
  # trial_data$subjectIx <- factor(trial_data$subjectIx,
  #                                levels = subject_levels)
  # bf_data$subjectIx <- factor(bf_data$subjectIx,
  #                             levels = subject_levels)
  # summary_data$subjectIx <- factor(summary_data$subjectIx,
  #                                  levels = subject_levels)

  # Plot the data
  plt <- plot_idv_rt_data_bf(trial_data = trial_data,
                             summary_data = summary_data,
                             bf_data = bf_data,
                             colorbar = colorbar,
                             xvar = 't_d_alt',
                             bvar = 'B_null_vs_order_restricted',
                             plot_orientation = plot_orientation,
                             bf_as_background = bf_as_background
                             )

  # # Main variables (x, y, fill, label) -----------------------------------------
  # plt <- ggplot2::ggplot(trial_data,
  #                        ggplot2::aes(x = t_d_alt,
  #                                     y = RT_trial)
  # )
  #
  # # Facets ---------------------------------------------------------------------
  # plt <- plt + ggplot2::facet_wrap("subjectIx",
  #                                  nrow = 2
  # )
  #
  # # Geoms  ---------------------------------------------------------------------
  # plt <-
  #   plt +
  #
  #   # Background
  #   ggplot2::geom_rect(ggplot2::aes(fill = B_null_vs_order_restricted),
  #                      xmin = -Inf,
  #                      xmax = Inf,
  #                      ymin = -Inf,
  #                      ymax = Inf,
  #                      alpha = 0.3
  #   ) +
  #
  #   # Individual trial RT data
  #   ggbeeswarm::geom_quasirandom(fill = NA,
  #                                shape = 42, # asterisk
  #                                size = 2,
  #                                stroke = 0.25,
  #                                alpha = 0.25
  #   ) +
  #
  #   # Trial mean RT data
  #   ggplot2::geom_point(data = summary_data,
  #                       ggplot2::aes(x = trial_alt,
  #                                    y = mean_RT_trial),
  #                       shape = 45,
  #                       size = 5,
  #                       color = '#ffffff'
  #   ) +
  #
  #   # Bayes factor of H0 vs H1
  #   ggplot2::geom_text(data = bf_data,
  #                      ggplot2::aes(label = fancy_scientific(B)),
  #                      x = 1.5,
  #                      y = 0.2,
  #                      size = 2,
  #                      parse = TRUE
  #   )
  #
  # # Scales  --------------------------------------------------------------------
  #
  # plt <-
  #   plt +
  #
  #   ggplot2::scale_fill_gradient2(midpoint = 0,
  #                                 # Colorblind-friendly colors
  #                                 low = "#91bfdb",
  #                                 mid = "#ffffbf",
  #                                 high = "#fc8d59",
  #                                 # name = expression(log['10'](B['01'])),
  #                                 name = expression(B['01']),
  #                                 breaks = colorbar_breaks,
  #                                 # labels = colorbar_labels,
  #                                 labels = parse(text = colorbar_labels),
  #                                 limits = colorbar_limits,
  #                                 trans = 'log10',
  #                                 oob = scales::squish
  #   ) +
  #
  #   ggplot2::scale_x_discrete(name = "Trial type"
  #   ) +
  #
  #   ggplot2::scale_y_continuous(name = "Response time (s)",
  #                               breaks = seq(from = 0.2,to = 1.2, by = 0.2),
  #                               minor_breaks = seq(from = 0.2,to = 1.2, by = 0.02),
  #                               limits = c(0.15,1.25)
  #   ) +
  #
  #
  #   # Theme  ---------------------------------------------------------------------
  #
  # plt <-
  #   plt +
  #
  #   irmass::theme_irmass()






  # ggplot2::ggplot(df,
  #                 ggplot2::aes(x = t_d_alt,
  #                              y = RT_trial)
  #                 ) +
  #
  #   ggplot2::facet_wrap("subjectIx",
  #                       nrow = 2
  #                       ) +
  #
  #   ggplot2::geom_rect(ggplot2::aes(fill = log10B),
  #                      xmin = -Inf,
  #                      xmax = Inf,
  #                      ymin = -Inf,
  #                      ymax = Inf,
  #                      alpha = 0.3
  #                      ) +
  #
  #   ggbeeswarm::geom_quasirandom(fill = NA,
  #                                shape = 42,
  #                                size = 2,
  #                                stroke = 0.25,
  #                                alpha = 0.5
  #                                ) +
  #
  #   ggplot2::geom_point(data = df_summary,
  #                       ggplot2::aes(x = t_d_alt,
  #                       y = mean_RT),
  #                       shape = 45,
  #                       size = 5
  #                       ) +
  #
  #   ggplot2::geom_text(data = df_summary,
  #                      ggplot2::aes(label = sprintf("log['10'](B['01']) == %.02f",log10B)),
  #                      x = 2,
  #                      y = 0.3,
  #                      size = 1,
  #                      parse = TRUE
  #                      ) +
  #
  #   ggplot2::scale_fill_gradient2(midpoint = 0,
  #                                 low = "#91bfdb",
  #                                 mid = "#ffffbf",
  #                                 high = "#fc8d59",
  #                                 name = expression(log['10'](B['01'])),
  #                                 breaks = colorbar_breaks,
  #                                 limits = colorbar_limits,
  #                        oob = squish) +
  #   scale_x
  #   scale_y_continuous(breaks = seq(from = 0.2,to = 1.2, by = 0.2),
  #                      minor_breaks = seq(from = 0.2,to = 1.2, by = 0.02),
  #                      limits = c(0.2,1.2)) +
  #   xlab("Stop-signal delay category") +
  #   ylab("Response time (s)") +
  #   ggtitle("Stop-respond response time as a function of stop-signal delay") +
  #   theme_few() +
  #   theme_irmass +
  #   theme(axis.text.x = element_text(angle = 0),
  #         plot.margin = margin(1,1,1,1),
  #         plot.background = element_rect(colour = "red", size = 1),
  #         panel.background = element_rect(colour = "blue", size = 1),
  #         strip.background = element_rect(colour = "yellow", size = 1),
  #         legend.margin = margin(0,0,0,0),
  #         legend.background = element_rect(colour = "green", size = 1))

}

#' Plot stop-respond RT vs. stop-signal delay for group-level data
#' @export
plot_srrt_vs_ssd_grp <- function(df) {

  idv_alpha = 0.2

  # Main variables (x, y, fill, label) -----------------------------------------
  plt <-
    ggplot2::ggplot(df,
                    ggplot2::aes(x = t_d_alt,
                                 y = mean_RT)
                    )

  # Geoms  ---------------------------------------------------------------------
  plt <-
    plt +
    ggplot2::geom_line(ggplot2::aes(group = subjectIx),
                       alpha = idv_alpha) +

    ggplot2::geom_point(shape = 1,
                        size = 2,
                        alpha = idv_alpha
                        )

  # Summary statistics ---------------------------------------------------------
  plt <-
    plt +
    ggplot2::stat_summary(ggplot2::aes(group = 1),
                          fun.y = mean,
                          geom = "line",
                          size = 2,
                          linetype = 'solid',
                          na.rm = TRUE)

  # Scales  --------------------------------------------------------------------

  plt <-
    plt +

    ggplot2::scale_x_discrete(name = "Delay category") +

    ggplot2::scale_y_continuous(name = "S-R RT (s)",
                                breaks = seq(from = 0.4,to = 0.8, by = 0.2),
                                limits = c(0.4,0.9)
                                )

    # Theme  ---------------------------------------------------------------------

  plt <-
    plt +

    irmass::theme_irmass()

}

#' plot_mcmc_analysis ##########################################################
#' Plots graphical diagnostics of MCMC chains (for models fit with brms)
#'
#' @param mdl, brmsfit object
#' @export
plot_mcmc_analysis <- function(mdl) {
  mdl_long <- ggmcmc::ggs(mdl)

  # Trace plot
  plt_trace <-
    ggmcmc::ggs_traceplot(mdl_long) +
    ggplot2::facet_wrap("Parameter") +
    theme_irmass() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 1))

  # Plot of Rhat
  plt_rhat <-
    ggmcmc::ggs_Rhat(mdl_long) +
    theme_irmass()

  return(list(plt_trace, plt_rhat))
}


#' plot_posterior ##########################################################
#' Plots posterior distribution
#'
#' @param mdl, brmsfit object
#' @export
plot_posterior <- function(mdl) {
  # the ggmcmc::ggs function transforms the BRMS output into a longformat tibble, that we can use to make different types of plot
  mdl_long <-
    ggmcmc::ggs(mdl)

  ggplot2::ggplot(data = mdl_long,
                  mapping = ggplot2::aes(x = value)) +
    ggplot2::facet_wrap("Parameter", scales = "free") +

    ggplot2::geom_density(fill = "yellow", alpha = .5) +
    ggplot2::geom_vline(xintercept = 0, col = "red", size = 1) +
    ggplot2::scale_x_continuous(name = "Value") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::expand_limits(x = 0) +
    # ggplot2::geom_vline(xintercept = summary(mdl)$fixed[stringr::str_remove(par_name, "b_"), 3:4],
    #                     col = "blue",
    #                     linetype = 2) +
    irmass::theme_irmass() +
    ggplot2::labs(title = "Posterior densities")

}

#' Write figure caption to disk
#'
#' @export
write_fig_cap <- function(fig_title, fig_description, fig_dir, notebook_name, stopping_type) {

  tag <-
    switch(stopping_type,
           action_selective = 'action_selective_stopping.txt',
           stimulus_selective = 'stimulus_selective_stopping.txt',
           action_and_stimulus_selective = 'action_and_stimulus_selective_stopping.txt'
           )

  filename_fig_caption <-
    paste('plot_caption', notebook_name, tag, sep = '_')

  path_fig_caption <-
    file.path(fig_dir,
              notebook_name,
              filename_fig_caption)

  # Write figure caption
  readr::write_file(paste(fig_title, fig_description, sep = " "),
                    path = path_fig_caption
                    )

  # Show where file is written
  return(path_fig_caption)
}
