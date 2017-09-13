#' Get default ggplot theme settings to be used in this project
#' @export
theme_irmass <- function() {

  title_font_size <- 10
  subtitle_font_size <- 8
  label_font_size <- 5

  ggthemes::theme_few() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold",
                                         size = title_font_size,
                                         hjust = 0.5),
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = label_font_size),
      axis.text.x = ggplot2::element_text(angle = 45,
                                     hjust = 0.5),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = subtitle_font_size),

      legend.background = ggplot2::element_rect(),
      legend.position = "none",
      legend.text = ggplot2::element_text(size = label_font_size),
      legend.title = ggplot2::element_text(size = subtitle_font_size),

      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.01,"in"),

      # strip.background = ggplot2::element_rect(),
      # strip.text = ggplot2::element_text(size = subtitle_font_size),
      # strip.background = ggplot2::element_blank(),
      # strip.text = ggplot2::element_blank()

    )
}

#' Plots task performance overview, for assessment of performance criteria
#'
#' Plotting can be done for practice and experimental stage.
#'
#' @param data tibble containing data
#' @param stage experiment stage, can be 'prac' or 'expt'
#' @param simple logical, whether to plot details (performance measures, such as RT) or not (showing number of failures/attempts only)
#' @param block_type char, which block to plot data of (only applies to stage = 'prac' and simple = TRUE)
#' @export
plot_overview <- function(data, stage = 'expt', simple = FALSE, block_type = NULL) {

  if (stage == 'prac') {
    if (simple == TRUE) {
      data <-
        dplyr::filter_(data, paste('block_type == "', block_type,'"',sep = "")) %>%
        droplevels()
    }
  }

  # Main variables (x, y, fill, label) -----------------------------------------

  # x, y, and fill vary between `stage`
  if (stage == 'prac') {
    plt <- ggplot2::ggplot(data,
                           ggplot2::aes(x = attempt,
                                        y = criterion,
                                        fill = failed
                           )
    )
  } else if (stage == 'expt') {
    plt <- ggplot2::ggplot(data,
                           ggplot2::aes(x = subjectIx,
                                        y = criterion,
                                        fill = failed
                           )
    )
  }

  # Label varies between `stage` and `simple``
  if (stage == 'prac') {
    plt <- plt + ggplot2::aes(label = sprintf("%0.0f", performance))
    } else if (stage == 'expt') {
      if (simple) {
        plt <- plt + ggplot2::aes(label = n_consec_failures)
      } else {
        plt <- plt + ggplot2::aes(label = sprintf("%0.0f", performance))
      }
    }

  # Facets ---------------------------------------------------------------------

  if (stage == 'prac') {
    if (simple) {
      plt <- plt + ggplot2::facet_wrap("subjectIx", ncol = 9)
    } else {
      plt <- plt + ggplot2::facet_grid(block_type ~ subjectIx)
    }
  }

  # Geoms  ---------------------------------------------------------------------

  if (stage == 'prac') {
    if (simple) {
      plt <- plt + ggplot2::geom_tile(na.rm = TRUE, size = 2) +
        ggplot2::geom_text(size = 2, color = "white")

    } else {
      plt <- plt + ggplot2::geom_tile(na.rm = TRUE)
    }

  } else if (stage == 'expt') {

    plt <- plt + ggplot2::geom_tile() +
      ggplot2::geom_text(size = 2, color = "white")

  }

  # Scales  --------------------------------------------------------------------

  # fill
  if (stage == 'prac') {
    fill_name <- "failure to meet block-level task performance criteria"
  } else if (stage == 'expt') {
    if (simple) {
      fill_name <- "failure to meet overall task performance criteria"
    } else {
      fill_name <- "failure to meet performance criteria based on consecutive blocks"
    }
  }

  plt <- plt + ggplot2::scale_fill_manual(values = c("#008000","#FF0000"),
                                          guide = ggplot2::guide_legend(
                                            title.position = "top",
                                            title.hjust = 0.5),
                                          name = fill_name
                                          )


  # coord_equal
  plt <- plt + switch(stage,
                      expt = ggplot2::coord_equal())


  # x and y
  if (stage == 'prac') {
    if (!simple) {
      plt <- plt + ggplot2::scale_x_discrete(breaks = 1:5)
    }

  } else if (stage == 'expt') {
    plt <- plt + ggplot2::scale_x_discrete(breaks = 0:35, position = "top") +
      ggplot2::scale_y_discrete(limits = rev(c("NS_accuracy", "SL_accuracy", "SR_accuracy",
                                               "SB_accuracy", "IG_accuracy", "NS_mean_RT")),
                                labels = c("NS_accuracy" = "no-signal accuracy",
                                           "SL_accuracy" = "stop-left accuracy",
                                           "SR_accuracy" = "stop-right accuracy",
                                           "SB_accuracy" = "stop-both accuracy",
                                           "IG_accuracy" = "ignore accuracy",
                                           "NS_mean_RT" = "no-signal mean RT (ms)")
      )

  }

  # Plot title  ----------------------------------------------------------------
  if (stage == 'prac') {
    if (simple) {
      plt <- plt + ggplot2::ggtitle(sprintf("Assessment of task performance exclusion criteria for %s block of practice session",block_type))
    } else {
      plt <- plt + ggplot2::ggtitle("Assessment of task performance exclusion criteria of practice session")
    }
  } else if (stage == 'expt') {
    if (simple) {
      plt <- plt + ggplot2::ggtitle("Exclusion based on number of consecutive blocks in which performance failed to meet criteria")
    } else {
      plt <- plt + ggplot2::ggtitle("Exclusion based on overall task performance")
    }
  }

  # Axes labels  ---------------------------------------------------------------
  if (stage == 'prac') {
    plt <-
      plt +
      ggplot2::xlab("Attempt") +
      ggplot2::ylab("Criterion measure")
  } else if (stage == 'expt') {
    plt <-
      plt +
      ggplot2::xlab("Subject ID") +
      ggplot2::ylab("Criterion measure")
  }

  # Theme  ---------------------------------------------------------------------

  plt <-
    plt +
    irmass::theme_irmass()

  if (stage == 'prac') {
    if (!simple) {
      plt <-
        plt +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0.5, size = 3))
    }
  }

  plt
}

#' Plot inhibition function for individual-level data
#'
#' Plots the probability of responding given a stop-signal against stop-signal delay, a histogram of the number stop-inhibit and stop-respond trials for each stop-signal delay, and a logistic regression fit to the data.
plot_if_idv <- function() {

}

#' Plot inhibition function for group-level data
#'
plot_if_grp <- function() {

}

#' Plot stop-respond RT vs. no-signal RT for individual-level data
#'
plot_srrt_vs_nsrt_idv <- function() {

}

#' Plot stop-respond RT vs. no-signal RT for group-level data
#'
plot_srrt_vs_nsrt_grp <- function(data, ns, sr) {
  # ggplot2::ggplot(data = data,
  #                 ggplot2::aes(x = data[[ns]],
  #                              y = data[[sr]],
  #                              color = is_outlier
  #                              )
  #                 ) +
  #
  #   ggplot2::geom_point(shape = 21) +
  #   ggplot2::geom_abline(intercept = 0,
  #                        slope = 1,
  #                        linetype = "dashed"
  #                        ) +
  #   ggplot2::geom_vline(xintercept = 0.65,
  #                       linetype = "dotted"
  #   ) +
  #   ggplot2::scale_x_continuous(name = "Mean no-signal RT (s)",
  #                               limits = c(0.45, 0.75)
  #   ) +
  #   ggplot2::scale_y_continuous(name = "Mean stop-respond RT (s)",
  #                               limits = c(0.45, 0.75)
  #   ) +
  #   ggthemes::theme_few() +
  #   irmass::theme_irmass() +
  #   ggplot2::theme(aspect.ratio = 1)
}

#' Plot stop-respond RT vs. stop-signal delay for individual-level data
#'
plot_srrt_vs_ssd_idv <- function() {
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
#'
plot_srrt_vs_ssd_grp <- function() {



}
