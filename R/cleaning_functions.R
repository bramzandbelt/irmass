# Cleaning functions

# ==============================================================================

#' Tidy block data
#'
#' Tidying involves multiple steps, including aggregating no-signal performance trial performance across go-signal levels, renaming variables, and assessing performance criteria on the basis of aggregate measures.
#' @export
tidy_block_data <- function(df, stage = 'expt') {

    if (stage == 'prac') {
      clean_df <-
        df %>%
        dplyr::mutate(block_type = ifelse(stringr::str_detect(blockId, '^e.*'),
                                          "mixed",
                                          ifelse(.$blockIx == 0,
                                                 "NS",
                                                 ifelse(((.$blockIx == 1) & (.$taskVersionId == "A")) |
                                                          ((.$blockIx == 2) & (.$taskVersionId == "B")),
                                                        "AS",
                                                        ifelse(((.$blockIx == 2) & (.$taskVersionId == "A")) |
                                                                 ((.$blockIx == 1) & (.$taskVersionId == "B")),
                                                               "SS",
                                                               ifelse(.$blockIx == 3,
                                                                      "mixed",
                                                                      NA_character_
                                                                      )
                                                               )
                                                        )
                                                 )
                                          )
                      ) %>%
        dplyr::mutate(NS_accuracy = pmin(s1Acc_00,s1Acc_01),
                      NS_mean_RT = pmax(s1MeanRt_00,s1MeanRt_01),
                      NS_mean_RTdiff = pmax(s1MeanRtDiff_00,s1MeanRtDiff_01)
                      ) %>%
        dplyr::rename(SL_accuracy = s2Acc_00,
                      SR_accuracy = s2Acc_01,
                      SB_accuracy = s2Acc_02,
                      IG_accuracy = s2Acc_03
                      ) %>%
        dplyr::mutate(NS_accuracy_failed = NS_accuracy < 85,
                      NS_mean_RT_failed = NS_mean_RT > 650,
                      NS_mean_RTdiff_failed = NS_mean_RTdiff > 50,
                      SL_accuracy_failed = SL_accuracy < 20,
                      SR_accuracy_failed = SR_accuracy < 20,
                      SB_accuracy_failed = SB_accuracy < 20,
                      IG_accuracy_failed = IG_accuracy < 80
                      ) %>%
        dplyr::select(subjectIx, blockId, block_type,
                      NS_accuracy, NS_mean_RT, NS_mean_RTdiff,
                      SL_accuracy, SR_accuracy, SB_accuracy, IG_accuracy,
                      NS_accuracy_failed, NS_mean_RT_failed, NS_mean_RTdiff_failed,
                      SL_accuracy_failed, SR_accuracy_failed, SB_accuracy_failed, IG_accuracy_failed
                      ) %>%
        dplyr::group_by(subjectIx,blockId) %>%
        dplyr::mutate(attempt = 1:n()) %>%
        dplyr::ungroup()

    } else if (stage == 'expt') {

      clean_df <-
        df %>%
        dplyr::group_by(subjectIx, blockId) %>%
        dplyr::summarize(NS_accuracy = mean(c(s1Acc_00,s1Acc_01)),
                         NS_accuracy_failed = NS_accuracy < 85,
                         NS_mean_RT = mean(c(s1MeanRt_00,s1MeanRt_01)),
                         NS_mean_RT_failed = NS_mean_RT > 650,
                         NS_mean_RTdiff = mean(c(s1MeanRtDiff_00,s1MeanRtDiff_01)),
                         NS_mean_RTdiff_failed = NS_mean_RTdiff > 50,
                         SL_accuracy = mean(s2Acc_00),
                         SL_accuracy_failed = SL_accuracy < 20,
                         SR_accuracy = mean(s2Acc_01),
                         SR_accuracy_failed = SR_accuracy < 20,
                         SB_accuracy = mean(s2Acc_02),
                         SB_accuracy_failed = SB_accuracy < 20,
                         IG_accuracy = mean(s2Acc_03),
                         IG_accuracy_failed = IG_accuracy < 80)

    }

  # Make tidy ------------------------------------------------------------------

  if (stage == 'prac') {

    general_cols <-  c("subjectIx","blockId","block_type","attempt")
    performance_cols <- c("NS_accuracy","NS_mean_RT","NS_mean_RTdiff","SL_accuracy","SR_accuracy","SB_accuracy","IG_accuracy")
    failure_cols <- colnames(clean_df) %>% .[stringr::str_detect(., ".*failed$")]

    # Performance stats

    tidy_performance_stats <-
      clean_df %>%
      tidyr::gather_(gather_cols = performance_cols,
                     key_col = "criterion",
                     value_col = "performance") %>%
      dplyr::select_(.dots = c(general_cols,"criterion","performance"))

    # Failure stats
    tidy_failure_stats <-
      clean_df %>%
      dplyr::select_(.dots = c(general_cols,failure_cols)) %>%
      setNames(gsub("_failed","",names(.))) %>%
      tidyr::gather_(gather_cols = performance_cols,
                     key_col = "criterion",
                     value_col = "failed") %>%
      dplyr::select_(.dots = c(general_cols,"criterion","failed"))

    # Combine performance and failure stats in one data frame
    tidy_data <-
      dplyr::left_join(tidy_performance_stats,tidy_failure_stats,
                       by = c(general_cols,"criterion")) %>%
      dplyr::arrange(subjectIx, blockId, attempt, criterion)

  } else if (stage == 'expt') {

    general_cols <-  c("subjectIx","blockId")
    performance_cols <- c("NS_accuracy","SL_accuracy","SR_accuracy","SB_accuracy","IG_accuracy","NS_mean_RT", "NS_mean_RTdiff")
    failure_cols <- colnames(clean_df) %>% .[stringr::str_detect(., ".*failed$")]

    # Performance stats
    tidy_performance_stats <-
      clean_df %>%
      tidyr::gather_(gather_cols = performance_cols,
                     key_col = "criterion",
                     value_col = "performance") %>%
      dplyr::select_(.dots = c(general_cols,"criterion","performance"))

    # Failure stats
    tidy_failure_stats <-
      clean_df %>%
      dplyr::select_(.dots = c(general_cols,failure_cols)) %>%
      setNames(gsub("_failed","",names(.))) %>%
      tidyr::gather_(gather_cols = performance_cols,
                     key_col = "criterion",
                     value_col = "failed") %>%
      dplyr::select_(.dots = c(general_cols,"criterion","failed"))

    # Combine performance and failure stats in one data frame
    tidy_data <-
      dplyr::left_join(tidy_performance_stats,tidy_failure_stats,
                       by = c(general_cols,"criterion")) %>%
      dplyr::arrange(subjectIx, blockId, criterion)
  }
}

# ==============================================================================

#' Tidy trial data
#'
#' Tidying involves the following steps
#' - renaming and computing variables in line with preregistration
#' @export
tidy_trial_data <- function(df) {

  # Manipulated variables (see §2.5.1. of the preregistration) =================

  # s1 - go stimululs
  # trial - trial type
  # t_d - stop-signal delay
  # t_d_alt - alternative stop-signal delay category

  clean_df <-
    df %>%
    dplyr::mutate(s1 = ifelse(s1Ix == 0,
                              "M",
                              ifelse(s1Ix == 1,
                                     "I",
                                     NA_integer_)
                              ),
           trial = ifelse(is.na(s2Ix),
                          "NS",
                          ifelse(s2Ix == 0,
                                 "SL",
                                 ifelse(s2Ix == 1,
                                        "SR",
                                        ifelse(s2Ix == 2,
                                               "SB",
                                               ifelse(s2Ix == 3,
                                                      "IG",
                                                      NA_integer_)
                                        )
                                 )
                          )
           ),
           t_d = round((s2Ons - s2OnsDt) - (s1Ons - s1OnsDt), digits = 3),
           t_d_alt = ifelse(soaIx %in% 0:2,
                            "short",
                            ifelse(soaIx == 3,
                                   "intermediate",
                                   ifelse(soaIx == 4,
                                          "long",
                                          NA_character_
                                   )
                            )
           )
    ) %>%

    # Measured variables (see § 2.5.2. of the pre-registration) ==================

  # Finger-level response count (RC_{finger})
  # Finger-level response time (RT_{finger})

  dplyr::rename(RC_LM = keyCount_f,
                RC_RM = keyCount_b,
                RC_LI = keyCount_e,
                RC_RI = keyCount_a,
  ) %>%
    dplyr::rename(RT_LM = rt1_f,
                  RT_RM = rt1_b,
                  RT_LI = rt1_e,
                  RT_RI = rt1_a
    ) %>%

    # Indices (see § 2.5.3. of the pre-registration) ===========================

  # Response category ----------------------------------------------------------
  # - RB:   Bimanual response, fingers compatible with the go-stimulus
  # - RL:   Left-hand response, finger compatible with the go-stimulus
  # - RR:   Right-hand response, finger compatible with the go-stimulus
  # - RBO:  Bimanual response, fingers compatible with the other go-stimulus
  # - RLO:  Left-hand response, finger compatible with the other go-stimulus
  # - RRO:  Right-hand response, finger compatible with the other go-stimulus
  # - NR:   No response
  # - NOC:  Not otherwise classified

  dplyr::mutate(r = dplyr::case_when(
    .$s1 == "M" & .$RC_LM == 1 & .$RC_RM == 1 & .$RC_LI == 0 & .$RC_RI == 0 ~ "RB",
    .$s1 == "M" & .$RC_LM == 1 & .$RC_RM == 0 & .$RC_LI == 0 & .$RC_RI == 0 ~ "RL",
    .$s1 == "M" & .$RC_LM == 0 & .$RC_RM == 1 & .$RC_LI == 0 & .$RC_RI == 0 ~ "RR",
    .$s1 == "M" & .$RC_LM == 0 & .$RC_RM == 0 & .$RC_LI == 1 & .$RC_RI == 1 ~ "RBO",
    .$s1 == "M" & .$RC_LM == 0 & .$RC_RM == 0 & .$RC_LI == 1 & .$RC_RI == 0 ~ "RLO",
    .$s1 == "M" & .$RC_LM == 0 & .$RC_RM == 0 & .$RC_LI == 0 & .$RC_RI == 1 ~ "RRO",
    .$s1 == "M" & .$RC_LM == 0 & .$RC_RM == 0 & .$RC_LI == 0 & .$RC_RI == 0 ~ "NR",
    .$s1 == "I" & .$RC_LM == 0 & .$RC_RM == 0 & .$RC_LI == 1 & .$RC_RI == 1 ~ "RB",
    .$s1 == "I" & .$RC_LM == 0 & .$RC_RM == 0 & .$RC_LI == 1 & .$RC_RI == 0 ~ "RL",
    .$s1 == "I" & .$RC_LM == 0 & .$RC_RM == 0 & .$RC_LI == 0 & .$RC_RI == 1 ~ "RR",
    .$s1 == "I" & .$RC_LM == 1 & .$RC_RM == 1 & .$RC_LI == 0 & .$RC_RI == 0 ~ "RBO",
    .$s1 == "I" & .$RC_LM == 1 & .$RC_RM == 0 & .$RC_LI == 0 & .$RC_RI == 0 ~ "RLO",
    .$s1 == "I" & .$RC_LM == 0 & .$RC_RM == 1 & .$RC_LI == 0 & .$RC_RI == 0 ~ "RRO",
    .$s1 == "I" & .$RC_LM == 0 & .$RC_RM == 0 & .$RC_LI == 0 & .$RC_RI == 0 ~ "NR"
  )
  ) %>%

    dplyr::mutate(r = ifelse(is.na(r),
                             "NOC",
                             r
                             )
                  ) %>%

    # Bimanual response recorded (r_{bi})
    dplyr::mutate(r_bi = ifelse(r %in% c("RB","RBO"),
                                TRUE,
                                FALSE
                                )
                  ) %>%

    # Trial-level response time (RT_{trial})
    dplyr::mutate(RT_trial = rt1_mean) %>%

    # Trial label - performance ($trial_{performance}$)
    dplyr::mutate(trial_performance = dplyr::case_when(
      .$trial == "NS" & .$r == "RB" ~ "NS_correct",
      .$trial == "NS" & .$r == "RBO" ~ "NS_commission-error-bi",
      .$trial == "NS" & .$r %in% c("RL","RR","RLO","RRO") ~ "NS_commission-error-uni",
      .$trial == "NS" & .$r == "NOC" ~ "NS_commission-error-other",
      .$trial == "NS" & .$r == "NR" ~ "NS_omission-error",
      .$trial == "SL" & .$r %in% c("RB","RBO") ~ "SL_stop-respond-bi",
      .$trial == "SL" & .$r %in% c("RL","RLO","RRO") ~ "SL_stop-respond-uni",
      .$trial == "SL" & .$r %in% c("NR","NOC") ~ "SL_stop-respond-other",
      .$trial == "SL" & .$r == "RR" ~ "SL_stop-inhibit",
      .$trial == "SR" & .$r %in% c("RB","RBO") ~ "SR_stop-respond-bi",
      .$trial == "SR" & .$r %in% c("RR","RLO","RRO") ~ "SR_stop-respond-uni",
      .$trial == "SR" & .$r %in% c("NR","NOC") ~ "SR_stop-respond-other",
      .$trial == "SR" & .$r == "RL" ~ "SR_stop-inhibit",
      .$trial == "SB" & .$r %in% c("RB","RBO") ~ "SB_stop-respond-bi",
      .$trial == "SB" & .$r %in% c("RL","RR","RLO","RRO") ~ "SB_stop-respond-uni",
      .$trial == "SB" & .$r == "NOC" ~ "SB_stop-respond-other",
      .$trial == "SB" & .$r == "NR" ~ "SB_stop-inhibit",
      .$trial == "IG" & .$r == "RB" ~ "IG_correct",
      .$trial == "IG" & .$r == "RBO" ~ "IG_commission-error-bi",
      .$trial == "IG" & .$r %in% c("RL","RR","RLO","RRO") ~ "IG_commission-error-uni",
      .$trial == "IG" & .$r == "NOC" ~ "IG_commission-error-other",
      .$trial == "IG" & .$r == "NR" ~ "IG_omission-error"
      )
      )

  RT_df <-
    clean_df %>%
    dplyr::select(subjectIx, blockIx, trialIx, trial, r, t_d, t_d_alt, RT_trial)

  resp_df <-
    clean_df %>%
    dplyr::select(subjectIx, blockIx, trialIx, trial, r, t_d, t_d_alt, r_bi, trialCorrect)

  list(RT_df = RT_df, resp_df = resp_df)
}

# ==============================================================================

#' Get column types for extracting data from csv files
#'
# '@export
get_col_types <- function(file_type){

  # blocklog_raw
  # blocklog_preproc
  #
  # triallog_raw
  # triallog_preproc
  # triallog_compliant


  switch(tolower(file_type),
         sess_cols =
           readr::cols_only(subjectIx = readr::col_integer(),
                            sessDate = readr::col_date(format = "%Y-%m-%d"),
                            studyId = readr::col_character(),
                            experimenterId = readr::col_character(),
                            taskVersionId = readr::col_character(),
                            responseDevice = readr::col_character(),
                            blockId = readr::col_character()
                            ),
         block_cols =
           readr::cols_only(subjectIx = readr::col_integer(),
                            taskVersionId = readr::col_character(),
                            blockId = readr::col_character(),
                            blockIx = readr::col_integer(),
                            s1Acc_00 = readr::col_double(),
                            s1AccCritMet_00 = readr::col_logical(),
                            s1Acc_01 = readr::col_double(),
                            s1AccCritMet_01 = readr::col_logical(),
                            s2Acc_00 = readr::col_double(),
                            s2AccCritMet_00 = readr::col_logical(),
                            s2Acc_01 = readr::col_double(),
                            s2AccCritMet_01 = readr::col_logical(),
                            s2Acc_02 = readr::col_double(),
                            s2AccCritMet_02 = readr::col_logical(),
                            s2Acc_03 = readr::col_double(),
                            s2AccCritMet_03 = readr::col_logical(),
                            s1MeanRt_00 = readr::col_double(),
                            s1MeanRtCritMet_00 = readr::col_logical(),
                            s1MeanRt_01 = readr::col_double(),
                            s1MeanRtCritMet_01 = readr::col_logical(),
                            s1MeanRtDiff_00 = readr::col_double(),
                            s1MeanRtDiffCritMet_00 = readr::col_logical(),
                            s1MeanRtDiff_01 = readr::col_double(),
                            s1MeanRtDiffCritMet_01 = readr::col_logical()
                            ),
         trial_cols =
           readr::cols_only(subjectIx = readr::col_integer(),
                            blockId = readr::col_character(),
                            blockIx = readr::col_integer(),
                            trialIx = readr::col_integer(),
                            fixIx = readr::col_integer(),
                            s1Ix = readr::col_integer(),
                            s2Ix = readr::col_double(),
                            soaIx = readr::col_double(),
                            tSession = readr::col_time(format = "%H:%M:%S"),
                            tBlock = readr::col_time(format = "%H:%M:%OS"),
                            trialOns = readr::col_double(),
                            trialDur = readr::col_double(),
                            fixOns = readr::col_double(),
                            fixOnsDt = readr::col_double(),
                            fixDur = readr::col_double(),
                            fixDurDt = readr::col_double(),
                            s1Ons = readr::col_double(),
                            s1OnsDt = readr::col_double(),
                            s1Dur = readr::col_double(),
                            s1DurDt = readr::col_double(),
                            s2Ons = readr::col_double(),
                            s2OnsDt = readr::col_double(),
                            s2Dur = readr::col_double(),
                            s2DurDt = readr::col_double(),
                            keyCount_f = readr::col_integer(),
                            keyCount_b = readr::col_integer(),
                            keyCount_e = readr::col_integer(),
                            keyCount_a = readr::col_integer(),
                            rt1_f = readr::col_double(),
                            rt1_b = readr::col_double(),
                            rt1_e = readr::col_double(),
                            rt1_a = readr::col_double(),
                            rt1_mean = readr::col_double(),
                            `rtDiff1_f-b` = readr::col_double(),
                            `rtDiff1_e-a` = readr::col_double(),
                            rtDiff1_mean = readr::col_double(),
                            trialCorrect = readr::col_logical(),
                            trialType = readr::col_character(),
                            responseType = readr::col_character(),
                            trialFeedback = readr::col_character()
                            ),
         trial_cols_sub00 =
           readr::cols_only(subjectIx = readr::col_integer(),
                            blockId = readr::col_character(),
                            blockIx = readr::col_integer(),
                            trialIx = readr::col_integer(),
                            fixIx = readr::col_integer(),
                            s1Ix = readr::col_integer(),
                            s2Ix = readr::col_double(),
                            soaIx = readr::col_double(),
                            tSession = readr::col_time(format = "%H:%M:%S"),
                            tBlock = readr::col_time(format = "%H:%M:%OS"),
                            trialOns = readr::col_double(),
                            trialDur = readr::col_double(),
                            fixOns = readr::col_double(),
                            fixOnsDt = readr::col_double(),
                            fixDur = readr::col_double(),
                            fixDurDt = readr::col_double(),
                            s1Ons = readr::col_double(),
                            s1OnsDt = readr::col_double(),
                            s1Dur = readr::col_double(),
                            s1DurDt = readr::col_double(),
                            s2Ons = readr::col_double(),
                            s2OnsDt = readr::col_double(),
                            s2Dur = readr::col_double(),
                            s2DurDt = readr::col_double(),
                            keyCount_f = readr::col_integer(),
                            keyCount_h = readr::col_integer(),
                            keyCount_v = readr::col_integer(),
                            keyCount_b = readr::col_integer(),
                            rt1_f = readr::col_double(),
                            rt1_h = readr::col_double(),
                            rt1_v = readr::col_double(),
                            rt1_b = readr::col_double(),
                            rt1_mean = readr::col_double(),
                            `rtDiff1_f-h` = readr::col_double(),
                            `rtDiff1_v-b` = readr::col_double(),
                            rtDiff1_mean = readr::col_double(),
                            trialCorrect = readr::col_logical(),
                            trialType = readr::col_character(),
                            responseType = readr::col_character(),
                            trialFeedback = readr::col_character()
                            )
         )

}

#' Splits log files into a session table and a block/trial table
#'
split_log_in_multi_table <- function(df) {

  session_data <-
    df %>%
    dplyr::select(studyId:sessTime, -rngSeed) %>%
    dplyr::distinct()
}

# ==============================================================================

#' Verifies index columns
#'
#' Identifies index columns, replaces NaNs with NAs, and converts objects to integer
#' @export
verify_ix_cols <- function(df) {
  col_vector <- stringr::str_subset(colnames(df),".*Ix$")
  df[col_vector] <- df[col_vector] %>%
    purrr::map(~ stringr::str_replace(.x, "nan", "NA")) %>%
    purrr::map(~ as.integer(.x))
  df
}
