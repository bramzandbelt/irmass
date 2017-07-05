# Cleaning functions



#' Get column types for extracting data from csv files
#'
get_col_types <- function(file_type){

  # blocklog_raw
  # blocklog_preproc
  #
  # triallog_raw
  # triallog_preproc
  # triallog_compliant


  switch(tolower(file_type),
         sess_cols =
           readr::cols_only(subjectIx = col_integer(),
                            sessDate = col_date(format = "%Y-%m-%d"),
                            studyId = col_character(),
                            experimenterId = col_character(),
                            taskVersionId = col_character(),
                            responseDevice = col_character(),
                            blockId = col_character()
                            ),
         block_cols =
           readr::cols_only(subjectIx = col_integer(),
                            blockId = col_character(),
                            blockIx = col_integer(),
                            iterIx = col_double(),
                            s1Acc_00 = col_double(),
                            s1AccCritMet_00 = col_logical(),
                            s1Acc_01 = col_double(),
                            s1AccCritMet_01 = col_logical(),
                            s2Acc_00 = col_double(),
                            s2AccCritMet_00 = col_logical(),
                            s2Acc_01 = col_double(),
                            s2AccCritMet_01 = col_logical(),
                            s2Acc_02 = col_double(),
                            s2AccCritMet_02 = col_logical(),
                            s2Acc_03 = col_double(),
                            s2AccCritMet_03 = col_logical(),
                            s1MeanRt_00 = col_double(),
                            s1MeanRtCritMet_00 = col_logical(),
                            s1MeanRt_01 = col_double(),
                            s1MeanRtCritMet_01 = col_logical(),
                            s1MeanRtDiff_00 = col_double(),
                            s1MeanRtDiffCritMet_00 = col_logical(),
                            s1MeanRtDiff_01 = col_double(),
                            s1MeanRtDiffCritMet_01 = col_logical()
                            ),
         trial_cols =
           readr::cols_only(subjectIx = col_integer(),
                            blockId = col_character(),
                            blockIx = col_integer(),
                            trialIx = col_integer(),
                            fixIx = col_integer(),
                            s1Ix = col_integer(),
                            s2Ix = col_double(),
                            soaIx = col_double(),
                            tSession = col_time(format = "%H:%M:%S"),
                            tBlock = col_time(format = "%H:%M:%OS"),
                            trialOns = col_double(),
                            trialDur = col_double(),
                            fixOns = col_double(),
                            fixOnsDt = col_double(),
                            fixDur = col_double(),
                            fixDurDt = col_double(),
                            s1Ons = col_double(),
                            s1OnsDt = col_double(),
                            s1Dur = col_double(),
                            s1DurDt = col_double(),
                            s2Ons = col_double(),
                            s2OnsDt = col_double(),
                            s2Dur = col_double(),
                            s2DurDt = col_double(),
                            waitedForTrigger = col_double(),
                            keyCount_f = col_integer(),
                            keyCount_b = col_integer(),
                            keyCount_e = col_integer(),
                            keyCount_a = col_integer(),
                            rt1_f = col_double(),
                            rt1_b = col_double(),
                            rt1_e = col_double(),
                            rt1_a = col_double(),
                            rt1_mean = col_double(),
                            rt1_min = col_double(),
                            rt1_max = col_double(),
                            `rtDiff1_f-b` = col_double(),
                            `rtDiff1_e-a` = col_double(),
                            rtDiff1_mean = col_double(),
                            trialCorrect = col_logical(),
                            trialType = col_character(),
                            responseType = col_character(),
                            trialFeedback = col_character()
                            ),
         trial_cols_sub00 =
           readr::cols_only(subjectIx = col_integer(),
                            blockId = col_character(),
                            blockIx = col_integer(),
                            trialIx = col_integer(),
                            fixIx = col_integer(),
                            s1Ix = col_integer(),
                            s2Ix = col_double(),
                            soaIx = col_double(),
                            tSession = col_time(format = "%H:%M:%S"),
                            tBlock = col_time(format = "%H:%M:%OS"),
                            trialOns = col_double(),
                            trialDur = col_double(),
                            fixOns = col_double(),
                            fixOnsDt = col_double(),
                            fixDur = col_double(),
                            fixDurDt = col_double(),
                            s1Ons = col_double(),
                            s1OnsDt = col_double(),
                            s1Dur = col_double(),
                            s1DurDt = col_double(),
                            s2Ons = col_double(),
                            s2OnsDt = col_double(),
                            s2Dur = col_double(),
                            s2DurDt = col_double(),
                            waitedForTrigger = col_logical(),
                            keyCount_f = col_integer(),
                            keyCount_h = col_integer(),
                            keyCount_v = col_integer(),
                            keyCount_b = col_integer(),
                            rt1_f = col_double(),
                            rt1_h = col_double(),
                            rt1_v = col_double(),
                            rt1_b = col_double(),
                            rt1_mean = col_double(),
                            `rtDiff1_f-h` = col_double(),
                            `rtDiff1_v-b` = col_double(),
                            rtDiff1_mean = col_double(),
                            trialCorrect = col_character(),
                            trialType = col_character(),
                            responseType = col_character(),
                            trialFeedback = col_character()
                            )
         )
}


#' Preprocesses raw triallog files from participant 1
#'
#' Participant one had a slightly different key mapping than the other participants, because
preproc_trial_logs_sub01 <- function() {

}

#' Splits log files into a session table and a block/trial table
#'
split_log_in_multi_table <- function(df) {

  session_data <-
    df %>%
    select(studyId:sessTime, -rngSeed) %>%
    distinct()
}



#' Preprocesses the raw triallog files as to format data preregistration
#'
preproc_trial_logs <- function(data) {

  # Manipulated variables (see §2.5.1. of the preregistration) =================

  # s1 - go stimululs
  # trial - trial type
  # t_d - stop-signal delay
  # t_d_alt - alternative stop-signal delay category

  data <-
    mutate(data,
           s1 = ifelse(s1Ix == 0,
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

    rename(data,
           RC_LM = keyCount_f,
           RC_RM = keyCount_b,
           RC_LI = keyCount_e,
           RC_RI = keyCount_a,
           ) %>%
    rename(.,
           RT_LM = rt1_f,
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

    mutate(r = case_when(
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

    mutate(r = ifelse(is.na(r),
                      "NOC",
                      r
                      )
           ) %>%

    # Bimanual response recorded (r_{bi})
    mutate(r_bi = ifelse(r %in% c("RB","RBO"),
                    TRUE,
                    FALSE
                    )
           ) %>%

    # Trial-level response time (RT_{trial})
    mutate(RT_trial = rt1_mean) %>%

    # Trial label - performance ($trial_{performance}$) ------------------------
    mutate(trial_performance = case_when(
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

}

# ==============================================================================

#' Verifies index columns
#'
#' Identifies index columns, replaces NaNs with NAs, and converts objects to integer
verify_ix_cols <- function(df) {
  col_vector <- stringr::str_subset(colnames(df),".*Ix$")
  df[col_vector] <- df[col_vector] %>%
    purrr::map(~ stringr::str_replace(.x, "nan", "NA")) %>%
    purrr::map(~ as.integer(.x))
  df
}
