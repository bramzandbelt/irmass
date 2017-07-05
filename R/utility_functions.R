#' Verify critirion met columns
#'
#' Identifies CritMet columns, converts values to logical
verify_crit_met_cols <- function(df) {
  col_vector <- stringr::str_subset(colnames(prac_block_data),".*CritMet.*")
  df[col_vector] <- df[col_vector] %>%
    purrr::map(~ stringr::str_replace(.x, "nan", "NA")) %>%
    purrr::map(~ as.logical(.x))
  df

}

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

#' WHAT IT DOES
#'
# create_dirs_for_notebook_output(base_dirs = list(derivatives_dir, figures_dir),
#                                 notebook_name = notebook_name
# )

# ==============================================================================

#' Selects performance log files from sourcedata directory
#'
#' Selection of performance log files based on stage ("practice", "experiment") and file type ("trial", "block").
#'
#' @param stage
#' @param filetype
select_log_files <- function(stage = "experiment",filetype = "trial") {

  # We to need process experiment triallog files from subj00 differently than the others, because of different key namings (this is not the case for triallog from practice session and the blocklogs)

  if (stage == "experiment" & filetype == "triallog") {

    files_subj00 <-
      Sys.glob(file.path(project_dir,
                         "data",
                         "sourcedata",
                         "*",
                         "performance",
                         stage,
                         paste(filetype,"*.csv", sep = "")
                         )
               ) %>%
      subset(x, subset = grepl(".*subj(00+).*",.)
             )

    files_others <-
      Sys.glob(file.path(project_dir,
                         "data",
                         "sourcedata",
                         "*",
                         "performance",
                         stage,
                         paste(filetype,"*.csv", sep = "")
                         )
               ) %>%
      subset(x, subset = !grepl(".*subj(00+).*",.))

    list("subj00" = files_subj00[!is.na(files_subj00)], "others" = files_others[!is.na(files_others)])
  } else {
    Sys.glob(file.path(project_dir,
                       "data",
                       "sourcedata",
                       "*",
                       "performance",
                       stage,
                       paste(filetype,"*.csv", sep = "")
                       )
             )
  }
}


read_log_files <- function(files, data_type) {

  switch(tolower(data_type),
         sess_data =
           files %>%
           map_df(~read_csv(.x,
                            col_types = get_col_types("sess_cols")
                            )
                  ) %>%
           distinct(subjectIx, .keep_all = TRUE),

         block_data =
           files %>%
           map_df(~read_csv(.x,
                            col_types = get_col_types("block_cols")
                            )
                  ),

         trial_data =
           files %>%
           map_df(~read_csv(.x,
                            col_types = get_col_types("trial_cols")
                            )
                  ),

         trial_data_sub00 =
           files %>%
           map_df(~read_csv(.x,
                            col_types = get_col_types("trial_cols_sub00")
                            )
           )
  )





}
