#' Whether or not to upload data to figShare
#'
#' When this function returns TRUE, then data and figures are uploaded to figShare, DOIs are created, data is made public.
#' @export
upload_data_to_figshare <- function(){
  TRUE
}

#' Return project-specific variable values for uploading to figShare
#'
#' @param spec variable
#' @param article_id figShare ID number of the article (optional)
#' @export
get_figshare_specs <- function(spec, article_id) {

  # If only a spec argument is provided (e.g. article_id not yet created)
  if (missing(article_id)) {

    switch(spec,
           authors = list("0000-0002-6491-1247"),
           categories = c(7,   # Behavioral Neuroscience
                          143, # Neuroscience and Physiological Psychology
                          283, # Motor control
                          368, # Biological Psychology
                          377, # Psychological Methodology, Design and Analysis
                          379, # Sensory Processes, Perception and Performance
                          382, # Psychology not elsewhere classified
                          388, # Cognitive Science not elsewhere classified
                          389  # Psychology and Cognitive Sciences not elsewhere classified
           ),
           do_upload = FALSE, # Whether or not to upload data to figshare
           links = list('https://osf.io/mq64z/' # Pre-registration document
                        ),
           tags = c('executive function',
                    'cognitive contol',
                    'motor control',
                    'response inhibition',
                    'inhibition',
                    'stopping',
                    'selective control',
                    'selective stopping',
                    'action-selective stopping',
                    'stimulus-selective stopping',
                    'stop-signal task',
                    'stop task',
                    'response time',
                    'response latency'
                    ),
           visibility = 'draft'
    )
    } else {
      switch(spec,
             doi = paste0('https://doi.org/10.6084/m9.figshare.',as.character(article_id))
      )
    }
}

# ==============================================================================

#' Check whether directories exists, and create recursively if they don't
#'
#' @param all_dirs Character vector of directory paths to check
#' @export
check_dir <- function(all_dirs){

  for (this_dir in all_dirs) {
    if (!dir.exists(this_dir)) {
      dir.create(this_dir, recursive = TRUE)
    }
  }

}

#' Checks if directories for notebook output exists, and if not creates them
#'
#' @param base_dirs A list of directories in which subdirectories should be created, if they don't exist
#' @param notebook_name Name of subdir
#' @return created_any Whether or not any output dir was created
#' @export
verify_output_dirs <- function(base_dirs, notebook_name) {

  for (base_dir in base_dirs) {
    if (!dir.exists(file.path(base_dir,notebook_name))) {
      dir.create(file.path(base_dir,notebook_name), recursive = TRUE)
    }
  }
}


#' WHAT IT DOES
#'
# create_dirs_for_notebook_output(base_dirs = list(derivatives_dir, figures_dir),
#                                 notebook_name = notebook_name
# )

# ==============================================================================

#' Selects performance log files from raw data directory
#'
#' Selection of performance log files based on stage ("practice", "experiment") and file type ("trial", "block").
#'
#' @param stage Study stage: practice or experiment
#' @param filetype File type: triallog or blocklog
#' @import magrittr
#' @export
select_log_files <- function(stage = "experiment", filetype = "triallog") {

  # We to need process experiment triallog files from subj00 differently than the others, because of different key namings (this is not the case for triallog from practice session and the blocklogs)

  if (stage == "experiment" & filetype == "triallog") {

    files_subj00 <-
      Sys.glob(file.path(project_dir,
                         "data",
                         "raw",
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
                         "raw",
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
                       "raw",
                       "*",
                       "performance",
                       stage,
                       paste(filetype,"*.csv", sep = "")
                       )
             )
  }
}

#' Reads logs files
#'
#' @param files files: list of files
#' @param data_type Data type (char): trial_data, block_data, sess_data
#' @export
read_log_files <- function(files, data_type) {

  switch(tolower(data_type),
         sess_data =
           files %>%
           purrr::map_df(~readr::read_csv(.x,
                                          col_types = get_col_types("log_sess_cols")
                                          )
                         ) %>%
           dplyr::distinct(subjectIx, .keep_all = TRUE),

         block_data =
           files %>%
           purrr::map_df(~readr::read_csv(.x,
                                          col_types = get_col_types("log_block_cols")
                                          )
                         ),

         trial_data =
           files %>%
           purrr::map_df(~readr::read_csv(.x,
                                          col_types = get_col_types("log_trial_cols")
                                          )
                         ),

         trial_data_sub00 =
           files %>%
           purrr::map_df(~readr::read_csv(.x,
                                          col_types = get_col_types("log_trial_cols_sub00")
                                          )
                         )
         )
}
