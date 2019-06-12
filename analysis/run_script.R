require(tidyverse)

# Provides access to a copy of the command line arguments, which specifies what to do
args <- commandArgs(TRUE)

# Identify project dir
project_dir <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))

# Make reportsdir, if it does not exist
irmass::verify_output_dirs(project_dir, "reports")

# Get index of notebook to render to identify notebook filename
notebook_ix <- as.integer(args[1])

notebook_file <-
  switch(notebook_ix,
         "1" = "01_preprocess_log_files.Rmd",
         "2" = "02_assess_task_performance_criteria.Rmd",
         "3" = "03_individual_analysis_effect_ssd_on_prob_responding_given_stopsignal.Rmd",
         "4" = "04_individual_analysis_rt_difference_nosignal_stoprespond.Rmd",
         "5" = "05_individual_analysis_effect_ssd_on_stoprespond_rt.Rmd",
         "6" = "06_group_analysis_effect_ssd_on_prob_responding_given_stopsignal.Rmd",
         "7" = "07_group_analysis_rt_difference_nosignal_stoprespond.Rmd",
         "8" = "08_group_analysis_effect_ssd_on_stoprespond_rt.Rmd"
         )

# Define input dir (notebook_templates_dir) and output dir (reports_dir)
notebook_dir <- "analysis"
reports_dir <- "reports"

# Reports are not parameterized ----
params_tibble <- NULL

# Render notebook into static HTML file ----------------------------------------
irmass::render_notebook(notebook_file = notebook_file,
                        notebook_dir = notebook_dir,
                        reports_dir = reports_dir,
                        params_tibble = params_tibble,
                        force = TRUE)

